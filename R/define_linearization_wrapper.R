#' TODO
#' @export define_linearization_wrapper
#' @import Matrix
#'
define_linearization_wrapper <- function(
  linearization_function
  , arg_type = list(data = "y", weight = "w", param = NULL)
  , allow_factor = FALSE
  , display_function = standard_display_function
){
  
  # Step 0 : Control arguments type
  in_arg_type_not_in_variance_function <- setdiff(unlist(arg_type), names(formals(linearization_function)))
  in_variance_function_not_in_arg_type <- setdiff(names(formals(linearization_function)), unlist(arg_type))
  if(length(in_arg_type_not_in_variance_function) > 0 | length(in_variance_function_not_in_arg_type) > 0) stop(
    "variance_function arguments and arg_type value are not consistent:"
    , if(length(in_arg_type_not_in_variance_function) > 0) paste("\n  -", paste(in_arg_type_not_in_variance_function, collapse = ", "), "in arg_type but not in variance_function arguments") else ""
    , if(length(in_variance_function_not_in_arg_type) > 0) paste("\n  -", paste(in_variance_function_not_in_arg_type, collapse = ", "), "in variance_function arguments but not in arg_type") else ""
    , call. = FALSE
  )
  if(is.null(arg_type$weight))
    stop("A weight argument must be provided in order to create a linearization wrapper.")
  
  # Step 1 : Creating the linearization wrapper
  linearization_wrapper <- function(by = NULL, where = NULL, technical_arg){

    # Step 1.1 : Capture and modify the call
    call <- match.call(expand.dots = TRUE)
    call_list <- as.list(call)[-1]
    call_list$technical_arg <- c(technical_arg, list(allow_factor = allow_factor, arg_type = arg_type))
    
    # Step 1.2 : Proceeed to standard preparation
    preparation <- do.call(standard_preparation, call_list)
    d <- lapply(preparation, function(i) list(
      preparation = i
      , display = list(metadata = list(standard = data.frame(
        label = technical_arg$label
        , call = paste(deparse(call[names(call) != "technical_arg" | sapply(call, is.null)], width.cutoff = 500L), collapse = "")
        , mod = i$metadata$mod, by = i$metadata$by
        , stringsAsFactors = FALSE
      )))
    ))
    # TODO: directly produce the display part within standard_preparation() (save for call)
    
    # Step 1.3 : Evaluate the linearization functions
    d <- lapply(d, function(i){
      t <- do.call(linearization_function, with(i$preparation, c(data, weight, param)))
      list(
        preparation = c(i$preparation, list(linearization_function = linearization_function, lin  = t$lin))
        , display = list(metadata = c(i$display$metadata, t$metadata), fun = display_function)
      )
    })

    return(d)
  }

  # Step 2 : Modify linearization_wrapper formals
  formals(linearization_wrapper) <- c(
    formals(linearization_function)[setdiff(names(formals(linearization_function)), arg_type$weight)]
    , formals(linearization_wrapper)
  )

  # Step 3 : Include additional objects
  e <- new.env(parent = globalenv())
  assign_all(objects = "standard_preparation", to = e, from = asNamespace("gustave"))
  assign_all(objects = c("linearization_function", "arg_type", "allow_factor", "display_function"), to = e, from = environment())
  linearization_wrapper <- change_enclosing(linearization_wrapper, envir = e)
  linearization_wrapper <- structure(linearization_wrapper, class = c("function", "gustave_linearization_wrapper"))

  return(linearization_wrapper)
}

# standard_preparation() function
standard_preparation <- function(..., by = NULL, where = NULL, technical_arg){

  # Step 1 : Evaluation
  eval_data <- eval(technical_arg$data, technical_arg$evaluation_envir)
  expr <- eval(substitute(alist(...)))
  d <- list(list(
    data = lapply(expr[names(expr) %in% technical_arg$arg_type$data], eval, eval_data)
    , weight = stats::setNames(
      rep(list(eval(technical_arg$w, technical_arg$execution_envir)), length(technical_arg$arg_type$weight))
      , technical_arg$arg_type$weight
    )
    , param = if(any(names(expr) %in% technical_arg$arg_type$param)) expr[names(expr) %in% technical_arg$arg_type$param] else NULL
  ))
  
  # Step 2 : Handling factors and character variables in data
  fac <- sapply(d[[1]]$data, function(i) is.character(i) || is.factor(i))
  if(sum(fac) > 0){
    if(technical_arg$allow_factor && length(d[[1]]$data) == 1){
      t <- Matrix::t(Matrix::fac2sparse(d[[1]]$data[[1]], giveCsparse = FALSE))
      # TODO: get rid of Matrix dependency at this point
      if(any(na <- is.na(d[[1]]$data[[1]]))) t[na, , drop = FALSE] <- NA
      d <- lapply(1:NCOL(t), function(i){
        list(
          data = stats::setNames(list(c(t[, i])), names(d[[1]]$data))
          , weight = d[[1]]$weight, param = d[[1]]$param
          , metadata = list(mod = colnames(t)[i])
        )
      })
    }else stop("Character or factor variables aren't allowed.", call. = FALSE)
  }else d[[1]]$metadata$mod <- NA
  
  # Step 3 : by and where arguments preparation
  n <- length(d[[1]]$data[[1]])
  byNULL <- is.null(substitute(by))
  by <- if(!byNULL){
    as.factor(eval(substitute(by), eval_data))
  }else{
    t <- rep(1L, n); levels(t) <- "1"; class(t) <- "factor"; t
  }
  if(!is.null(substitute(where))){
    by[!as.logical(eval(substitute(where), eval_data))] <- NA
    by <- droplevels(by)
  }
  if(sum(!is.na(by)) == 0)
    stop("by and/or where arguments exclude all observations.", call. = FALSE)

  # Step 4 : Splitting across domains
  bypos <- split(1:n, by, drop = TRUE)
  d <- unlist(lapply(seq_along(bypos), function(i){
    lapply(d, function(j) list(
      data = lapply(j$data, `[`, bypos[[i]])
      , weight = lapply(j$weight, `[`, bypos[[i]])
      , param = j$param
      , metadata = c(j$metadata, list(
        by = if(byNULL) NA else names(bypos)[i]
        , bypos = bypos[[i]]
      ))))
  }), recursive = FALSE)
  
  return(d)

}

# standard_display_function()
standard_display_function <- function(i, alpha){
  d <- i$metadata$standard
  d$est <- i$metadata$est
  d$variance <- i$var[[1]]
  d$std <- sqrt(d$variance)
  d$cv <- d$std*100/d$est
  d$lower <- d$est - stats::qnorm(1-alpha/2)*d$std
  d$upper <- d$est + stats::qnorm(1-alpha/2)*d$std
  return(d)
}


