#' TODO
#' @export define_linearization_wrapper
#'
define_linearization_wrapper <- function(
  linearization_function, 
  arg_type = list(data = "y", weight = "weight", param = NULL), 
  arg_not_affected_by_domain = NULL, 
  allow_factor = FALSE, 
  display_function = standard_display_function
){
  
  # Step 0 : Control arguments consistency
  inconsistent_arg <- list(
    in_arg_type_not_in_linearization_function = setdiff(unlist(arg_type), names(formals(linearization_function)))
    , in_linearization_function_not_in_arg_type = setdiff(names(formals(linearization_function)), unlist(arg_type))
    , in_arg_not_affected_by_domain_not_in_linearization_function = setdiff(arg_not_affected_by_domain, names(formals(linearization_function)))
  )
  if(length(unlist(inconsistent_arg)) > 0) stop(
    "Some arguments are inconsistent:"
    , if(length(inconsistent_arg[[1]]) > 0) paste("\n  -", paste(inconsistent_arg[[1]], collapse = ", "), "in arg_type but not in linearization_function arguments") else ""
    , if(length(inconsistent_arg[[2]]) > 0) paste("\n  -", paste(inconsistent_arg[[2]], collapse = ", "), "in linearization_function arguments but not in arg_type") else ""
    , if(length(inconsistent_arg[[3]]) > 0) paste("\n  -", paste(inconsistent_arg[[3]], collapse = ", "), "in arg_not_affected_by_domain but not in linearization_function arguments") else ""
    , call. = FALSE
  )
  if(is.null(arg_type$weight))
    stop("A weight argument must be provided in order to create a linearization wrapper.")

  # Step 1 : Create the linearization wrapper
  linearization_wrapper <- function(by = NULL, where = NULL, technical_arg){
    
    # Step 1.1 : Capture and modify the call
    call <- match.call(expand.dots = TRUE)
    call_list <- as.list(call)[-1]
    call_list$technical_arg <- c(technical_arg, list(
      allow_factor = allow_factor, arg_type = arg_type
      , arg_not_affected_by_domain = arg_not_affected_by_domain
    ))
    
    # Step 1.2 : Proceeed to standard preparation
    preparation <- do.call(standard_preparation, call_list)
    if(is.null(preparation)) return(NULL)
    d <- lapply(preparation, function(i) list(
      preparation = i
      , display = list(metadata = list(standard = data.frame(
        label = technical_arg$label
        , call = paste(deparse(call[!(names(call) %in% c("technical_arg", arg_type$weight)) | sapply(call, is.null)], width.cutoff = 500L), collapse = "")
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
  formals(linearization_wrapper) <- c(formals(linearization_function), formals(linearization_wrapper))

  # Step 3 : Include aobjects in linearization_wrapper enclosing environment
  e <- new.env(parent = globalenv())
  assign_all(objects = "standard_preparation", to = e, from = asNamespace("gustave"))
  assign_all(objects = c("linearization_function", "arg_type", "allow_factor", "arg_not_affected_by_domain", "display_function"), to = e, from = environment())
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
    data = lapply(expr[names(expr) %in% technical_arg$arg_type$data], eval, envir = eval_data, enclos = technical_arg$evaluation_envir)
    , weight = lapply(expr[names(expr) %in% technical_arg$arg_type$weight], eval, envir = eval_data, enclos = technical_arg$evaluation_envir)
    , param = if(any(names(expr) %in% technical_arg$arg_type$param)) expr[names(expr) %in% technical_arg$arg_type$param] else NULL
  ))
  if(all(sapply(d[[1]]$data, is.null))) return(NULL)

  # Step 2 : Handling factors and character variables in data
  fac <- sapply(d[[1]]$data, function(i) is.character(i) || is.factor(i))
  if(sum(fac) > 0){
    if(technical_arg$allow_factor && length(d[[1]]$data) == 1){
      tmp <- d[[1]]$data[[1]]
      if(is.character(tmp)) tmp <- as.factor(tmp)
      tmp <- droplevels(tmp)
      levels <- levels(tmp)
      tmp2 <- stats::model.matrix(~ . -1, data = stats::model.frame(~ ., data = tmp))
      tmp3 <- matrix(NA, nrow = NROW(tmp), ncol = length(levels))
      tmp3[as.integer(rownames(tmp2)), ] <- tmp2
      d <- lapply(1:length(levels), function(i){
        list(
          data = stats::setNames(list(c(tmp3[, i])), names(d[[1]]$data))
          , weight = d[[1]]$weight, param = d[[1]]$param
          , metadata = list(mod = levels[i])
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
      data = lapply(stats::setNames(seq_along(j$data), names(j$data)), function(k){
        if(names(j$data)[k] %in% technical_arg$arg_not_affected_by_domain) j$data[[k]] else j$data[[k]][bypos[[i]]]
      })
      , weight = lapply(stats::setNames(seq_along(j$weight), names(j$weight)), function(k){
        if(names(j$weight)[k] %in% technical_arg$arg_not_affected_by_domain) j$weight[[k]] else j$weight[[k]][bypos[[i]]]
      }), param = j$param
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
  if(!is.null(i$metadata$n)) d$n <- i$metadata$n
  d$est <- i$metadata$est
  d$variance <- i$var[[1]]
  d$std <- sqrt(d$variance)
  d$cv <- d$std*100/d$est
  d$lower <- d$est - stats::qnorm(1-alpha/2)*d$std
  d$upper <- d$est + stats::qnorm(1-alpha/2)*d$std
  return(d)
}
standard_display_function <- change_enclosing(standard_display_function, globalenv())


