#' TODO
#' @export define_linearization_wrapper
#' @import Matrix
#'
define_linearization_wrapper <- function(
  linearization_function
  , weight_arg_name = "w"
  , allow_factor = FALSE
  , display_function = standard_display_function
){

  # Step 1 : Creating the linearization wrapper
  linearization_wrapper <- function(by = NULL, where = NULL, tech_args){

    call <- match.call(expand.dots = TRUE)

    # Step 1.1 : Standard preparation
    standard <- do.call(standard_preparation, c(as.list(call)[-1], allow_factor = allow_factor))

    # Step 1.2 : Building up a structured object
    eval_w <- eval(tech_args$w, tech_args$execution_envir)
    d <- lapply(standard, function(i) list(
      preparation = list(
        data = c(i$data, stats::setNames(list(eval_w[i$metadata$bypos]), weight_arg_name))
        , metadata = i$metadata["bypos"]
      )
      , display = list(metadata = list(standard = data.frame(
        label = tech_args$label
        , call = paste(deparse(call[names(call) != "tech_args" | sapply(call, is.null)], width.cutoff = 500L), collapse = "")
        , by = i$metadata$by
        , mod = i$metadata$mod
        , stringsAsFactors = FALSE
      )))
    ))

    # Step 1.3 : Evaluating the linearization functions
    linearization_function_args <- stats::setNames(
      sapply(names(formals(linearization_function)), as.symbol)
      , names(formals(linearization_function))
    )
    d <- lapply(d, function(i){
      t <- with(i$preparation$data, do.call(linearization_function, linearization_function_args))
      list(
        preparation = c(i$preparation, list(linearization_function = linearization_function, lin  = t$lin))
        , display = list(metadata = c(i$display$metadata, t$metadata), fun = display_function)
      )
    })

    return(d)
  }

  # Step 2 : Modifying linearization_wrapper formals
  if(is.null(weight_arg_name) || !(weight_arg_name %in% names(formals(linearization_function))))
    stop("A weight argument must be specified in order to create a linearization wrapper.")
  formals(linearization_wrapper) <- c(formals(linearization_function)[setdiff(names(formals(linearization_function)), c(weight_arg_name, names(formals(linearization_wrapper))))], formals(linearization_wrapper))

  # Step 3 : Including additional objects
  e <- new.env(parent = globalenv())
  assign_all(objects = c("standard_preparation"), to = e, from = asNamespace("gustave"))
  assign_all(objects = c("linearization_function", "weight_arg_name", "allow_factor", "display_function"), to = e, from = environment())
  linearization_wrapper <- change_enclosing(linearization_wrapper, envir = e)
  linearization_wrapper <- structure(linearization_wrapper, class = c("function", "gustave_linearization_wrapper"))

  return(linearization_wrapper)
}

# standard_preparation() function
standard_preparation <- function(..., by = NULL, where = NULL, tech_args, allow_factor){

  # Step 1 : Evaluation
  eval_data <- eval(tech_args$data, tech_args$evaluation_envir)
  d <- list(list(data = lapply(eval(substitute(alist(...))), eval, eval_data)))
  n <- length(d[[1]]$data[[1]])

  # Step 2 : Handling factors and character variables
  fac <- sapply(d[[1]]$data, function(i) is.character(i) || is.factor(i))
  if(sum(fac) > 0){
    if(allow_factor && length(d[[1]]$data) == 1){
      t <- t(Matrix::fac2sparse(d[[1]]$data[[1]], giveCsparse = FALSE))
      # TODO: get rid of Matrix dependency at this point
      if(any(na <- is.na(d[[1]]$data[[1]]))) t[na, , drop = FALSE] <- NA
      d <- lapply(1:NCOL(t), function(i){
        list(
          data = stats::setNames(list(c(t[, i])), names(d[[1]]$data))
          , metadata = list(mod = colnames(t)[i])
        )
      })
    }else stop("Character or factor variables aren't allowed.", call. = FALSE)
  }else d[[1]]$metadata$mod <- NA

  # Step 3 : by and where arguments preparation
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


