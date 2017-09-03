
#' Create a variance estimation wrapper

#' @export define_variance_wrapper
#' @import Matrix

define_variance_wrapper <- function(
  variance_function = NULL, data_arg_name = "y", objects_to_include = NULL
  , default_id = NULL, reference_id = NULL, reference_weight = NULL
  , default_stat = "total", default_alpha = 0.05, envir = parent.frame()
){

  # Step 1 : Creating the variance estimation wrapper
  variance_wrapper <- function(
    data, ..., by = NULL, where = NULL, stat = NULL, alpha = NULL
    , display = TRUE, id = NULL, w = NULL, envir = parent.frame()
  ){

    evaluation_envir <- envir
    execution_envir <- environment()
    call <- match.call(expand.dots = TRUE)
    substitute_data <- substitute(data)
    eval_data <- eval(substitute_data, evaluation_envir)

    # Step 1.1 : Controlling identifiers and deriving the weights
    # to use for point estimates and linearizations
    reference_id <- eval(reference_id)
    reference_weight <- eval(reference_weight)
    if(!is.null(reference_id)){
      id <- eval(id, eval_data)
      id_match <- match(id, reference_id)
      if(anyNA(id_match))
        stop("Some values of the id variable \"", deparse(substitute(id)), "\" do not match with the values of the reference id variable.", call. = FALSE)
      w <- reference_weight[id_match]
    }else{
      id_match <- 1:length(w)
      if(is.null((substitute(w)))) stop("A weight must be provided (using argument w). ")
      w <- eval(substitute(w), eval_data)
    }

    # Step 1.2 : Specifying default values for stat, by and where arguments
    l <- eval(substitute(alist(...)))
    l <- lapply(l, function(i){
      if(is.symbol(i) || !("gustave_linearization_wrapper" %in% class(eval(i[[1]]))))
        i <- as.call(c(as.symbol(stat), i))
      i <- as.list(i)
      if(!("by" %in% names(i))) i$by <- substitute(by, execution_envir)
      if(!("where" %in% names(i))) i$where <- substitute(where, execution_envir)
      as.call(i)
    })

    # Step 1.3 : Calling the linearization wrappers
    labels <- if(!is.null(names(l))) names(l) else rep(NA, length(l))
    labels[labels %in% ""] <- NA
    d <- unlist(lapply(seq_along(l), function(i){
      linearization_wrapper_call <- as.call(c(as.list(l[[i]]), list(tech_args = list(
        data = substitute_data, w = quote(w), label = labels[i]
        , evaluation_envir = evaluation_envir, execution_envir = execution_envir
      ))))
      eval(linearization_wrapper_call, envir = execution_envir)
    }), recursive = FALSE)
    d <- list(
      preparation = lapply(d, `[[`, "preparation")
      , estimation = NULL
      , display = lapply(d, `[[`, "display")
    )

    # Step 1.4 : Building up the sparse matrix to be used in the estimation
    d$estimation$data <- {
      data <- lapply(d$preparation, function(k){
        t <- do.call(cbind, k$lin)
        Matrix::sparseMatrix(
          i = rep(k$metadata$bypos, NCOL(t))
          , j = rep(1:NCOL(t), each = NROW(t)), giveCsparse = FALSE
          , x = c(t), dims = c(length(id), NCOL(t)), check = FALSE
        )
      })
      data <- methods::as(Matrix::drop0(do.call(cbind, data)), "TsparseMatrix")
      data@i <- as.integer(match(id, reference_id)[data@i + 1] - 1)
      data@Dim <- c(length(reference_id), NCOL(data))
      data@Dimnames <- list(as.character(reference_id), NULL)
      data
    }

    # Step 1.5 : Calling the variance estimation function
    # and reorganizing the results
    variance_function_args <- c(
      stats::setNames(list(d$estimation$data), data_arg_name)
      , lapply(setdiff(names(formals(variance_function)), data_arg_name), get, envir = execution_envir)
    )
    d$estimation$result <- suppressMessages(do.call(variance_function, variance_function_args))
    d$estimation$variance_function <- variance_function
    k <- 0;
    d$display <- lapply(seq_along(d$display), function(i) c(d$display[[i]]
      , list(var = lapply(d$preparation[[i]]$lin, function(j){
        t <- d$estimation$result$var[(k + 1):(k + NCOL(j))]
        assign("k", (k + NCOL(j)), envir = execution_envir)
        return(t)
      }))
    ))

    # Step 1.6 : Displaying the results if requested (the default)
    if(display){
      d <- lapply(d$display, function(i) i$fun(i, alpha = alpha))
      names <- unique(do.call(base::c, lapply(d, names)))
      d <- do.call(rbind, lapply(d, function(i){
        i[, setdiff(names, names(i))] <- NA
        i[, names]
      }))
      d <- d[, sapply(d, function(i) !all(is.na(i)))]
      rownames(d) <- NULL
      return(d)
    }else return(invisible(d))

  }

  # Step 2 : Modifying variance_wrapper arguments depending on the context
  formals(variance_wrapper)$stat <- default_stat
  formals(variance_wrapper)$alpha <- default_alpha
  if(!is.null(reference_id)){
    formals(variance_wrapper)$id <- substitute(default_id)
    formals(variance_wrapper) <- formals(variance_wrapper)[names(formals(variance_wrapper)) != "w"]
  }else{
    formals(variance_wrapper) <- formals(variance_wrapper)[names(formals(variance_wrapper)) != "id"]
  }
  formals(variance_wrapper) <- c(formals(variance_wrapper), formals(variance_function)[setdiff(names(formals(variance_function)), data_arg_name)])
  rm(default_id, default_stat, default_alpha)

  # Step 3 : Including objects in variance_wrapper
  e1 <- new.env(parent = globalenv())
  assign_all(objects = ls(asNamespace("gustave")), to = e1, from = asNamespace("gustave"))
  e2 <- new.env(parent = e1)
  assign_all(objects = c("reference_id", "reference_weight", "data_arg_name", "variance_function"), to = e2, from = environment())
  assign_all(objects = objects_to_include, to = e2, from = envir)
  variance_wrapper <- change_enclosing(variance_wrapper, envir = e2)

  variance_wrapper <- structure(variance_wrapper, class = c("function", "gustave_variance_wrapper"))

  return(variance_wrapper)

}

