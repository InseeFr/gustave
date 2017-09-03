
#' Create a variance estimation wrapper

#' @export
#' @import Matrix

varwrap_factory <- function(
  varfun = NULL, objects_to_include = NULL
  , id_default = NULL, idref = NULL, wref = NULL
  , data_arg = "y", envir = parent.frame()
){

  # Step 1 : Creating the variance estimation wrapper
  varwrap <- function(data, ..., by = NULL, where = NULL, stat = "total", alpha = 0.05
    , display = TRUE, id = NULL, w = NULL, envir = parent.frame()
  ){

    # if(!requireNamespace("Matrix", quietly = TRUE)) stop("The Matrix package is required.")
    if(!suppressWarnings(require("Matrix", quietly = TRUE))) stop("The Matrix package is required.")

    evaluation_envir <- envir
    execution_envir <- environment()
    call <- match.call(expand.dots = TRUE)
    substitute_data <- substitute(data)
    eval_data <- eval(substitute_data, evaluation_envir)

    # Step 1 : Controlling identifiers and deriving the weights
    # to use for point estimates and linearizations
    idref <- eval(idref)
    wref <- eval(wref)
    if(!is.null(idref)){
      id <- eval(id, eval_data)
      id_match <- match(id, idref)
      if(anyNA(id_match))
        stop("Some values of the id variable \"", deparse(substitute(id)), "\" do not match with the values of the reference id variable.", call. = FALSE)
      w <- wref[id_match]
    }else{
      id_match <- 1:length(w)
      if(is.null((substitute(w)))) stop("A weight must be provided (using argument w). ")
      w <- eval(substitute(w), eval_data)
    }

    # Step 2 : Specifying default values for stat, by and where arguments
    l <- eval(substitute(alist(...)))
    l <- lapply(l, function(i){
      if(is.symbol(i) || !("gustave_linwrap"  %in% class(eval(i[[1]]))))
        i <- as.call(c(as.symbol(stat), i))
      i <- as.list(i)
      if(!("by" %in% names(i))) i$by <- substitute(by, execution_envir)
      if(!("where" %in% names(i))) i$where <- substitute(where, execution_envir)
      as.call(i)
    })

    # Step 3 : Calling the linearization wrappers
    labels <- if(!is.null(names(l))) names(l) else rep(NA, length(l))
    labels[labels %in% ""] <- NA
    d <- unlist(lapply(seq_along(l), function(i){
      linwrap_call <- as.call(c(as.list(l[[i]]), list(tech_args = list(
        data = substitute_data, w = quote(w), label = labels[i]
        , evaluation_envir = evaluation_envir, execution_envir = execution_envir
      ))))
      eval(linwrap_call, envir = execution_envir)
    }), recursive = FALSE)
    d <- list(
      preparation = lapply(d, `[[`, "preparation")
      , estimation = NULL
      , display = lapply(d, `[[`, "display")
    )

    # Step 4 : Building up the sparse matrix to be used in the estimation
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
      data@i <- as.integer(match(id, idref)[data@i + 1] - 1)
      data@Dim <- c(length(idref), NCOL(data))
      data@Dimnames <- list(as.character(idref), NULL)
      data
    }

    # Step 5 : Calling the variance estimation function
    # and reorganizing the results
    varfun_args <- c(
      stats::setNames(list(d$estimation$data), data_arg)
      # , as.list(call)[intersect(names(call), setdiff(names(formals(varfun)), data_arg))]
      # , as.list(environment())[setdiff(names(formals(varfun)), data_arg)]
      , lapply(setdiff(names(formals(varfun)), data_arg), get, envir = execution_envir)
    )
    d$estimation$result <- suppressMessages(do.call(varfun, varfun_args))
    d$estimation$varfun <- varfun
    k <- 0;
    d$display <- lapply(seq_along(d$display), function(i) c(d$display[[i]]
      , list(var = lapply(d$preparation[[i]]$lin, function(j){
        t <- d$estimation$result$var[(k + 1):(k + NCOL(j))]
        assign("k", (k + NCOL(j)), envir = execution_envir)
        return(t)
      }))
    ))

    # Step 6 : Displaying the results if requested (the default)
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

  # Step 2 : Modifying varwrap arguments depending on the context
  if(!is.null(idref)){
    formals(varwrap)$id <- substitute(id_default)
    formals(varwrap) <- formals(varwrap)[names(formals(varwrap)) != "w"]
  }else{
    formals(varwrap) <- formals(varwrap)[names(formals(varwrap)) != "id"]
  }
  formals(varwrap) <- c(formals(varwrap), formals(varfun)[setdiff(names(formals(varfun)), data_arg)])
  rm(id_default)

  # Step 3 : Including objects in varwrap
#   varwrap <- include_objects(varwrap, c(objects_to_include, c(
#     "idref", "wref", "data_arg", "varfun"
#     , "total", "ratio", "mean", "diffratio"
#     , "sumby", "Diagby", "add0"
#   )), from = envir, sanitize = TRUE)
#   varwrap <- include_objects(varwrap, c(
#     "idref", "wref", "data_arg", "varfun", ls(asNamespace("gustave"))
#     # , "total", "ratio", "mean", "diffratio", "sumby", "Diagby", "add0"
#   ), from = asNamespace("gustave"), sanitize = TRUE)
#   varwrap <- include_objects(varwrap, objects_to_include, from = envir, sanitize = TRUE)
#   varwrap <- include_objects(varwrap, ls(asNamespace("gustave")), from = asNamespace("gustave"))
#   parent_env <- environment(varwrap)
#   varwrap <- include_objects(
#     varwrap, c("idref", "wref", "data_arg", "varfun")
#     , from = environment(), parent_env = parent_env
#   )
#   varwrap <- include_objects(varwrap, objects_to_include, from = envir, parent_env = parent_env)
  #     parent_env <- new.env(parent = globalenv())
  #     for(n in ls(asNamespace("gustave")))
  #       assign(n, eval(parse(text = deparse(get(n, asNamespace("gustave")))), envir = to), envir = to)

  e1 <- new.env(parent = globalenv())
  assign_all(objects = ls(asNamespace("gustave")), to = e1, from = asNamespace("gustave"))
  e2 <- new.env(parent = e1)
  assign_all(objects = c("idref", "wref", "data_arg", "varfun"), to = e2, from = environment())
  assign_all(objects = objects_to_include, to = e2, from = envir)
  varwrap <- change_enclosing(varwrap, envir = e2)

  varwrap <- structure(varwrap, class = c("function", "gustave_varwrap"))

  return(varwrap)

}

# Adding the linearization wrappers to the varwrap_factory
# varwrap_factory <- include_objects(varwrap_factory, objects = c(
#   "total", "mean", "ratio", "diffratio"
# ))

