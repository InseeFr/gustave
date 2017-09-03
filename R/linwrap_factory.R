

#' TODO
#' @export

linwrap_factory <- function(
  linfun, weight_arg = "w", allowfactors = FALSE, displayfun = display_default
){

  # Step 1 : Creating the linearization wrapper
  linwrap <- function(by = NULL, where = NULL, tech_args){

    call <- match.call(expand.dots = TRUE)

    # Step 1 : Standard preparation
    standard <- do.call(lin_standard, c(as.list(call)[-1], allowfactors = allowfactors))

    # Step 2 : Building up a structured object
    eval_w <- eval(tech_args$w, tech_args$execution_envir)
	  d <- lapply(standard, function(i) list(
	    preparation = list(
	      data = c(i$data, stats::setNames(list(eval_w[i$metadata$bypos]), weight_arg))
	      , metadata = i$metadata["bypos"]
	    )
	    , display = list(metadata = list(standard = data.frame(label = tech_args$label
        , call = paste(deparse(
          call[names(call) != "tech_args" | sapply(call, is.null)]
          , width.cutoff = 500L
        ), collapse = "")
	      , by = i$metadata$by, mod = i$metadata$mod, stringsAsFactors = FALSE
	    )))
	  ))

	  # Step 3 : Evaluating the linearization functions
	  linfun_args <- stats::setNames(sapply(names(formals(linfun)), as.symbol), names(formals(linfun)))
	  d <- lapply(d, function(i){
	    t <- with(i$preparation$data, do.call(linfun, linfun_args))
	    list(
	      preparation = c(i$preparation, list(linfun = linfun, lin  = t$lin))
	      , display = list(metadata = c(i$display$metadata, t$metadata), fun = displayfun)
	    )
	  })

	  return(d)
  }

  # Creating the lin_standard() function
  lin_standard <- function(..., by = NULL, where = NULL, tech_args, allowfactors){

    # Step 1 : Evaluation
    eval_data <- eval(tech_args$data, tech_args$evaluation_envir)
    d <- list(list(data = lapply(eval(substitute(alist(...))), eval, eval_data)))
    n <- length(d[[1]]$data[[1]])

    # Step 2 : Handling factors and character variables
    fac <- sapply(d[[1]]$data, function(i) is.character(i) || is.factor(i))
    if(sum(fac) > 0){
      if(allowfactors && length(d[[1]]$data) == 1){
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

    # Step 4 : Splitting accross domains
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

  # Creating the display_default() function
  display_default <- function(i, alpha){
    d <- i$metadata$standard
    d$est <- i$metadata$est
    d$variance <- i$var[[1]]
    d$std <- sqrt(d$variance)
    d$cv <- d$std*100/d$est
    d$lower <- d$est - stats::qnorm(1-alpha/2)*d$std
    d$upper <- d$est + stats::qnorm(1-alpha/2)*d$std
    return(d)
  }

  # Step 2 : Modifying linwrap formals
  if(is.null(weight_arg) || !(weight_arg %in% names(formals(linfun))))
     stop("A weight argument must be specified in order to create a linearization wrapper.")
  formals(linwrap) <- c(formals(linfun)[setdiff(names(formals(linfun)), c(weight_arg, names(formals(linwrap))))], formals(linwrap))

  # Step 3 : Including additional objects
  e <- new.env(parent = globalenv())
  assign_all(objects = c("linfun", "weight_arg", "allowfactors", "lin_standard", "displayfun"), to = e, from = environment())
  linwrap <- change_enclosing(linwrap, envir = e)
  # linwrap <- include_objects(linwrap, objects = c("linfun", "weight_arg", "allowfactors", "lin_standard", "displayfun"), from = parent.frame())
  linwrap <- structure(linwrap, class = c("function", "gustave_linwrap"))

  return(linwrap)
}
