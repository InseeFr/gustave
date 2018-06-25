
#' Define a linearization wrapper

#' @description Given a linearization \emph{function} (specific to an
#'   estimator), \code{define_linearization_wrapper} defines a 
#'   linearization \emph{wrapper} to be used together with
#'   \code{\link[=define_variance_wrapper]{variance estimation wrappers}}
#'   in order to make variance estimation easier. 
#'   This function is intended for \strong{advanced use only} (see Details), 
#'   standard linearization wrappers are included in the gustave package (see
#'   \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}})
#'   
#' @param linearization_function An R function with input the quantities 
#'   used in the linearization formula and with output a list with two 
#'   named element: \itemize{\item \code{lin}: a list of numerical vectors (most
#'   of the time, only 1) which correspond to the value of the linearized 
#'   variable \item \code{metadata}: a list of metadata to be used by the display
#'   function (see \code{display_function} argument), including (for the
#'   standard display function) \code{est} for the point-estimate and 
#'   \code{n} for the number of observations used in the estimation.}
#' @param arg_type A named list with three character vectors describing 
#'   the type of each argument of \code{linearization_function}: \itemize{
#'   \item \code{data}: data argument(s), numerical vector(s) to be used in the
#'   linearization formula \item \code{weight}: weight argument, numerical vector
#'   to be used as row weights in the linearization formula \item \code{param}: 
#'   parameters, non-data arguments (most of the time boolean) to be used to 
#'   control some aspect of the linearization formula}
#' @param arg_not_affected_by_domain A character vector indicating the (data) 
#'   arguments which should not be affected by domain-splitting. Such parameters
#'   may appear in some complex linearization formula, for instance when the 
#'   At-Risk of Poverty Rate (ARPR) is estimated by region but with a poverty 
#'   line calculated at the national level.
#' @param  display_function An R function which produces, for each variance 
#'   estimation, the data.frame row to be displayed by the variance estimation 
#'   wrapper. It uses three arguments: 
#'   \itemize{\item \code{var} the estimated variance \item \code{metadata} the 
#'   metadata associated with the estimation, especially the one outputted by 
#'   \code{linearization_function} (e.g. \code{est}, \code{n}) \item \code{alpha} 
#'   the level for the construction of confidence intervals (at execution time, 
#'   its value is taken from the \code{alpha} argument of the variance wrapper.)}
#'   The default display function (\code{standard_display}) uses standard metadata
#'   to display usual variance indicator (variance, standard deviation, coefficient
#'   of variation, confidence interval) broken down by linearization wrapper, domain 
#'   (if any) and level (if the variable is a factor).
#'   
#' @details When the estimator is not the estimator of a total, the application of 
#'   analytical variance estimation formulae developed for the estimator of a total 
#'   is not straightforward (Deville, 1999). An asymptotically unbiased variance 
#'   estimator can nonetheless be obtained if the estimation of variance is performed
#'   on a variable obtained from the original data through a linearization step. 
#'   
#'   \code{define_linearization_wrapper} is the function used to create, given 
#'   a linearization \emph{function} implementing a given linearization 
#'   \emph{formula}, a linearization \emph{wrapper} which can be used together 
#'   with a variance wrapper. 
#'   
#'   Linearization wrappers are quite flexible tools
#'   to apply a variance function to an estimator requiring a linearization step
#'   (e.g. all estimators except the estimator of a total) with virtually no 
#'   additional complexity for the end-user. To some extent, linearization wrappers 
#'   can be seen as ggplot2 \code{geom_} and \code{stat_} functions: they help 
#'   the end-user in writing down what he or she wants without having to go too 
#'   deep into the details of the corresponding layers. 
#'   
#'   \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}} 
#'   are included within the gustave package and  automatically added 
#'   to the variance estimation wrappers. New linearization wrappers can be defined
#'   using the \code{define_linearization_wrapper} and then explicitly added to the 
#'   variance estimation wrappers using the \code{objects_to_include} argument.
#'   
#' @return A function to be used within a variance estimation wrapper to perform
#'   a specific linearization (see examples). Its formals are the ones of 
#'   \code{linearization_function} with the addition of \code{by} and \code{where} 
#'   (for domain estimation, set to \code{NULL} by default). 
#' 
#' @author Martin Chevalier
#'    
#' @seealso \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}} 
#'   \code{\link{define_variance_wrapper}}
#'   
#' @references 
#'   Deville J.-C. (1999), "Variance estimation for complex statistics and 
#'   estimators: linearization and residual techniques", \emph{Survey Methodology}, 
#'   25:193–203
#'   
#' @examples ### Example from the Information and communication technologies (ICT) survey
#' 
#' # The subset of the (simulated) ICT survey has the following features: 
#' # - stratified one-stage sampling design of 650 firms;
#' # - 612 responding firms, non-response correction through reweighting 
#' # in homogeneous response groups based on economic sub-sector and turnover;
#' # - calibration on margins (number of firms and turnover broken down
#' # by economic sub-sector).
#' 
#' # Step 1 : Dummy variance wrapper
#' # Note : see define_variance_wrapper() for a more 
#' # realistic variance function and examples.
#' variance_wrapper <- define_variance_wrapper(
#'   variance_function = function(y) abs(colSums(y)), 
#'   reference_id = ict_survey$firm_id, 
#'   reference_weight = ict_survey$w_calib, 
#'   default_id = "firm_id"
#' )
#' variance_wrapper(ict_survey, total(speed_quanti))
#' 
#' # Step 2 : Redefine the mean linearization wrapper
#' # The mean() linearization wrapper defined in the gustave 
#' # package is bulit on top of the ratio() linearization wrapper.
#' variance_wrapper(ict_survey, mean(speed_quanti))
#' 
#' # Let's redefine it directly from the formula found for instance
#' # in (Caron, Deville, Sautory, 1998) and without handling NA
#' # values
#' mean2 <- define_linearization_wrapper(
#'   linearization_function = function(y, weight){
#'     est <- sum(y * weight) / sum(weight)
#'     lin <- (y - est) / sum(weight)
#'     list(
#'       lin = list(lin), 
#'       metadata = list(est = est, n = length(y))
#'     )
#'   },
#'   arg_type = list(data = "y", weight = "weight")
#' )
#' variance_wrapper(ict_survey, mean(speed_quanti), mean2(speed_quanti))
#' 
#' @export

define_linearization_wrapper <- function(linearization_function, 
                                         arg_type, 
                                         arg_not_affected_by_domain = NULL, 
                                         display_function = standard_display
){
  
  # Step 1: Control arguments consistency
  inconsistent_arg <- list(
    in_arg_type_not_in_linearization_function = setdiff(unlist(arg_type), names(formals(linearization_function))), 
    in_linearization_function_not_in_arg_type = setdiff(names(formals(linearization_function)), unlist(arg_type)), 
    in_arg_not_affected_by_domain_not_in_linearization_function = setdiff(arg_not_affected_by_domain, names(formals(linearization_function)))
  )
  if(length(unlist(inconsistent_arg)) > 0) stop(
    "Some arguments are inconsistent:", 
    if(length(inconsistent_arg[[1]]) > 0) paste("\n  -", paste(inconsistent_arg[[1]], collapse = ", "), "in arg_type but not in linearization_function arguments") else "", 
    if(length(inconsistent_arg[[2]]) > 0) paste("\n  -", paste(inconsistent_arg[[2]], collapse = ", "), "in linearization_function arguments but not in arg_type") else "", 
    if(length(inconsistent_arg[[3]]) > 0) paste("\n  -", paste(inconsistent_arg[[3]], collapse = ", "), "in arg_not_affected_by_domain but not in linearization_function arguments") else ""
  )
  if(is.null(arg_type$weight))
    stop("A weight argument must be provided in order to create a linearization wrapper.")
  
  # Step 2: Create the linearization wrapper
  linearization_wrapper <- function(by = NULL, where = NULL, ...){
    
    # Step 1: Capture and expand the call
    call <- match.call(expand.dots = TRUE)
    call_display_arg <- c(1, which(names(call) %in% setdiff(names(formals(sys.function())), "...") & !sapply(call, is.null)))
    call_display <- paste(deparse(call[call_display_arg], width.cutoff = 500L), collapse = "")
    call_list <- c(as.list(call)[-1], list(
      arg_type = arg_type, 
      arg_not_affected_by_domain = arg_not_affected_by_domain,
      call = call_display
    ))
    
    # Step 2: Proceeed to standard preparation
    d <- do.call(standard_preparation, call_list)
    if(is.null(d)) return(NULL)

    # Step 3: Evaluate the linearization functions
    d <- lapply(d, function(i){
      tmp <- do.call(linearization_function, with(i, c(data, weight, param)))
      i$metadata <- c(i$metadata, tmp$metadata)
      c(i, list(
        linearization_function = linearization_function, 
        lin  = tmp$lin,
        display_function = display_function
      ))
    })
    
    return(d)
  }

  # Step 3: Modify linearization_wrapper formals
  formals(linearization_wrapper) <- c(
    formals(linearization_function)[setdiff(names(formals(linearization_function)), arg_type$weight)], 
    formals(linearization_wrapper)
  )

  # Step 4: Include objects in linearization_wrapper enclosing environment
  e <- new.env(parent = globalenv())
  assign_all(objects = "standard_preparation", to = e, from = asNamespace("gustave"))
  assign_all(objects = c("linearization_function", "arg_type", "arg_not_affected_by_domain", "display_function"), to = e, from = environment())
  linearization_wrapper <- change_enclosing(linearization_wrapper, envir = e)
  
  structure(linearization_wrapper, class = c("function", "gustave_linearization_wrapper"))
  
}

standard_preparation <- function(..., 
                                 by = NULL, where = NULL, 
                                 data, label, evaluation_envir, execution_envir, 
                                 arg_type, arg_not_affected_by_domain,
                                 call
  ){

  # Step 0: Allow factors only if there is only 1 data argument
  allow_factor <- length(arg_type$data) == 1
  
  # Step 1 : Evaluation
  eval_data <- eval(data, evaluation_envir)
  expr <- eval(substitute(alist(...)))
  d <- list(list(
    data = lapply(expr[names(expr) %in% arg_type$data], eval, envir = eval_data, enclos = evaluation_envir)
    , weight = lapply(expr[names(expr) %in% arg_type$weight], eval, envir = execution_envir)
    , param = if(any(names(expr) %in% arg_type$param)) expr[names(expr) %in% arg_type$param] else NULL
  ))
  if(all(sapply(d[[1]]$data, is.null))) return(NULL)

  # Step 2 : Handling factors and character variables in data
  fac <- sapply(d[[1]]$data, function(i) is.character(i) || is.factor(i))
  if(sum(fac) > 0){
    if(allow_factor && length(d[[1]]$data) == 1){
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
    tmp <- rep(1L, n)
    levels(tmp) <- "1"
    class(tmp) <- "factor"
    tmp
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
        if(names(j$data)[k] %in% arg_not_affected_by_domain) j$data[[k]] else j$data[[k]][bypos[[i]]]
      })
      , weight = lapply(stats::setNames(seq_along(j$weight), names(j$weight)), function(k){
        if(names(j$weight)[k] %in% arg_not_affected_by_domain) j$weight[[k]] else j$weight[[k]][bypos[[i]]]
      }), param = j$param
      , metadata = c(j$metadata, list(
        by = if(byNULL) NA else names(bypos)[i], 
        bypos = bypos[[i]],
        label = label,
        call = call
      ))))
  }), recursive = FALSE)
  
  d

}

standard_display <- function(var, metadata, alpha){
  d <- as.data.frame(metadata[c("label", "call", "mod", "by")])
  if(!is.null(metadata$n)) d$n <- metadata$n
  d$est <- metadata$est
  d$variance <- var[[1]]
  d$std <- sqrt(d$variance)
  d$cv <- d$std * 100 / d$est
  d$lower <- d$est - stats::qnorm(1-alpha/2)*d$std
  d$upper <- d$est + stats::qnorm(1-alpha/2)*d$std
  d
}
standard_display <- change_enclosing(standard_display, globalenv())


