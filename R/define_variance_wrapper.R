
#' Define a variance estimation wrapper

#' @description Given a variance estimation \emph{function} (specific to a 
#'   survey), \code{define_variance_wrapper} defines a variance estimation 
#'   \emph{wrapper} easier to use (e.g. automatic domain estimation, 
#'   linearization).
#'   
#' @param variance_function An R function. It is the methodological workhorse of 
#'   the estimation of variance: from a set of arguments including the variables 
#'   of interest (see below), it should return a vector of estimated variances 
#'   (or a list whose first element is a vector of estimated variances).
#'   Its arguments fall into three types: \itemize{
#'   \item the data argument (mandatory, only one allowed): the numerical matrix of 
#'   variables of interest to apply the variance estimation formula on
#'   \item technical data arguments (optional, one or more allowed): technical 
#'   and methodological information used by the variance estimation function
#'   (e.g. sampling strata, first- or second-order probabilities of inclusion, 
#'   estimated response probabilities, calibration variables)
#'   \item parameters (optional, one or more allowed): non-data arguments to be used 
#'   to control some aspect of the variance estimation (e.g. alternative methodology)
#'   }
#'   \code{define_variance_wrapper} tries to determine the type of each argument
#'   of \code{variance_function}. This behaviour can be overriden using the
#'   \code{arg_type} argument (see below).
#' @param reference_id A vector containing the ids of all the responding units
#'   of the survey. It is compared with \code{default$id} (see below) to check
#'   whether some observations are missing in the survey file. The matrix of variables
#'   of interest is reordered according to \code{reference_id} before being processed
#'   by \code{variance_function}.
#' @param default A named list specifying the default values for: \itemize{
#'   \item \code{id}: the name of the default identifying variable in the survey 
#'   file. It can also be an unevaluated expression (enclosed in \code{substitute()}) to be 
#'   evaluated within the survey file.
#'   \item \code{weight}: the name of the default weight variable in the survey file. 
#'   It can also be an unevaluated expression (enclosed in \code{substitute()}) to be 
#'   evaluated within the survey file.
#'   \item \code{stat}: the name of the default statistic to compute when none is specified. 
#'   It is set to \code{"total"} by default.
#'   \item \code{alpha}: the default threshold for confidence interval derivation. 
#'   It is set to \code{0.05} by default.
#' }
#' @param technical_data A named list of all technical data needed to perform 
#'   the variance estimation (e.g. sampling strata, first- or second-order 
#'   probabilities of inclusion, estimated response probabilities, calibration 
#'   variables). Its names should match the names of the arguments 
#'   of \code{variance_function}.
#' @param arg_type (Advanced use) A named list with three character vectors 
#'   describing the type of each argument of \code{variance_function} (see
#'   above). In most cases it should not be necessary to explicitly set its 
#'   value as it is automatically determined based on the following rules: 
#'   \itemize{
#'   \item the first argument of \code{variance_function} is considered 
#'   as the data argument
#'   \item all arguments of \code{variance_function} matching elements of 
#'   \code{technical_data} are considered as technical data arguments
#'   \item all remaining arguments of \code{variance_function} are considered 
#'   as parameters
#'   }
#'   
#' @param objects_to_include (Advanced use) A character vector indicating the name of 
#'   additional R objects to include within the variance wrapper.

#' 
#'   
#' @details Defining variance estimation wrappers is the \strong{key feature} of
#'   the \code{gustave} package.
#'   
#'   Analytical variance estimation is often difficult to carry out by 
#'   non-specialists owing to the complexity of the underlying sampling 
#'   and estimation methodology. This complexity yields complex \emph{variance estimation 
#'   functions} which are most often only used by the sampling expert who 
#'   actually wrote them. A \emph{variance estimation wrapper} is an 
#'   intermediate function that is "wrapped around" the (complex) variance 
#'   estimation function in order to provide the non-specialist with 
#'   user-friendly features: \itemize{ \item checks for consistency between the 
#'   provided dataset and the survey characteristics \item factor discretization
#'   \item domain estimation \item linearization of complex statistics (see 
#'   \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}})}
#'   
#'   \code{define_variance_wrapper} allows the sampling expert to define a 
#'   variance estimation wrapper around a given variance estimation function and
#'   set its default parameters. The produced variance estimation wrapper will 
#'   be stand-alone in the sense that it can contain all technical data necessary
#'   to carry out the estimation (see \code{technical_data}).
#'   
#' @return An R function that makes the estimation of variance based on the provided 
#' variance function easier. Its parameters are:
#'   \itemize{
#'    \item \code{data}: the survey data where the interest variables are stored
#'    \item \code{...}: one or more calls to a linearization wrapper (see examples
#'    and \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}})
#'    \item \code{where}: a logical vector indicating a domain on which the variance
#'    estimation is conducted
#'    \item \code{by}: a qualitative variable whose levels are used to define domains
#'    on which the variance estimation is conducted
#'    \item \code{stat}: a character vector of size 1 indicating the linearization
#'    wrapper to use when none is specified. Its default value depends on
#'    the value of \code{default_stat} in \code{define_variance_wrapper}
#'    \item \code{alpha}: a numeric vector of size 1 indicating the threshold
#'    for confidence interval derivation. Its default value depends on
#'    the value of \code{default_alpha} in \code{define_variance_wrapper}
#'    \item \code{id}: a character vector of size 1 containing the name of
#'    the identifying variable in the survey file. It can also be an 
#'    unevaluated expression (using \code{substitute()}) to be evaluated within
#'    the survey file. Its default value depends on the value of 
#'    \code{default_id} in \code{define_variance_wrapper}
#'    \item \code{envir}: an environment containing a binding to \code{data}
#'  }
#' 
#' @author Martin Chevalier
#'    
#' @seealso \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}} \code{\link{varDT}}
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
#' # Step 1 : Definition of a variance function
#' 
#' # In this context, the variance estimation function specific to the ICT 
#' # survey can be defined as follows:
#' 
#' variance_function_ict <- function(y, x, w, samp){
#'   
#'   # Calibration
#'   y <- rescal(y, x = x, w = w)
#'   
#'   # Non-response
#'   y <- add0(y, rownames = samp$firm_id)
#'   var_nr <- var_pois(y, pik = samp$response_prob_est, w = samp$w_sample)
#'   
#'   # Sampling
#'   y <- y / samp$response_prob_est
#'   var_sampling <- var_srs(y, pik = 1 / samp$w_sample, strata = samp$division)
#'   
#'   var_sampling + var_nr
#'   
#' }
#' 
#' # y is the matrix of variables of interest, x, w, and samp are some technical data:
#' technical_data_ict <- list(
#'   
#'   # x: calibration variables matrix
#'   x = as.matrix(ict_sample[
#'     ict_survey$firm_id,
#'     c(paste0("N_", 58:63), paste0("turnover_", 58:63))
#'   ]),
#' 
#'   # w: calibrated weights
#'   w = ict_survey$w_calib[order(ict_survey$firm_id)],
#'   
#'   # samp: sample file
#'   samp = ict_sample
#'   
#' )
#' 
#' # Test of the variance function
#' y <- matrix(ict_survey$speed_quanti, dimnames = list(ict_survey$firm_id))
#' with(technical_data_ict, variance_function_ict(y, samp = samp, x = x, w = w))
#' 
#' # Step 2 : Definition of a variance wrapper
#' 
#' variance_wrapper_ict <- define_variance_wrapper(
#'   variance_function = variance_function_ict,
#'   reference_id = ict_survey$firm_id, 
#'   technical_data = technical_data_ict,
#'   default = list(id = "firm_id", weight = "w_calib")
#' )
#' 
#' # The object technical_data_ict is embedded within 
#' # the function variance_wrapper_ict
#' ls(environment(variance_wrapper_ict))
#' # Note : variance_wrapper_ict is a closure
#' # (http://adv-r.had.co.nz/Functional-programming.html#closures)
#' # As a consequence, the variance wrapper will work even if 
#' # technical_data_ict is removed from globalenv()
#' rm(technical_data_ict)
#' 
#' # Step 3 : Features of the variance wrapper
#' 
#' # Better display of results
#' variance_wrapper_ict(ict_survey, speed_quanti)
#' 
#' # Mean linearization
#' variance_wrapper_ict(ict_survey, mean(speed_quanti))
#' # Ratio linearization
#' variance_wrapper_ict(ict_survey, ratio(turnover, employees))
#' 
#' # Discretization of qualitative variables
#' variance_wrapper_ict(ict_survey, speed_quali)
#' # On-the-fly recoding
#' variance_wrapper_ict(ict_survey, speed_quali == "Between 2 and 10 Mbs")
#' 
#' # 1-domain estimation
#' variance_wrapper_ict(ict_survey, speed_quanti, where = division == "58")
#' # Multiple domains estimation
#' variance_wrapper_ict(ict_survey, speed_quanti, by = division)
#' 
#' # Multiple variables at a time
#' variance_wrapper_ict(ict_survey, speed_quanti, big_data)
#' variance_wrapper_ict(ict_survey, speed_quanti, mean(big_data))
#' # Flexible syntax for where and by arguments
#' # (similar to the aes() function in ggplot2)
#' variance_wrapper_ict(ict_survey, where = division == "58", 
#'   mean(speed_quanti), mean(big_data * 100)
#' )
#' variance_wrapper_ict(ict_survey, where = division == "58", 
#'   mean(speed_quanti), mean(big_data * 100, where = division == "61")
#' )
#' variance_wrapper_ict(ict_survey, where = division == "58", 
#'   mean(speed_quanti), mean(big_data * 100, where = NULL)
#' )
#' 
#' @export define_variance_wrapper
#' @import Matrix

define_variance_wrapper <- function(
  variance_function, 
  reference_id,
  technical_data = NULL,
  default = list(id = NULL, weight = NULL, stat = "total", alpha = 0.05), 
  arg_type = NULL,
  objects_to_include = NULL
){

  # TODO: add some sort of startup message on first run of the function
  # whith the package version number and the github repo. Something like : 
  # "Variance wrapper generated by the gustave V.V on DD/MM/YYYY. See https://github.com/martinchevalier/gustave" for documentation and bug reports."
  
  # TODO: enable magrittr pipe %>% operations

  # Step 0.0: Control if arguments are missing
  if(missing(variance_function)) 
    stop("A variance estimation function must be provided (see variance_function argument).")
  if(missing(reference_id)) 
    stop("A reference id must be provided (see reference_id argument).")

  # Step 0.1: Provide default values
  if(is.null(default$stat) && !("stat" %in% names(default))) default$stat <- "total"
  if(is.null(default$alpha) && !("alpha" %in% names(default))) default$alpha <- 0.05
  names_formals_variance_function <- names(formals(variance_function))
  names_technical_data <- names(technical_data)
  if(is.null(arg_type)) arg_type <- list(
    data = names_formals_variance_function[1],
    tech = names_technical_data,
    param = setdiff(names_formals_variance_function, c(names_formals_variance_function[1], names_technical_data))
  )

  # Step 0.2 : Control arguments consistency
  if(length(arg_type$data) == 0)
    stop("A data argument is expected in variance_function and should be described as such in arg_type.")
  if(length(arg_type$data) > 1)
    stop("Only one data argument is expected in variance_function and should be described as such in arg_type.")
  inconsistent_arg <- list(
    in_arg_type_not_in_variance_function = setdiff(unlist(arg_type), names_formals_variance_function),
    in_variance_function_not_in_arg_type = setdiff(names_formals_variance_function, unlist(arg_type)),
    tech_arg_not_in_technical_data = setdiff(arg_type$tech, names_technical_data),
    technical_data_not_in_tech_arg = setdiff(names_technical_data, arg_type$tech)
  )
  if(length(unlist(inconsistent_arg)) > 0) stop(
    "Some arguments are inconsistent:", 
    if(length(inconsistent_arg[[1]]) > 0) paste("\n  -", paste(inconsistent_arg[[1]], collapse = ", "), "in arg_type but not in variance_function arguments") else "", 
    if(length(inconsistent_arg[[2]]) > 0) paste("\n  -", paste(inconsistent_arg[[2]], collapse = ", "), "in variance_function arguments but not in arg_type") else "",
    if(length(inconsistent_arg[[3]]) > 0) paste("\n  -", paste(inconsistent_arg[[3]], collapse = ", "), "identified as technical data argument in arg_type but not in technical_data") else "",
    if(length(inconsistent_arg[[4]]) > 0) paste("\n  -", paste(inconsistent_arg[[4]], collapse = ", "), "in technical_data but not identified as technical data argument in arg_type") else ""
  )
  
  
  # Step 1 : Creating the variance estimation wrapper
  variance_wrapper <- function(
    data, ..., by = NULL, where = NULL, stat = NULL, alpha = NULL,
    weight = NULL, id = NULL, display = TRUE, envir = parent.frame()
  ){

    if(!("package:Matrix" %in% search())) attachNamespace("Matrix")
    
    # Step 1.0: Retrieve information about the environments,
    # the call and evaluate the data argument
    evaluation_envir <- envir
    execution_envir <- environment()
    call <- match.call(expand.dots = TRUE)
    substitute_data <- substitute(data)
    eval_data <- eval(substitute_data, evaluation_envir)
    
    # Step 1.1: Control identifiers
    reference_id <- eval(reference_id)
    id <- if(is.character(id)) eval_data[, id] else eval(id, eval_data)      
    in_reference_id_not_in_id <- setdiff(reference_id, id)
    if(length(in_reference_id_not_in_id) > 0)
      warning("Some observations from the survey appear to be missing. The variance estimation function may produce unexpected results.", call. = FALSE)
    in_id_not_in_reference_id <- setdiff(id, reference_id)
    if(length(in_id_not_in_reference_id) > 0)
      stop("Some observations do not belong to the survey.", call. = FALSE)

    # Step 1.2 : Specify default values for stat, weight, by and where arguments
    l <- eval(substitute(alist(...)))
    l <- lapply(l, function(i){
      if(is.symbol(i) || !("gustave_linearization_wrapper" %in% class(eval(i[[1]]))))
        i <- as.call(c(as.symbol(stat), i))
      i <- as.list(i)
      for(j in environment(eval(i[[1]]))$arg_type$weight){
        if(!(j %in% names(i))) i[[j]] <- if(is.character(weight)) as.symbol(weight) else weight
      }
      if(!("by" %in% names(i))) i$by <- substitute(by, execution_envir)
      if(!("where" %in% names(i))) i$where <- substitute(where, execution_envir)
      as.call(i)
    })
    # TODO: keep track of the non-automatically assigned weight arguments
    # in order to display them in the call columns of the output (see
    # define_linearization_wrapper, about row 44).
  
    # Step 1.3 : Call the linearization wrappers
    labels <- if(!is.null(names(l))) names(l) else rep(NA, length(l))
    labels[labels %in% ""] <- NA
    d <- unlist(lapply(seq_along(l), function(i){
      linearization_wrapper_call <- as.call(c(as.list(l[[i]]), list(
        data = substitute_data, label = labels[i]
        , evaluation_envir = evaluation_envir, execution_envir = execution_envir
      )))
      eval(linearization_wrapper_call, envir = execution_envir)
    }), recursive = FALSE)
    if(is.null(d)) stop("No variable to estimate variance on.", call. = FALSE)

    # Step 1.4 : Build up the sparse matrix to be used in the estimation
    d_matrix <- {
      data <- lapply(d, function(k){
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

    # Step 1.5 : Call the variance estimation function
    variance_function_args <- c(
      stats::setNames(list(d_matrix), arg_type$data)
      , stats::setNames(lapply(arg_type$param, get, envir = execution_envir), arg_type$param)
      , technical_data
    )[names(formals(variance_function))]
    r <- suppressMessages(do.call(variance_function, variance_function_args))
    if(is.data.frame(r)) r <- as.matrix(r)
    if(!is.list(r)) r <- list(var = r)

    # Step 1.6 Reorganize the results of the estimation
    k <- 0;
    d <- lapply(seq_along(d), function(i) c(d[[i]]
      , list(var = lapply(d[[i]]$lin, function(j){
        tmp <- r[[1]][(k + 1):(k + NCOL(j))]
        assign("k", (k + NCOL(j)), envir = execution_envir)
        return(tmp)
      }))
    ))

    # Step 1.7 : Display the results if requested (the default)
    if(display){
      d <- lapply(d, function(i) with(i, 
        display_function(var = var, metadata = metadata, alpha = alpha)
      ))
      names <- unique(do.call(base::c, lapply(d, names)))
      d <- do.call(rbind, lapply(d, function(i){
        i[, setdiff(names, names(i))] <- NA
        i[, names]
      }))
      d <- d[, sapply(d, function(i) !all(is.na(i)))]
      rownames(d) <- NULL
      d
    }else invisible(d)

  }

  # Step 2.1: Modify variance_wrapper arguments depending on the context
  if(!is.null(default$id)) formals(variance_wrapper)$id <- substitute(default$id)
  if(!is.null(default$weight)) formals(variance_wrapper)$weight <- substitute(default$weight)
  if(!is.null(default$stat)) formals(variance_wrapper)$stat <- default$stat
  if(!is.null(default$alpha)) formals(variance_wrapper)$alpha <- default$alpha
  
  # Step 2.2: Add variance_function parameters to variance_wrapper arguments
  # (just after the ...)
  add_variance_function_param_after <- match("...", names(formals(variance_wrapper)))
  formals(variance_wrapper) <- c(formals(variance_wrapper)[1:add_variance_function_param_after], 
                                 formals(variance_function)[arg_type$param],
                                 formals(variance_wrapper)[(add_variance_function_param_after + 1):length(formals(variance_wrapper))]
  )
  
  # Step 3 : Include objects in variance_wrapper enclosing environment
  e1 <- new.env(parent = globalenv())
  assign_all(objects = ls(asNamespace("gustave")), to = e1, from = asNamespace("gustave"))
  e2 <- new.env(parent = e1)
  assign_all(objects = c("variance_function", "reference_id", "technical_data", "arg_type"), to = e2, from = environment())
  assign_all(objects = objects_to_include, to = e2, from = parent.frame())
  variance_wrapper <- change_enclosing(variance_wrapper, envir = e2)

  structure(variance_wrapper, class = c("function", "gustave_variance_wrapper"))

}

