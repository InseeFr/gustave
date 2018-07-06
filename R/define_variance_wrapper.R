
#' Define a variance estimation wrapper

#' @description Given a variance estimation \emph{function} (specific to a 
#'   survey), \code{define_variance_wrapper} defines a variance estimation 
#'   \emph{wrapper} easier to use (e.g. automatic domain estimation, 
#'   linearization).
#'   
#' @param variance_function An R function. It is the methodological workhorse of 
#'   the estimation of variance: from a set of arguments including the variables 
#'   of interest (see below), it should return a vector of estimated variances 
#'   (or a list whose first element is a vector of estimated variances). See Details.
#' @param reference_id A vector containing the ids of all the responding units
#'   of the survey. It can also be an unevaluated expression (enclosed in 
#'   \code{quote()}) to be evaluated within the execution environment of the wrapper.
#'   It is compared with \code{default$id} (see below) to check whether 
#'   some observations are missing in the survey file. The matrix of variables 
#'   of interest has \code{reference_id} as rownames and is ordered according
#'   to its values before being processed by \code{variance_function}.
#' @param reference_weight A vector containing the reference weight of the survey. 
#'   It can also be an unevaluated expression (enclosed in \code{quote()}) to 
#'   be evaluated within the execution environment of the wrapper. 
#' @param default_id A character vector of length 1, the name of the default 
#'   identifying variable in the survey file. It can also be an unevaluated 
#'   expression (enclosed in \code{quote()}) to be evaluated within the survey file.
#' @param technical_data A named list of technical data needed to perform 
#'   the variance estimation (e.g. sampling strata, first- or second-order 
#'   probabilities of inclusion, estimated response probabilities, calibration 
#'   variables). Its names should match the names of the arguments 
#'   of \code{variance_function}.
#' @param technical_param A named list of technical parameters used to control 
#'   some aspect of the variance estimation process (e.g. alternative methodology).
#'   Its names should match the names of the arguments  of \code{variance_function}.
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
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}})}
#'   
#'   \code{define_variance_wrapper} allows the sampling expert to define a 
#'   variance estimation wrapper around a given variance estimation function and
#'   set its default parameters. The produced variance estimation wrapper will 
#'   be stand-alone in the sense that it can contain all technical data necessary
#'   to carry out the estimation (see \code{technical_data}).
#'   
#'   The arguments of the \code{variance_function} fall into three types: \itemize{
#'   \item the data argument (mandatory, only one allowed): the numerical matrix of 
#'   variables of interest to apply the variance estimation formula on
#'   \item technical data arguments (optional, one or more allowed): technical 
#'   and methodological information used by the variance estimation function
#'   (e.g. sampling strata, first- or second-order probabilities of inclusion, 
#'   estimated response probabilities, calibration variables)
#'   \item technical parameters (optional, one or more allowed): non-data arguments 
#'   to be used to control some aspect of the variance estimation (e.g. alternative
#'   methodology)
#'   }
#'   \code{technical_data} and \code{technical_param} are used to determine
#'   which arguments of \code{variance_function} relate to technical information, 
#'   the only remaining argument is considered as the data argument.
#'   
#' @return An R function that makes the estimation of variance based on the provided 
#' variance function easier. Its parameters are:
#'   \itemize{
#'    \item \code{data}: the survey data where the interest variables are stored
#'    \item \code{...}: one or more calls to a statistic wrapper (see examples
#'    and \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}})
#'    \item \code{where}: a logical vector indicating a domain on which the variance
#'    estimation is conducted
#'    \item \code{by}: a qualitative variable whose levels are used to define domains
#'    on which the variance estimation is conducted
#'    \item \code{alpha}: a numeric vector of size 1 indicating the threshold
#'    for confidence interval derivation. Its default value depends on
#'    the value of \code{default_alpha} in \code{define_variance_wrapper}
#'    \item \code{id}: a character vector of size 1 containing the name of
#'    the identifying variable in the survey file. Its default value depends 
#'    on the value of \code{default_id} in \code{define_variance_wrapper}
#'    \item \code{envir}: an environment containing a binding to \code{data}
#'  }
#' 
#' @author Martin Chevalier
#'    
#' @seealso \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}} \code{\link{varDT}}
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
#'   y <- add0(y, rownames = samp$id)
#'   var_nr <- var_pois(y, pik = samp$response_prob_est, w = samp$w_sample)
#'   
#'   # Sampling
#'   y <- y / samp$response_prob_est
#'   var_sampling <- var_srs(y, pik = 1 / samp$w_sample, strata = samp$strata)
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
#'   samp = list(
#'     id = ict_sample$firm_id, 
#'     w_sample = ict_sample$w_sample,
#'     response_prob_est = ict_sample$w_sample / ict_sample$w_nrc,
#'     strata = ict_sample$strata
#'   )
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
#'   reference_weight = ict_survey$w_calib, 
#'   default_id = "firm_id",
#'   technical_data = technical_data_ict
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
#' # Mean statistic
#' variance_wrapper_ict(ict_survey, mean(speed_quanti))
#' # Ratio statistic
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

define_variance_wrapper <- function(variance_function, 
                                    reference_id,
                                    reference_weight,
                                    default_id = NULL,
                                    technical_data = NULL,
                                    technical_param = NULL,
                                    objects_to_include = NULL
){

  # TODO: add some sort of startup message on first run of the function
  # with the package version number and the github repo. Something like : 
  # "Variance wrapper generated by the gustave V.V on DD/MM/YYYY. See https://github.com/martinchevalier/gustave" for documentation and bug reports."
  # Check what Matrix does.
  
  # Step I: Controls 

  # Missing arguments  
  is_missing <- c(
    variance_function = missing(variance_function),
    reference_id = missing(reference_id),
    reference_weight = missing(reference_weight)
  )
  if(any(is_missing)) stop(
    "The following arguments are missing: ", 
    paste(names(which(is_missing)), collapse = ", "), "."
  )
  
  # Determine argument type
  names_formals_variance_function <- names(formals(variance_function))
  names_technical_data <- names_else_NA(technical_data)
  if(anyNA(names_technical_data)) stop("All elements of technical_data must be named.")
  if(!all(names_technical_data %in% names_formals_variance_function))
    stop("All elements of technical_data must match an argument of variance_function.")
  names_technical_param <- names_else_NA(technical_param)
  if(anyNA(names_technical_param)) stop("All elements of technical_param must be named.")
  if(!all(names_technical_param %in% names_formals_variance_function))
    stop("All elements of technical_param must match an argument of variance_function.")
  names_remaining_args <- setdiff(names_formals_variance_function, c(names_technical_data, names_technical_param))
  if(length(names_remaining_args) == 0) stop(
    "variance_function appears to have only technical arguments (identified with technical_data and technical_param).",
    " It must also have a data argument (see the Details section in help)."
  )
  if(length(names_remaining_args) > 1) stop(
    "variance_function appears to have several data arguments (", paste(names_remaining_args, collapse = ", "), 
    ") where it should only have one. Use technical_data and technical_param to identify the technical arguments (see the Details section in help)."
  )
  arg_type <- list(data = names_remaining_args, tech_data = names_technical_data, tech_param = names_technical_param)

  # Step II: Create the variance wrapper
  variance_wrapper <- function(
    data, ..., by = NULL, where = NULL, id, 
    alpha = 0.05, display = TRUE, envir = parent.frame()
  ){

    if(!("package:Matrix" %in% search())) attachNamespace("Matrix")

    # Step 1: Preliminary operations and controls
    
    # Step 1.1: Environments and missing arguments
    evaluation_envir <- envir
    execution_envir <- environment()
    is_missing <- c(
      data = missing(data),
      id = missing(id) && is.symbol(formals()$id)
    )
    if(any(is_missing)) stop(
      "The following arguments are missing: ", 
      paste(names(which(is_missing)), collapse = ", "), "."
    )
    
    # Step 1.2 Evaluation 
    data <- eval(data, envir = evaluation_envir)
    reference_id <- eval(reference_id)
    id <- tryCatch(
      eval(substitute(id), envir = execution_envir),
      error = function(e) substitute(id, execution_envir)
    )
    id <- if(is.character(id)) data[, id] else eval(id, data)
    reference_weight <- eval(reference_weight)
    
    # Step 1.3: Controls
    in_reference_id_not_in_id <- setdiff(reference_id, id)
    if(length(in_reference_id_not_in_id) > 0)
      warn("Some observations from the survey appear to be missing. The variance estimation function may produce unexpected results.")
    in_id_not_in_reference_id <- setdiff(id, reference_id)
    if(length(in_id_not_in_reference_id) > 0){
      warn(length(in_id_not_in_reference_id), " observations do not match any responding units of the survey. They are discarded.")
      data <- data[id %in% reference_id, ]
      id <- id[id %in% reference_id]
    }
    if(!identical(match_id <- match(reference_id, id), seq_along(reference_id))){
      warn(
        "The inputted id variable (id argument) appears not to match the reference ",
        "id variable provided when the variance wrapper was defined: it is reordered ",
        "and everything should be fine. Issues may nonetheless arise if part of the call ", 
        "is to be evaluated outside of the inputted data.frame (data argument)."
      )
      data <- data[match_id, ]  
    }

    # Step 2: Handling domains, qualitative variables and linearization
    # TODO: POC for direct evaluation of ... (compatible with lapply())
    # statistic_wrapper_list <- as.list(substitute(list(...)))[-1];
    # spy <<- lapply(statistic_wrapper_list, eval, envir = evaluation_envir); stop()
    statistic_wrapper_list <- eval(substitute(alist(...)))
    statistic_wrapper_label <- names_else_NA(statistic_wrapper_list)
    data_as_list <- unlist(lapply(seq_along(statistic_wrapper_list), function(i){

      call <- statistic_wrapper_list[[i]]
      
      # Add a statistic wrapper when none is spefified
      if(is.symbol(call) || !is_statistic_wrapper(eval(call[[1]]))) 
        call <- as.call(c(quote(total), call))
      # TODO: Suppres the auto-completion of total() with an error message

      # Evaluate the statistic wrapper
      d <- eval(call)
      if(is.null(d)) return(d)
      # TODO: Rethink the process so that lists of statistic_wrappers
      # generated by a lapply() may be correctly handled (to be used
      # with standard evaluation)
      
      # Add labels
      # TODO: Have labels as list names and use them as metadata 
      # in a subsequent step
      lapply(d, function(dd){
        dd$metadata$label <- statistic_wrapper_label[i]
        dd
      })

    }), recursive = FALSE)
    if(is.null(data_as_list)) stop("No variable to estimate variance on.")
    
    # Step 3: Variance estimation
    
    # Step 3.1: Build up the sparse matrix to be used in the estimation
    data_as_Matrix <- {
      data <- lapply(data_as_list, function(k){
        t <- do.call(cbind, k$lin)
        Matrix::sparseMatrix(
          i = rep(k$metadata$row_number, NCOL(t))
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
    # TODO: Handle the node stack overflow problem

    # Step 3.2: Call the variance estimation function
    variance_function_args <- c(
      stats::setNames(list(data_as_Matrix), arg_type$data), 
      stats::setNames(lapply(arg_type$tech_param, get, envir = execution_envir), arg_type$tech_param), 
      technical_data
    )
    result <- suppressMessages(do.call(variance_function, variance_function_args))
    if(is.data.frame(result)) result <- as.matrix(result)
    if(!is.list(result)) result <- list(var = result)

    # Step 4: Results
    
    # Step 4.1: Reorganize the results of the estimation
    k <- 0;
    data_as_list <- lapply(seq_along(data_as_list), function(i) c(
      data_as_list[[i]], 
      list(var = lapply(data_as_list[[i]]$lin, function(j){
        tmp <- result[[1]][(k + 1):(k + NCOL(j))]
        assign("k", (k + NCOL(j)), envir = execution_envir)
        return(tmp)
      }))
    ))

    # Step 4.2: Display the results if requested (the default)
    if(display){
      data_as_list <- lapply(data_as_list, function(i) with(i, 
        display_function(point = point, var = var, metadata = metadata, alpha = alpha)
      ))
      names <- unique(do.call(base::c, lapply(data_as_list, names)))
      data_as_list <- do.call(rbind, lapply(data_as_list, function(i){
        i[, setdiff(names, names(i))] <- NA
        i[, names]
      }))
      data_as_list <- data_as_list[, sapply(data_as_list, function(i) !all(is.na(i)))]
      rownames(data_as_list) <- NULL
      data_as_list
    }else invisible(data_as_list)

  }

  # Step III: Finalize the variance wrapper
  
  # Step III.1: Modify variance wrapper arguments depending on the context
  if(!is.null(default_id)) formals(variance_wrapper)$id <- default_id

  # Step III.2: Add variance_function technical parameters to variance_wrapper arguments
  # (just after the ...)
  add_technical_param_after <- match("...", names(formals(variance_wrapper)))
  formals(variance_wrapper) <- c(
    formals(variance_wrapper)[1:add_technical_param_after], 
    technical_param,
    formals(variance_wrapper)[(add_technical_param_after + 1):length(formals(variance_wrapper))]
  )
  
  # Step III.3: Include objects in variance_wrapper enclosing environment
  e1 <- new.env(parent = globalenv())
  assign_all(objects = ls(asNamespace("gustave")), to = e1, from = asNamespace("gustave"))
  e2 <- new.env(parent = e1)
  assign_all(objects = c("variance_function", "reference_id", "reference_weight", "technical_data", "arg_type"), to = e2, from = environment())
  assign_all(objects = objects_to_include, to = e2, from = parent.frame())
  variance_wrapper <- change_enclosing(variance_wrapper, envir = e2)

  structure(variance_wrapper, class = c("function", "gustave_variance_wrapper"))

}
