


#' Perform a variance estimation for one-stage stratified surveys 
#' taking out-of-scope units, non-response and calibration into acount
#' 
#' @description \code{qvar} is a function performing analytical 
#' variance estimation in most common cases, that is: \itemize{\item stratified 
#' simple random sampling \item correct handling of out-of-scope units \item 
#' non-response correction (if any) through reweighting \item calibration (if any)}
#' 
#' Used with \code{define = TRUE}, it defines a so-called variance wrapper, that 
#' is a standalone ready-to-use function that can be applied to the survey dataset 
#' without having to specify the methodological characteristics of the survey.
#'
#' @param data The \code{data.frame} containing all the technical information
#'   required to prepare the variance estimation process (see other arguments 
#'   below). Note that this file should contain all the units sampled, 
#'   including the out-of-scope and non-responding units. If a variance
#'   estimation is to be performed right away (when \code{define = FALSE}),
#'   it should also contain the variables of interest.
#' @param ... One or more calls to a statistic wrapper (e.g. \code{total()}, 
#'   \code{mean()}, \code{ratio()}). See examples and 
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}})
#' @param where A logical vector indicating a domain on which the variance 
#'   estimation is to be performed.
#' @param by A qualitative variable whose levels are used to define domains
#'   on which the variance estimation is performed.
#' @param alpha A numeric vector of length 1 indicating the threshold
#'   for confidence interval derivation (\code{0.05} by default).
#' @param display A logical verctor of length 1 indicating whether
#'   the result of the estimation should be displayed or not.
#'   
#' @param id The identification variable of the units in \code{data}. 
#'   It should be unique for each row in \code{data} and not contain any 
#'   missing values.
#' @param dissemination_dummy A character vector of length 1, the name
#'   of the logical variable in \code{data} indicating whether the unit
#'   does appear in the disseminated file and should be used for point
#'   estimates.
#' @param dissemination_weight A character vector of length 1, the name
#'   of the numerical variable in \code{data} corresponding to the 
#'   dissemination weight of the survey. It should not contain any missing 
#'   (NA) values.
#'
#' @param sampling_weight A character vector of length 1, the name of the 
#'   numeric variable in \code{data} corresponding to the sampling weights 
#'   of the survey. It should not contain any missing (NA) values.
#' @param strata A character vector of length 1, the name of the factor 
#'   variable in \code{data} whose level match the stratification
#'   used in the survey. Character variables are coerced to factor.
#'   If defined, it should not contain any missing value. If \code{NULL},
#'   the variance estimation process does not take any stratification
#'   into account.
#'
#' @param scope_dummy A character vector of length 1, the name of the logical 
#'   variable in \code{data} indicating whether the unit belongs to the
#'   scope of the survey or not. Numerical variables are coerced to logical.
#'   If defined, it should not contain any missing value. If \code{NULL},
#'   all units are supposed to be within the scope of the survey.
#'
#' @param nrc_weight A character vector of length 1, the name of the 
#'   numerical variable in \code{data} corresponding to the weights
#'   after non-response correction. If defined, all responding units 
#'   (see below) should have a non-missing value. If \code{NULL}, all
#'   units are supposed to be responding and the variance estimation
#'   process does not include a second phase in order to take non-response
#'   into account.
#' @param response_dummy A character vector of length 1, the name of of the logical 
#'   variable in \code{data} indicating whether the unit is a responding 
#'   unit or not. Numerical variables are coerced to logical. \code{response_dummy}
#'   should be defined as long as a \code{nrc_weight} is provided. All units 
#'   in the scope of the survey should have a non-missing value.
#' @param nrc_dummy A character vector of length 1, the name of
#'   the logical variable in \code{data} indicating whether the
#'   units did take part in the non-response correction process. 
#'   All units in the scope of the survey should have a non-missing 
#'   value. 
#'
#' @param calibration_weight A character vector of length 1, the name of the 
#'   numerical variable in \code{data} corresponding to the calibrated
#'   weights. If defined, all responding units (see above) should have 
#'   a non-missing value. If \code{NULL}, the variance estimation
#'   process does not take any calibration step into account.
#' @param calibration_dummy A character vector of length 1, the name of of the logical 
#'   variable in \code{data} indicating whether the unit did take part
#'   in the calibration process or not. Numerical variables are coerced to 
#'   logical. If defined, all responding units should have a non-missing
#'   value. If \code{NULL}, calibration is supposed to have been performed
#'   on all responding units.
#' @param calibration_var A character vector, the name of the variable(s) used in
#'   the calibration process. Logical variables are coerced to numeric. 
#'   Character and factor variables are automatically discretized. 
#'   \code{calibration_var} should be defined as long as a \code{calibration_weight} is 
#'   provided. All units taking part in the calibration process should have
#'   only non-missing values for all variables in \code{calibration_var}.
#' 
#' @param define Logical vector of lentgh 1. Should a variance wrapper
#'   be defined instead of performing a variance estimation?
#' @param envir An environment containing a binding to \code{data}.
#' 
#' @details \code{qvar} aims at being a straightforward variance
#'   estimation tool in the most common cases in social or business
#'   surveys, that is a stratified simple random sampling with 
#'   additional reweighting steps to correct non-response bias
#'   or reduce variance through calibration.
#'   
#'   It performs not only tehnical but also methodological checks
#'   in order to ensure that the variance estimation methodology 
#'   do apply (e.g. equal probability of inclusion within strata, 
#'   number of units per stratum). 
#'   
#'   When the sampling design or reweighting schemes are more complex,
#'   a specific variance estimation wrapper may be defined using
#'   the \code{\link{define_variance_wrapper}} function.
#'   
#'   "qvar" strands for (in French) "Variance estimation
#'   for stratified simple random sampling surveys with reweighting".
#'   
#' @seealso \code{\link{define_variance_wrapper}} \code{\link{standard_statistic_wrapper}}
#' 
#' @export

qvar <- function(data, ..., by = NULL, where = NULL, 
                 alpha = 0.05, display = TRUE, 
                 id, dissemination_dummy, dissemination_weight,
                 sampling_weight, strata = NULL,
                 scope_dummy = NULL, 
                 nrc_weight = NULL, response_dummy = NULL, nrc_dummy = NULL,
                 calibration_weight = NULL, calibration_dummy = NULL, calibration_var = NULL,
                 define = FALSE, envir = parent.frame()
){
  
  # Step 1: Define the variance wrapper
  call <- as.list(match.call())[-1]
  call$envir <- envir
  qvar_wrapper <- do.call(
    define_qvar_wrapper, 
    call[names(call) %in% names(formals(define_qvar_wrapper))]
  )
  
  # Step 2: Export the variance wrapper
  if(define || missing(...)){
    if(define){
      note("As define = TRUE, a ready-to-use variance wrapper is (invisibly) returned.")  
    }else{
      note("No variable to perform variance estimation on are specified. A ready-to-use variance wrapper is (invisibly) returned instead.")
    }
    return(invisible(qvar_wrapper))
  }
  
  # Step 3: Estimate variance
  qvar_data <- data[data[, id] %in% environment(qvar_wrapper)$reference_id, ]  
  call$data <- substitute(qvar_data)
  call$envir <- environment()
  do.call(
    qvar_wrapper,
    call[names(call) == "" | names(call) %in% names(formals(qvar_wrapper))]
  )

}



# Unexported (and undocumented) functions

define_qvar_wrapper <- function(data, id, dissemination_dummy, dissemination_weight,
                                  sampling_weight, strata = NULL,
                                  scope_dummy = NULL, 
                                  nrc_weight = NULL, response_dummy = NULL, nrc_dummy = NULL,
                                  calibration_weight = NULL, calibration_dummy = NULL, calibration_var = NULL,
                                  envir = parent.frame()
){
  
  # Step 1: Control arguments consistency and display the welcome message ----
  
  # Step 1.1: Arguments consistency
  is_missing <- c(
    data = missing(data),
    id = missing(id),
    dissemination_dummy = missing(dissemination_dummy),
    dissemination_weight = missing(dissemination_weight),
    sampling_weight = missing(sampling_weight)
  )
  if(any(is_missing)) stop(
    "The following arguments are missing: ", 
    paste(names(which(is_missing)), collapse = ", "), "."
  )
  inconsistency <- list(
    nrc_weight_but_no_response_dummy = !is.null(nrc_weight) && is.null(response_dummy),
    resp_or_nrc_dummy_but_no_nrc_weight = is.null(nrc_weight) && (!is.null(response_dummy) || !is.null(nrc_dummy)),
    calibration_weight_but_no_calibration_var = !is.null(calibration_weight) && is.null(calibration_var),
    calibration_or_calibration_var_but_no_calibration_weight = is.null(calibration_weight) && (!is.null(calibration_dummy) || !is.null(calibration_var))
  )
  if(any(unlist(inconsistency))) stop(
    "Some arguments are inconsistent:", 
    if(inconsistency$nrc_weight_but_no_response_dummy) 
      "\n  - weights after non-response correction are provided (nrc_weight argument) but no variable indicating responding units (response_dummy argument)" else "", 
    if(inconsistency$resp_or_nrc_dummy_but_no_nrc_weight) 
      "\n  - a variable indicating responding units and/or a variable indicating the units taking part in the non-response correction process are provided (response_dummy and nrc_dummy argument) but no weights after non-response correction (nrc_weight argument)." else "" ,
    if(inconsistency$calibration_weight_but_no_calibration_var) 
      "\n  - calibrated weights are provided (calibration_weight argument) but no calibration variables (calibration_var argument)" else "" ,
    if(inconsistency$calibration_or_calibration_var_but_no_calibration_weight) 
      "\n  - a variable indicating the units taking part in a calibration process and/or calibration variables are provided (calibration_dummy and calibration_var arguments) but no calibrated weights (calibration_weight argument)" else ""
  )

  # Step 1.2: Welcome message
  message(
    "Survey variance estimation with the gustave package",
    "\n\nThe following features are taken into account:",
    if(!is.null(strata)) "\n  - stratified simple random sampling" else 
      "\n  - simple random sampling WITHOUT stratification",
    if(!is.null(scope_dummy)) "\n  - out-of-scope units" else "",
    if(!is.null(nrc_weight)) "\n  - non-response correction through reweighting" else "",
    if(!is.null(calibration_weight)) "\n  - calibration on margins" else "",
    "\n"
  )
  
  # Step 2: Control that arguments do exist and retrieve their value ----

  # Step 2.1: Evaluation of all arguments
  deparse_data <- deparse(substitute(data))
  data <- eval(substitute(data), envir = envir)
  if(!is.data.frame(data)) stop("data argument must refer to a data.frame")
  arg <- lapply(as.list(match.call())[-1], eval)

  # Step 2.2: Expected length
  should_be_single_variable_name <- intersect(c(
    "id", "dissemination_dummy", "dissemination_weight",
    "sampling_weight", "strata", "scope_dummy", 
    "nrc_weight", "response_dummy", "nrc_dummy",
    "calibration_weight", "calibration_dummy"
  ), names(arg))
  should_be_variable_name_vector <- intersect(c("calibration_var"), names(arg))
  should_be_variable_name <- c(should_be_single_variable_name, should_be_variable_name_vector)

  # Step 2.3: Check whether arguments are character vectors and 
  # have the expected length
  is_single_variable_name <- sapply(
    arg[should_be_single_variable_name], 
    function(arg) is.null(arg) || is_variable_name(arg, max_length = 1)
  )
  if(any(!is_single_variable_name)) stop(
    "The following arguments do not refer to a variable name (character vector of length 1): ", 
    names(is_single_variable_name)[!is_single_variable_name]
  )
  is_variable_name_vector <- sapply(
    arg[should_be_variable_name_vector], 
    function(arg) is.null(arg) || is_variable_name(arg, max_length = Inf)
  )
  if(any(!is_variable_name_vector)) stop(
    "The following arguments do not refer to a vector of variable names: ", 
    names(is_variable_name_vector)[!is_variable_name_vector]
  )

  # Step 2.4: Check the presence of the variables in data
  is_not_in_data <- lapply(should_be_variable_name, function(param){
    tmp <- variable_not_in_data(var = arg[[param]], data = data)
    if(is.null(tmp)) return(NULL)
    paste0("\n  - ", param, " argument: ", paste0(tmp, collapse = " "))
  })
  if(length(unlist(is_not_in_data)) > 0) stop(
    "Some variables do not exist in ", deparse_data, ": ",
    unlist(is_not_in_data[!is.null(is_not_in_data)])
  )
  
  # Step 2.5: Retrieve the value of the arguments
  data <- data[order(data[[arg$id]]), ]
  list2env(c(
    lapply(
      arg[should_be_single_variable_name], function(param) 
        stats::setNames(data[[param]], data[[arg$id]])
    ),
    lapply(arg[should_be_variable_name_vector], function(param){
      tmp <- data[param]
      row.names(tmp) <- data[[arg$id]]
      tmp
    })
  ), envir = environment())

  
  # Step 3: Control arguments value ----

  # id
  if(anyNA(id))
    stop("The id variable (", arg$id, ") should not contain any missing (NA) values.")
  if(any(duplicated(id)))
    stop("The id variable (", arg$id, ") should not contain any duplicated values.")

  # dissemination_dummy
  if(is.numeric(dissemination_dummy)){
    note("The dissemination dummy variable (", arg$dissemination_dummy, ") is of type numeric. It is automatically coerced to logical.")
    # TODO: Check whether there is only 1/0 values and do it for all dummies (check_dummy() function)
    dissemination_dummy <- as.logical(dissemination_dummy)
  }
  if(!is.logical(dissemination_dummy))
    stop("The dissemination dummy variable (", arg$dissemination_dummy, ") should be of type logical or numeric.")
  if(anyNA(dissemination_dummy))
    stop("The dissemination dummy variable (", arg$dissemination_dummy, ") should not contain any missing (NA) values.")
  
  # dissemination_weight
  if(!is.numeric(dissemination_weight))
    stop("The dissemination weights (", arg$dissemination_weight, ") should be numeric.")
  if(anyNA(dissemination_weight[dissemination_dummy])) stop(
    "The dissemination weights (", arg$dissemination_weight, ") should not contain ", 
    "any missing (NA) values for disseminated units (", arg$dissemination_dummy, ")."
  )
  
  # sampling_weight
  if(!is.numeric(sampling_weight))
    stop("The sampling weights (", arg$sampling_weight, ") should be numeric.")
  if(anyNA(sampling_weight))
    stop("The sampling weights (", arg$sampling_weight, ") should not contain any missing (NA) values.")

  # strata
  if(is.null(strata)) strata <- stats::setNames(factor(rep("1", length(id))), id)
  if(!is.null(strata)){
    if(is.character(strata)){
      note("The strata variable (", arg$strata, ") is of type character. It is automatically coerced to factor.")
      strata <- factor(strata)
    }
    if(!is.factor(strata))
      stop("The strata variable (", arg$strata, ") should be of type factor or character.")
    if(anyNA(strata))
      stop("The strata variable (", arg$strata, ") should not contain any missing (NA) values.")
  }
  
  # scope_dummy
  if(is.null(scope_dummy)) scope_dummy <- rep(TRUE, length(id)) else{
    if(is.numeric(scope_dummy)){
      note("The scope dummy variable (", arg$scope_dummy, ") is of type numeric. It is automatically coerced to logical.")
      scope_dummy <- as.logical(scope_dummy)
    }
    if(!is.logical(scope_dummy))
      stop("The scope dummy variable (", arg$scope_dummy, ") should be of type logical or numeric.")
    if(anyNA(scope_dummy))
      stop("The scope dummy variable (", arg$scope_dummy, ") should not contain any missing (NA) values.")
    disseminated_out_of_scope <- id[dissemination_dummy & !scope_dummy]
    if(length(disseminated_out_of_scope) > 0) stop(
      "The following units are out-of-scope (", arg$scope_dummy, ") but nonetheless disseminated (",
      arg$dissemination_dummy, "): ", display_only_n_first(disseminated_out_of_scope), "."
    )
  }
  
  # response_dummy
  if(is.null(response_dummy)) response_dummy <- scope_dummy else{
    if(is.numeric(response_dummy)){
      note("The response dummy variable (", arg$response_dummy, ") is of type numeric. It is automatically coerced to logical.")
      response_dummy <- as.logical(response_dummy)
    }
    if(!is.logical(response_dummy))
      stop("The response dummy variable (", arg$response_dummy, ") should be of type logical or numeric.")
    if(anyNA(response_dummy))
      stop("The response dummy variable (", arg$response_dummy, ") should not contain any missing (NA) values.")
  }
  
  # nrc_dummy
  if(is.null(nrc_dummy)) nrc_dummy <- scope_dummy else{
    if(is.numeric(nrc_dummy)){
      note("The non-reponse correction dummy variable (", arg$nrc_dummy, ") is of type numeric. It is automatically coerced to logical.")
      nrc_dummy <- as.logical(nrc_dummy)
    }
    if(!is.logical(nrc_dummy))
      stop("The non-reponse correction dummy variable (", arg$nrc_dummy, ") should be of type logical or numeric.")
    if(anyNA(nrc_dummy))
      stop("The non-reponse correction dummy variable (", arg$nrc_dummy, ") should not contain any missing (NA) values.")
  }
  
  # nrc_weight
  if(!is.null(nrc_weight)){
    if(!is.numeric(nrc_weight))
      stop("The weights after non-response correction (", arg$nrc_weight, ") should be numeric.")
    if(anyNA(nrc_weight[response_dummy %in% TRUE & nrc_dummy %in% TRUE])) stop(
      "The weights after non-response correction (", arg$nrc_weight, ") should not contain any missing (NA) values ",
      "for responding units (", arg$response_dummy, ") having taken part in the non-reponse correction process (", arg$nrc_dummy, ")."
    )
    
  }
  

  # calibration_dummy
  if(is.null(calibration_dummy) && !is.null(calibration_weight)) calibration_dummy <- response_dummy
  if(!is.null(calibration_dummy)){
    if(is.numeric(calibration_dummy)){
      note("The dummy variable indicating the units used in the calibation process (", arg$calibration_dummy, ") is of type numeric. It is automatically coerced to logical.")
      calibration_dummy <- as.logical(calibration_dummy)
    }
    if(!is.logical(calibration_dummy))
      stop("The dummy variable indicating the units used in the calibation process (", arg$calibration_dummy, ") should be of type logical or numeric.")
    if(anyNA(calibration_dummy))
      stop("The dummy variable indicating the units used in the calibation process (", arg$calibration_dummy, ") should not contain any missing (NA) values.")
  }

  # calibration_weight
  if(!is.null(calibration_weight)){
    if(!is.numeric(calibration_weight))
      stop("The weights after calibration (", arg$calibration_weight, ") should be numeric.")
    if(anyNA(calibration_weight[calibration_dummy %in% TRUE]))
      stop("The weights after calibration (", arg$calibration_weight, ") should not contain any missing (NA) values for units used in the calibration process.")
  }
  
  # calibration_var
  if(!is.null(calibration_var)){
    calibration_var_quanti <- names(which(sapply(calibration_var, function(var) is.numeric(var) || is.logical(var))))
    calibration_var_quali <- names(which(sapply(calibration_var, function(var) is.factor(var) || is.character(var))))
    calibration_var_pb_type <- setdiff(arg$calibration_var, c(calibration_var_quanti, calibration_var_quali))
    if(length(calibration_var_pb_type) > 0) stop(
      "The following calibration variables are neither quantitative (numeric, logical) nor qualitative (factor, character): ",
      display_only_n_first(calibration_var_pb_type), "."
    )
    if(length(calibration_var_quali) > 0) note(
      "Note: The following calibration variables are qualitative (factor, character): ",
      display_only_n_first(calibration_var_quali), ". They will be automatically discretized."
    )
    calibration_var_pb_NA <- names(which(sapply(calibration_var, function(var) anyNA(var[calibration_dummy %in% TRUE]))))
    if(length(calibration_var_pb_NA) > 0) stop(
      "The following calibration variables contain missing (NA) values for units used in the calibration process: ",
      display_only_n_first(calibration_var_pb_NA, collapse = " "), "."
    )
  }
  
  # Step 4: Define methodological quantities ----
  
  samp_exclude <- stats::setNames(rep(FALSE, length(id)), id)

  # Logical controls
  inconsistency <- list(
    out_of_scope_and_responding = id[!scope_dummy & response_dummy]
  )
  if(any(sapply(inconsistency, length) > 0)) stop(
    "Some arguments are inconsistent:", 
    if(length(inconsistency$out_of_scope_and_responding) > 0) paste0(
      "\n  - the following units are classified both as out-of-scope units (", 
      arg$scope_dummy, " variable) and as responding units (", arg$response_dummy, 
      " variable): ", display_only_n_first(inconsistency$out_of_scope_and_responding), "."
    )
  )

  # Exclude strata with only one sampled unit
  strata_with_one_sampled_unit <- 
    names(which(tapply(id[!samp_exclude], strata[!samp_exclude], length) == 1))
  if(length(strata_with_one_sampled_unit) > 0){
    warn(
      "The following strata contain less than two sampled units: ",
      display_only_n_first(strata_with_one_sampled_unit), ". ",
      "They are excluded from the variance estimation process (but kept for point estimates)."
    )
    samp_exclude <- samp_exclude | as.character(strata) %in% strata_with_one_sampled_unit
  }
  
  # Enforce equal probabilities in each stratum
  sampling_weight_equal <- sampling_weight
  strata_with_unequal_sampling_weight <- 
    names(which(tapply(sampling_weight_equal[!samp_exclude], strata[!samp_exclude], stats::sd) > 1e-6))
  if(length(strata_with_unequal_sampling_weight) > 0){
    # TODO: Enhance warning message when strata = NULL
    warn(
      "The following strata contain units whose sampling weights are not exactly equal: ",
      display_only_n_first(strata_with_unequal_sampling_weight), ". ",
      "The mean weight per stratum is used instead."
    )
    sampling_weight_equal[!samp_exclude] <- 
      tapply(sampling_weight_equal, strata, base::mean)[as.character(strata[!samp_exclude])]
  }
  
  # Reference id and reference weight
  guessed_weight <- sampling_weight
  if(!is.null(nrc_weight)) guessed_weight[response_dummy & nrc_dummy] <- nrc_weight[response_dummy & nrc_dummy]
  if(!is.null(calibration_weight)) guessed_weight[calibration_dummy] <- calibration_weight[calibration_dummy] 
  guessed_weight_not_matching_dissemination_weight <- id[dissemination_dummy & guessed_weight != dissemination_weight]
  if(length(guessed_weight_not_matching_dissemination_weight)) stop(
    "The following units have a disseminated weight (", arg$dissemination_weight, 
    ") that does not match the one guessed from the survey description: ",
    display_only_n_first(guessed_weight_not_matching_dissemination_weight), "."
  )
  reference_id <- id[dissemination_dummy]
  reference_weight <- dissemination_weight[dissemination_dummy]
  
  # Sampling
  samp <- list()
  samp$id <- id
  samp$exclude <- samp_exclude[samp$id]
  samp$weight <- sampling_weight_equal[samp$id]
  samp$strata <- strata[samp$id]
  samp$precalc <- suppressMessages(with(samp, var_srs(
    y = NULL, pik = 1 / weight[!exclude], strata = strata[!exclude]
  )))
  samp <- samp[c("id", "exclude", "precalc")]

  # Non-reponse
  if(!is.null(nrc_weight)){
    nrc <- list()
    nrc$id <- id[response_dummy]
    nrc$sampling_weight <- sampling_weight[nrc$id]
    nrc$response_prob <- (sampling_weight / nrc_weight)[nrc$id]
  }else nrc <- NULL

  # Calibration
  if(!is.null(calibration_weight)){
    calib <- list()
    calib$id <- id[response_dummy & calibration_dummy]
    calib$weight <- calibration_weight[calib$id]
    calib$var <- calibration_var[calib$id, , drop = FALSE]
    calib$var[calibration_var_quanti] <- 
      lapply(calib$var[calibration_var_quanti], Matrix)
    calib$var[calibration_var_quali] <- 
      lapply(calib$var[calibration_var_quali], discretize_qualitative_var)
    calib$var <- do.call(cbind, calib$var)
    # TODO: Handle the node stack overflow problem
    calib$precalc <- res_cal(y = NULL, x = calib$var, w = calib$weight)
    calib <- calib[c("id", "precalc")]
  }else calib <- NULL
  
  
  
  # Step 5: Define the variance wrapper ----
  qvar_wrapper <- define_variance_wrapper(
    variance_function = var_simple,
    reference_id = reference_id,
    reference_weight = reference_weight,
    default_id = arg$id,
    technical_data = list(samp = samp, nrc = nrc, calib = calib)
  )
  
  qvar_wrapper

}


var_simple <- function(y, samp, nrc, calib){
  
  var <- list()
  
  # Calibration    
  if(!is.null(calib)){
    y <- add_zero(y, calib$id, remove = FALSE)
    y[calib$id, ] <- res_cal(y = y[calib$id, ], precalc = calib$precalc)
  } 

  # Non-response
  if(!is.null(nrc)){
    y <- y[nrc$id, , drop = FALSE]
    var[["nr"]] <- var_pois(y = y, pik = nrc$response_prob, w = nrc$sampling_weight)
    y <- y / nrc$response_prob
  }

  # Sampling
  y <- add_zero(y, rownames = samp$id)
  var[["sampling"]] <- var_srs(y = y[!samp$exclude, , drop = FALSE], precalc = samp$precalc)

  # Final summation
  Reduce(`+`, var)
  
}

  

