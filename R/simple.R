

#' Perform a variance estimation in most common cases
#' 
#' @description \code{everest} is a ready-to-use function performing
#' analytical variance estimation in most common cases, that is: 
#' \itemize{\item (stratified) simple random sampling \item non-response
#' correction (if any) through reweighting \item calibration (if any)}
#' 
#' \code{define_simple_wrapper} (or \code{everest} with \code{define = TRUE})
#' defines a so-called variance wrapper, that is a standalone function that
#' can be applied to the survey dataset in order to perform the variance estimation
#' without having to specify the methodological characteristics of the survey.
#'
#' @param data The \code{data.frame} containing all the technical information
#'   required to prepare the variance estimation process (see other arguments 
#'   below). Note that this file should contain all the units sampled, 
#'   including the out-of-scope and non-responding units. For the \code{everest}
#'   function, it should also contain the variables of interest (the variables
#'   to perform the variance estimation on).
#' @param ... One or more calls to a linearization wrapper (e.g. \code{total()}, 
#'   \code{mean()}, \code{ratio()}). See examples and 
#'   \code{\link[=linearization_wrapper_standard]{standard linearization wrappers}})
#' @param where A logical vector indicating a domain on which the variance 
#'   estimation is to be performed.
#' @param by A qualitative variable whose levels are used to define domains
#'   on which the variance estimation is performed.
#' @param alpha A numeric vector of length 1 indicating the threshold
#'   for confidence interval derivation (\code{0.05} by default).
#' @param display A logical verctor of length 1 indicating whether
#'   the result of the estimation should be displayed or not.
#' @param envir An environment containing a binding to \code{data}.
#'   
#' @param id The identification variable of the units in \code{data}. 
#'   It should be unique for each row in \code{data} and not contain any 
#'   missing values.
#'
#' @param samp_weight A character vector of length 1, the name of the 
#'   numeric variable in \code{data} corresponding to the sampling weights 
#'   of the survey. It should not contain any missing values.
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
#' @param resp_dummy A character vector of length 1, the name of of the logical 
#'   variable in \code{data} indicating whether the unit is a responding 
#'   unit or not. Numerical variables are coerced to logical. \code{resp_dummy}
#'   should be defined as long as a \code{nrc_weight} is provided. All units 
#'   in the scope of the survey should have a non-missing value.
#'
#' @param calib_weight A character vector of length 1, the name of the 
#'   numerical variable in \code{data} corresponding to the calibrated
#'   weights. If defined, all responding units (see above) should have 
#'   a non-missing value. If \code{NULL}, the variance estimation
#'   process does not take any calibration step into account.
#' @param calib_dummy A character vector of length 1, the name of of the logical 
#'   variable in \code{data} indicating whether the unit did take part
#'   in the calibration process or not. Numerical variables are coerced to 
#'   logical. If defined, all responding units should have a non-missing
#'   value. If \code{NULL}, calibration is supposed to have been performed
#'   on all responding units.
#' @param calib_var A character vector, the name of the variable(s) used in
#'   the calibration process. Logical variables are coerced to numeric. 
#'   Character and factor variables are automatically discretized. 
#'   \code{calib_var} should be defined as long as a \code{calib_weight} is 
#'   provided. All units taking part in the calibration process should have
#'   only non-missing values for all variables in \code{calib_var}.
#' 
#' @param force Logical vector of lentgh 1. Should all errors be considered 
#'   as warnings? Use at your own risks.
#' @param define Logical vector of lentgh 1. Should a variance wrapper
#'   be defined instead of performing a variance estimation?
#' 
#' 
#' @name everest
#' @aliases everest define_simple_wrapper

NULL

#' @rdname everest
#' @export
everest <- function(data, ..., by = NULL, where = NULL, id = NULL, 
                    alpha = 0.05, display = TRUE, envir = parent.frame(),
                    samp_weight, strata = NULL,
                    scope_dummy = NULL,
                    nrc_weight = NULL, resp_dummy = NULL,
                    calib_weight = NULL, calib_dummy = NULL, calib_var = NULL,
                    force = FALSE, define = FALSE
){
  
  call <- as.list(match.call())[-1]
  everest_wrapper <- do.call(
    define_simple_wrapper, 
    call[names(call) %in% names(formals(define_simple_wrapper))]
  )
  if(define) everest_wrapper else{
    everest_data <- data[data[, id] %in% environment(everest_wrapper)$reference_id, ]  
    call$data <- substitute(everest_data)
    call$envir <- environment()
    do.call(
      everest_wrapper,
      call[names(call) == "" | names(call) %in% names(formals(everest_wrapper))]
    )
  }
  
}

#' @rdname everest
#' @export 

define_simple_wrapper <- function(data, id,
                                  samp_weight, strata = NULL,
                                  scope_dummy = NULL,
                                  nrc_weight = NULL, resp_dummy = NULL,
                                  calib_weight = NULL, calib_dummy = NULL, calib_var = NULL,
                                  force = FALSE
){
  
  # TODO: Use more consistent functions to display msg, warn and errors
  # in the whole package
  
  # Step 0: Define how stop_ are handled depending on the force parameter ----
  if(!is.logical(force) || length(force) != 1)
    stop("The force argument should be a logical vector of length 1.")
  warn_ <- function(...) warning(..., call. = FALSE, immediate. = TRUE)
  stop_ <- if(!force) function(...) stop(..., call. = FALSE) else warn_
  

  # Step 1: Control arguments consistency and display the welcome message ----
  
  # Step 1.1: Arguments consistency
  if(missing(data)) stop_("A data file must be provided (data argument).")
  if(missing(id)) stop_("An identifier of the units must be provided (id argument).")
  if(missing(samp_weight)) stop_("Sampling weights must be provided (samp_weight argument).")
  inconsistency <- list(
    nrc_weight_but_no_resp = !is.null(nrc_weight) && is.null(resp_dummy),
    resp_but_no_nrc_weight = is.null(nrc_weight) && !is.null(resp_dummy),
    calib_weight_but_no_calib_var = !is.null(calib_weight) && is.null(calib_var),
    calib_or_calib_var_but_no_calib_weight = is.null(calib_weight) && (!is.null(calib_dummy) || !is.null(calib_var))
  )
  if(any(unlist(inconsistency))) stop_(
    "Some arguments are inconsistent:", 
    if(inconsistency$nrc_weight_but_no_resp) "\n  - weights after non-response correction are provided (nrc_weight argument) but no variable indicating responding units (resp_dummy argument)" else "", 
    if(inconsistency$resp_but_no_nrc_weight) "\n  - a variable indicating responding units is provided (resp_dummy argument) but no weights after non-response correction (nrc_weight argument)." else "" ,
    # TODO: better handle the case of a user who has no non-response but 
    # uses the resp_dummy argument with a logical variable with only TRUE values.
    if(inconsistency$calib_weight_but_no_calib_var) "\n  - calibrated weights are provided (calib_weight argument) but no calibration variables (calib_var argument)" else "" ,
    if(inconsistency$calib_or_calib_var_but_no_calib_weight) "\n  - a variable indicating the units taking part in a calibration process and/or calibration variables are provided (calib_dummy and calib_var arguments) but no calibrated weights (calib_weight argument)" else "" 
  )

  # Step 1.2: Welcome message
  message(
    "Survey variance estimation with the gustave package",
    "\n\nThe following features are taken into account:",
    if(!is.null(strata)) "\n  - stratified simple random sampling" else 
      "\n  - simple random sampling WITHOUT stratification",
    if(!is.null(scope_dummy)) "\n  - out-of-scope units" else "",
    if(!is.null(nrc_weight)) "\n  - non-response correction through reweighting" else "",
    if(!is.null(calib_weight)) "\n  - calibration on margins" else "",
    "\n"
  )
  
  # Step 2: Control that arguments do exist and retrive their value ----

  # Step 2.1: Evaluation of all arguments
  if(!is.data.frame(data)) stop_("data argument must refer to a data.frame")
  arg <- lapply(as.list(match.call())[-1], eval)

  # Step 2.2: Expected types
  should_be_single_variable_name <- intersect(c(
    "id", "samp_weight", "strata", "scope_dummy", "nrc_weight", 
    "resp_dummy", "calib_weight", "calib_dummy"
  ), names(arg))
  should_be_variable_name_vector <- intersect(c("calib_var"), names(arg))
  should_be_variable_name <- c(should_be_single_variable_name, should_be_variable_name_vector)

  # Step 2.2: Types
  is_single_variable_name <- sapply(
    arg[should_be_single_variable_name], 
    function(arg) is.null(arg) || is_variable_name(arg, max_length = 1)
  )
  if(any(!is_single_variable_name)) stop_(
    "The following arguments do not refer to a variable name (character vector of length 1): ", 
    names(is_single_variable_name)[!is_single_variable_name]
  )
  is_variable_name_vector <- sapply(
    arg[should_be_variable_name_vector], 
    function(arg) is.null(arg) || is_variable_name(arg, max_length = Inf)
  )
  if(any(!is_variable_name_vector)) stop_(
    "The following arguments do not refer to a vector of variable names: ", 
    names(is_variable_name_vector)[!is_variable_name_vector]
  )

  # Step 2.3: variables in data
  is_not_in_data <- lapply(should_be_variable_name, function(param){
    # param <- "id"
    tmp <- variable_not_in_data(var = arg[[param]], data = data)
    if(is.null(tmp)) return(NULL)
    paste0("\n  - ", param, " argument: ", paste0(tmp, collapse = " "))
  })
  if(length(unlist(is_not_in_data)) > 0) stop_(
    "Some variables do not exist in ", deparse(substitute(data)), ": ",
    unlist(is_not_in_data[!is.null(is_not_in_data)])
  )
  
  # Step 2.4: Retrieve the value of the arguments
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
  
  # Note: some useful variable are created or normalized: 
  # resp_dummy, calib_dummy (if !is.null(calib_weight)), reference_weight

  # id
  if(anyNA(id))
    stop_("The id variable (", arg$id, ") should not contain any missing (NA) values.")
  if(any(duplicated(id)))
    stop_("The id variable (", arg$id, ") should not contain any duplicated values.")
  
  # samp_weight
  if(!is.numeric(samp_weight))
    stop_("The sampling weights (", arg$samp_weight, ") should be numeric.")
  if(anyNA(samp_weight))
    stop_("The sampling weights (", arg$samp_weight, ") should not contain any missing (NA) values.")
  reference_weight <- samp_weight
  reference_weight_name <- arg$samp_weight

  # strata
  if(is.null(strata)) strata <- stats::setNames(factor(rep("1", length(id))), id)
  if(!is.null(strata)){
    if(is.character(strata)){
      message("Note: The strata variable (", arg$strata, ") is of type character. It is automatically coerced to factor.\n")
      strata <- factor(strata)
    }
    if(!is.factor(strata))
      stop_("The strata variable (", arg$strata, ") should be of type factor or character.")
    if(anyNA(strata))
      stop_("The strata variable (", arg$strata, ") should not contain any missing (NA) values.")
  }
  
  # scope_dummy
  if(is.null(scope_dummy)) scope_dummy <- rep(TRUE, length(id)) else{
    if(is.numeric(scope_dummy)){
      message("Note: The scope dummy variable (", arg$scope_dummy, ") is of type numeric. It is automatically coerced to logical.\n")
      scope_dummy <- as.logical(scope_dummy)
    }
    if(!is.logical(scope_dummy))
      stop_("The scope dummy variable (", arg$scope_dummy, ") should be of type logical or numeric.")
    if(anyNA(scope_dummy))
      stop_("The scope dummy variable (", arg$scope_dummy, ") should not contain any missing (NA) values.")
  }
  
  # resp_dummy
  if(is.null(resp_dummy)) resp_dummy <- scope_dummy else{
    if(is.numeric(resp_dummy)){
      message("Note: The response dummy variable (", arg$resp_dummy, ") is of type numeric. It is automatically coerced to logical.\n")
      resp_dummy <- as.logical(resp_dummy)
    }
    if(!is.logical(resp_dummy))
      stop_("The response dummy variable (", arg$resp_dummy, ") should be of type logical or numeric.")
    if(anyNA(resp_dummy))
      stop_("The response dummy variable (", arg$resp_dummy, ") should not contain any missing (NA) values.")
  }
    
  # nrc_weight
  if(!is.null(nrc_weight)){
    if(!is.numeric(nrc_weight))
      stop_("The weights after non-response correction (", arg$nrc_weight, ") should be numeric.")
    if(anyNA(nrc_weight[resp_dummy %in% TRUE]))
      stop_("The weights after non-response correction (", arg$nrc_weight, ") should not contain any missing (NA) values for responding units.")
  }
  
  # calib_dummy
  if(is.null(calib_dummy) && !is.null(calib_weight)) calib_dummy <- resp_dummy
  if(!is.null(calib_dummy)){
    if(is.numeric(calib_dummy)){
      message("Note: The dummy variable indicating the units used in the calibation process (", arg$calib_dummy, ") is of type numeric. It is automatically coerced to logical.\n")
      calib_dummy <- as.logical(calib_dummy)
    }
    if(!is.logical(calib_dummy))
      stop_("The dummy variable indicating the units used in the calibation process (", arg$calib_dummy, ") should be of type logical or numeric.")
    if(anyNA(calib_dummy[resp_dummy %in% TRUE]))
      stop_("The dummy variable indicating the units used in the calibation process (", arg$calib_dummy, ") should not contain any missing (NA) values for responding units.")
  }

  # calib_weight
  if(!is.null(calib_weight)){
    if(!is.numeric(calib_weight))
      stop_("The weights after calibration (", arg$calib_weight, ") should be numeric.")
    if(anyNA(calib_weight[calib_dummy %in% TRUE]))
      stop_("The weights after calibration (", arg$calib_weight, ") should not contain any missing (NA) values for units used in the calibration process.")
    if(is.null(nrc_weight) && any((samp_weight != calib_weight)[calib_dummy %in% FALSE])) stop_(
      "For the responding units not used in the calibration process, the weights after calibration (", 
      arg$calib_weight, ") should exactly match the sampling weights (", arg$samp_weight, ")."
    ) else if(!is.null(nrc_weight) && any((nrc_weight != calib_weight)[resp_dummy %in% TRUE & calib_dummy %in% FALSE])) stop_(
      "For the responding units not used in the calibration process, the weights after calibration (", 
      arg$calib_weight, ") should exactly match the weights after non-response correction (", arg$samp_weight, ")."
      
    )
  }
  
  # calib_var
  if(!is.null(calib_var)){
    calib_var_quanti <- names(which(sapply(calib_var, function(var) is.numeric(var) || is.logical(var))))
    calib_var_quali <- names(which(sapply(calib_var, function(var) is.factor(var) || is.character(var))))
    calib_var_pb_type <- setdiff(arg$calib_var, c(calib_var_quanti, calib_var_quali))
    if(length(calib_var_pb_type) > 0) stop_(
      "The following calibration variables are neither quantitative (numeric, logical) nor qualitative (factor, character): ",
      display_only_n_first(calib_var_pb_type), "."
    )
    if(length(calib_var_quali) > 0) message(
      "Note: The following calibration variables are qualitative (factor, character): ",
      display_only_n_first(calib_var_quali), ". They will be automatically discretized."
    )
    calib_var_pb_NA <- names(which(sapply(calib_var, function(var) anyNA(var[calib_dummy %in% TRUE]))))
    if(length(calib_var_pb_NA) > 0) stop_(
      "The following calibration variables contain missing (NA) values for units used in the calibration process: ",
      display_only_n_first(calib_var_pb_NA, collapse = " "), "."
    )
  }
  
  # Step 4: Define methodological quantities ----
  
  samp_exclude <- stats::setNames(rep(FALSE, length(id)), id)

  # Logical controls
  out_of_scope_but_responding <- id[!scope_dummy & resp_dummy]
  if(length(out_of_scope_but_responding) > 0) stop_(
    "The following units are classified both as out-of-scope units (", arg$scope_dummy, " variable) ", 
    "and as responding units (", arg$resp_dummy, " variable): ",
    display_only_n_first(out_of_scope_but_responding), "."
  )

  
  
  # Exclude strata with only one sampled unit
  strata_with_one_sampled_unit <- 
    names(which(tapply(id[!samp_exclude], strata[!samp_exclude], length) == 1))
  if(length(strata_with_one_sampled_unit) > 0){
    warn_(
      "The following strata contain less than two sampled units: ",
      display_only_n_first(strata_with_one_sampled_unit), ". ",
      "They are excluded from the variance estimation process (but kept for point estimates)."
    )
    samp_exclude <- samp_exclude | as.character(strata) %in% strata_with_one_sampled_unit
  }
  
  # Enforce equal probabilities in each stratum
  strata_with_unequal_samp_weight <- 
    names(which(tapply(samp_weight[!samp_exclude], strata[!samp_exclude], stats::sd) > 1e-6))
  if(length(strata_with_unequal_samp_weight) > 0){
    # TODO: Enhance warning message when strata = NULL
    warn_(
      "The following strata contain units whose sampling weights are not exactly equal: ",
      display_only_n_first(strata_with_unequal_samp_weight), ". ",
      "The mean weight per stratum is used instead."
    )
    samp_weight[!samp_exclude] <- 
      tapply(samp_weight, strata, base::mean)[as.character(strata[!samp_exclude])]
  }


  
  
  
  # Sampling
  samp <- list()
  samp$id <- id
  samp$exclude <- samp_exclude[samp$id]
  samp$weight <- samp_weight[samp$id]
  samp$strata <- strata[samp$id]
  samp$precalc <- with(samp, var_srs(y = NULL, pik = 1 / weight[!exclude], strata = strata[!exclude]))
  samp <- samp[c("id", "exclude", "precalc")]

  # Non-reponse
  if(!is.null(nrc_weight)){
    nrc <- list()
    nrc$id <- id[resp_dummy]
    nrc$samp_weight <- samp_weight[nrc$id]
    nrc$response_prob <- (samp_weight / nrc_weight)[nrc$id]
  }else nrc <- NULL

  # Calibration
  if(!is.null(calib_weight)){
    calib <- list()
    calib$id <- id[resp_dummy & calib_dummy]
    calib$weight <- calib_weight[calib$id]
    calib$var <- calib_var[calib$id, , drop = FALSE]
    calib$var[calib_var_quanti] <- 
      lapply(calib$var[calib_var_quanti], Matrix)
    calib$var[calib_var_quali] <- 
      lapply(calib$var[calib_var_quali], discretize_qualitative_var)
    # TODO: Handle the node stack overflow limitation
    calib$var <- do.call(cbind, calib$var)
    calib$precalc <- rescal(y = NULL, x = calib$var, w = calib$weight)
    calib <- calib[c("id", "precalc")]
  }else calib <- NULL
  
  # Reference id and reference weight
  reference_id <- id[resp_dummy]
  reference_weight <- if(!is.null(calib_weight)){
    calib_weight[resp_dummy %in% TRUE]
  }else if(!is.null(nrc_weight)){
    nrc_weight[resp_dummy %in% TRUE]
  }else samp_weight

  # TODO: Add non-reweighted units (units that did not take part
  # in any reweighting but that are kept for the point estimates)
  # Should they take part in the variance estimation (are they
  # exhaustive or not ?)
  
  
  # Step 5: Define the variance wrapper ----
  simple_wrapper <- define_variance_wrapper(
    variance_function = var_simple,
    reference_id = reference_id,
    reference_weight = reference_weight,
    default_id = arg$id,
    technical_data = list(samp = samp, nrc = nrc, calib = calib)
  )
  
  simple_wrapper

}





# Unexported (and undocumented) functions
var_simple <- function(y, samp, nrc, calib){
  
  var <- list()
  
  # Calibration    
  if(!is.null(calib)){
    y[calib$id, ] <- 
      rescal(y = y[calib$id, , drop = FALSE], precalc = calib$precalc)
  } 

  # Non-response
  if(!is.null(nrc)){
    y <- y[nrc$id, , drop = FALSE]
    var[["nr"]] <- var_pois(y = y, pik = nrc$response_prob, w = nrc$samp_weight)
    y <- y / nrc$response_prob
  }

  # Sampling
  y <- add0(y, rownames = samp$id)
  var[["sampling"]] <- var_srs(y = y[!samp$exclude, , drop = FALSE], precalc = samp$precalc)

  # Final summation
  Reduce(`+`, var)
  
}

is_variable_name <- function(param, max_length = 1)
  is.character(param) && length(param) > 0 && length(param) <= max_length

variable_not_in_data <- function(var, data){
  if(is.null(var)) return(NULL)
  tmp <- var[!(var %in% names(data))]
  if(length(tmp) == 0) return(NULL)
  tmp
} 

display_only_n_first <- function(x, 
                                 n = 10, 
                                 collapse = ", ", 
                                 text = paste0(" and ", length(x) - n, " more")
){
  if(length(x) <= n){
    paste(x, collapse = collapse)
  }else{
    paste0(paste(x[1:n], collapse = collapse), text)
  }
}
  
discretize_qualitative_var <- function(var, sparse = TRUE){
  workhorse <- if(sparse) Matrix::sparse.model.matrix else stats::model.matrix
  workhorse(~ droplevels(as.factor(var)) - 1)
}
