


#' Define a simple variance wrapper
#'
#' @description define_simple_wrapper is a convenience wrapper
#' for \code{define_variance_wrapper} for the simplest yet frequent cases: 
#' \itemize{\item (stratified) simple random sampling \item non-response
#' correction (if any) through reweighting \item calibration (if any)}
#' It performs technical and statistical checks and produces a 
#' ready-to-use variance wrapper adapted for such cases.
#'
#' @param data The \code{data.frame} containing all the technical information
#'   required to prepare the variance estimation process (see other arguments 
#'   below). Note that this file should contain all the units sampled, 
#'   including the out-of-scope and non-responding units.
#' @param id The identification variable of the units in \code{data}. 
#'   It should be unique for each row in \code{data} and not contain any 
#'   missing values.
#'
#' @param sampling_weight A character vector of length 1, the name of the 
#'   numeric variable in \code{data} corresponding to the sampling weights 
#'   of the survey. It should not contain any missing values.
#' @param strata A character vector of length 1, the name of the factor 
#'   variable in \code{data} whose level match the stratification
#'   used in the survey. Character variables are coerced to factor.
#'   If defined, it should not contain any missing value. If \code{NULL},
#'   the variance estimation process does not take any stratification
#'   into account.
#'
#' @param scope A character vector of length 1, the name of the logical 
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
#' @param resp A character vector of length 1, the name of of the logical 
#'   variable in \code{data} indicating whether the unit is a responding 
#'   unit or not. Numerical variables are coerced to logical. \code{resp}
#'   should be defined as long as a \code{nrc_weight} is provided. All units 
#'   in the scope of the survey should have a non-missing value.
#'
#' @param calib_weight A character vector of length 1, the name of the 
#'   numerical variable in \code{data} corresponding to the calibrated
#'   weights. If defined, all responding units (see above) should have 
#'   a non-missing value. If \code{NULL}, the variance estimation
#'   process does not take any calibration step into account.
#' @param calib A character vector of length 1, the name of of the logical 
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
#' @param force (Advanced use) A logical vector of lentgh one: should all
#'   error messages be considered as warnings? Use at your own risks.
#' 
#' 
#' @export 
define_simple_wrapper <- function(data, id,
                                  sampling_weight, strata = NULL,
                                  scope = NULL,
                                  nrc_weight = NULL, resp = NULL,
                                  calib_weight = NULL, calib = NULL, calib_var = NULL,
                                  force = FALSE
){
  
  # Step 0: Define how error are handled depending on the force parameter
  error <- if(!force){
    function(...) stop(..., call. = FALSE)
  }else{
    function(...) warning(..., call. = FALSE, immediate. = TRUE)
  }
  
  # Step 1: Control arguments consistency and display the welcome message
  
  # Step 1.1: Arguments consistency
  if(missing(data)) error("A data file must be provided (data argument).")
  if(missing(id)) error("An identifier of the units must be provided (id argument).")
  if(missing(sampling_weight)) error("A sampling weight must be provided (sampling_weight argument).")
  inconsistency <- list(
    nrc_weight_but_no_resp = !is.null(nrc_weight) && is.null(resp),
    resp_but_no_nrc_weight = is.null(nrc_weight) && !is.null(resp),
    calib_weight_but_no_calib_var = !is.null(calib_weight) && is.null(calib_var),
    calib_or_calib_var_but_no_calib_weight = is.null(calib_weight) && (!is.null(calib) || !is.null(calib_var))
  )
  if(any(unlist(inconsistency))) error(
    "Some arguments are inconsistent:", 
    if(inconsistency$nrc_weight_but_no_resp) "\n  - weights after non-response correction are provided (nrc_weight argument) but no variable indicating responding units (resp argument)" else "", 
    if(inconsistency$resp_but_no_nrc_weight) "\n  - a variable indicating responding units is provided (resp argument) but no weights after non-response correction (nrc_weight argument)" else "" ,
    if(inconsistency$calib_weight_but_no_calib_var) "\n  - calibrated weights are provided (calib_weight argument) but no calibration variables (calib_var argument)" else "" ,
    if(inconsistency$calib_or_calib_var_but_no_calib_weight) "\n  - a variable indicating the units taking part in a calibration process and/or calibration variables are provided (calib and calib_var arguments) but no calibrated weights (calib_weight argument)" else "" 
  )

  # Step 1.2: Welcome message
  message(
    "Variance wrapper definition using the dataset : ", deparse(substitute(data)),
    "\n\nThe following features are taken into account:",
    if(!is.null(strata)) "\n  - stratified simple random sampling" else 
      "\n  - simple random sampling WITHOUT stratification",
    if(!is.null(scope)) "\n  - out-of-scope units" else "",
    if(!is.null(nrc_weight)) "\n  - non-response correction through reweighting" else "",
    if(!is.null(calib_weight)) "\n  - calibration on margins" else "",
    "\n"
  )
  
  # Step 2: Control that arguments exist

  # Step 2.0: Evaluation of all arguments
  if(!is.data.frame(data)) error("data argument must refer to a data.frame")
  arg <- lapply(as.list(match.call())[-1], eval)
  
  # Step 2.1: Expected types
  should_be_single_variable_name <- c(
    "id", "sampling_weight", "strata", "scope", "nrc_weight", 
    "resp", "calib_weight", "calib"
  )
  should_be_variable_name_vector <- c("calib_var")
  should_be_variable_name <- c(should_be_single_variable_name, should_be_variable_name_vector)
  
  # Step 2.2: Types
  is_single_variable_name <- sapply(
    arg[should_be_single_variable_name], 
    function(arg) is.null(arg) || is_variable_name(arg, max_length = 1)
  )
  if(any(!is_single_variable_name)) error(
    "The following arguments do not refer to a variable name (character vector of length 1): ", 
    names(is_single_variable_name)[!is_single_variable_name]
  )
  is_variable_name_vector <- sapply(
    arg[should_be_variable_name_vector], 
    function(arg) is.null(arg) || is_variable_name(arg, max_length = Inf)
  )
  if(any(!is_variable_name_vector)) error(
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
  if(length(unlist(is_not_in_data)) > 0) error(
    "Some variables do not exist in ", deparse(substitute(data)), ": ",
    unlist(is_not_in_data[!is.null(is_not_in_data)])
  )
  
  
  
}

# Unexported (and undocumented) function
# TODO: use precalculated data in var_simple
var_simple <- function(y, samp, nr, calib){
  
  var <- list()
  
  # Calibration    
  if(!is.null(calib)) y <- rescal(y = y, x = calib$x, w = calib$w)

  # Non-response
  if(!is.null(nr)){
    y <- add0(y, rownames = nr$id)
    var[["nr"]] <- var_pois(y = y, pik = nr$response_prob, w = nr$w_sample)
    y <- y / nr$response_prob
  }

  # Sampling
  var[["sampling"]] <- var_srs(y = y, pik = 1 / samp$w_sample, strata = samp$strata)

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
