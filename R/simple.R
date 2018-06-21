


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
#' 
#' 
#' 
#' @export 
define_simple_wrapper <-function(data,
                                 sampling_weight, strata = NULL,
                                 scope = NULL,
                                 nrc_weight = NULL, resp = NULL,
                                 calib_weight = NULL, calib = NULL, calib_var = NULL
){
  
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

