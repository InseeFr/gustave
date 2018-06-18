


#' Define a simple variance wrapper
#'
#' @description define_simple_wrapper is a convenience wrapper
#' for define_variance_wrapper for the simplest yet frequent cases: 
#' \itemize{\item (stratified) simple random sampling \item non-response
#' correction (if any) through reweighting in Homogeneous response groups 
#' (HRG) \item calibration (if any)}
#' It performs technical and statistical checks and (eventually) produces
#' a ready-to-use variance wrapper adapted for such cases.
#'
#' @export 
define_simple_wrapper <-function(){
  
}

# Unexported (and undocumented) function
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

