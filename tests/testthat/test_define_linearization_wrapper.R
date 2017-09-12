

context("test_define_linearization_wrapper")

source("data.R")

linearization_wrapper_test <- define_linearization_wrapper(
  linearization_function = function(Y, weight, Y_thresh = NULL, weight_thresh = NULL, zero = FALSE){
    
    est <- if(!is.null(Y_thresh)) sum(Y_thresh * weight_thresh) else sum(Y * weight)
    est <- if(zero) rep(0, length(est)) else est
    lin <- Y / est
    return(list(lin = list(lin), metadata = list(est = est)))
  }
  , arg_type = list(data = c("Y", "Y_thresh") , weight = c("weight", "weight_thresh"), param = "zero")
  , arg_not_affected_by_domain = c("Y_thresh", "weight_thresh")
  , allow_factor = TRUE
)

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y, eurostat = FALSE){
    return(if(!eurostat) abs(colSums(y)) else 0)
  }
  , reference_id = ref$idref
  , default = list(id = "id3", weight = "w3")
  , objects_to_include = "linearization_wrapper_test"
)

expect_warning(
  variance_wrapper(survey, by = quali, linearization_wrapper_test(Y = quanti, Y_thresh = quanti))
  , regexp = NA
)