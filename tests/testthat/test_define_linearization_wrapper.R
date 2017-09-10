

context("test_define_linearization_wrapper")

source("data.R")

linearization_wrapper_test <- define_linearization_wrapper(
  linearization_function = function(Y, weight, Y_thresh = NULL, weight_thresh = NULL){
    est <- if(!is.null(Y_thresh)) sum(Y_thresh * weight_thresh) else sum(Y * weight)
    lin <- Y / est
    return(list(lin = list(lin), metadata = list(est = est)))
  }
  , arg_type = list(data = c("Y", "Y_thresh") , weight = c("weight", "weight_thresh"), param = NULL)
  , no_domain_splitting = c("Y_thresh", "weight_thresh")
  , allow_factor = TRUE
)

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y, eurostat = FALSE){
    return(if(!eurostat) abs(colSums(y)) else 0)
  }
  , default_id = "id3"
  , reference_id = ref$idref, reference_weight = ref$wref
  , objects_to_include = "linearization_wrapper_test"
)

expect_warning(
  variance_wrapper(survey, linearization_wrapper_test(Y = quanti, Y_thresh = quanti, by = quali))
  , regexp = NA
)

variance_wrapper(survey, linearization_wrapper_test(Y = quanti, by = quali))
variance_wrapper(survey, linearization_wrapper_test(Y = quanti, Y_thresh = quanti, by = quali))
