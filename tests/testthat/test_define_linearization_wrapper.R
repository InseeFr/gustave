

context("test_define_linearization_wrapper")

source("data.R")

linearization_wrapper_test <- define_linearization_wrapper(
  linearization_function = function(y, w_blabla, w2, zero = FALSE){
    # spy <<- w2; stop()
    if(zero){
      lin <- rep(0, length(y))
      est <- 0
     }else{
       lin <- y
       est <- sum(y * w2)
     }
    return(list(lin = list(lin), metadata = list(est = est)))
  }
  , arg_type = list(data = "y" , weight = c("w_blabla", "w2"), param = "zero")
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
  variance_wrapper(survey, linearization_wrapper_test(quanti, zero = FALSE, by = quali))
  , regexp = NA
  
)

variance_wrapper(survey, quanti, by = quali)
