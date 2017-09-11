
context("linearization_wrapper")

source("data.R")

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y, eurostat = FALSE){
    return(if(!eurostat) abs(colSums(y)) else 0)
  }
  , reference_id = ref$idref
  , default = list(id = "id3", weight = "w3")
)

variance_wrapper(survey, arpr(quanti))
variance_wrapper(survey, arpr(quanti, by = quali))
variance_wrapper(survey, gini(quanti), arpr(quanti), by = quali)