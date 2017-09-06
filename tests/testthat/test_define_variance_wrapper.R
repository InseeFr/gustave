

context("define_variance_wrapper - Function and data defined in globalenv()")

source("data.R")

variance_function_test <- function(y, eurostat = FALSE){
  return(if(!eurostat) abs(colSums(y)) else 0)
}

test_that("varwrap_test can be defined", {
  expect_error(
    varwrap_test <<- define_variance_wrapper(
      variance_function = variance_function_test, default_id = substitute(id3)
      , reference_id = ref$idref, reference_weight = ref$wref
    )
    , regexp = NA)
})

# ls(environment(varwrap_test))
# varwrap_test(data = survey, total(quali))

test_that("varwrap_test works", {
  expect_error(
    varwrap_test(survey, quanti, by = bynoNA)
    , regexp = NA)
})

context("define_variance_wrapper - Function and data defined in another function")

prepare_test <- function(){

  source("data.R")
  a <- 1

  variance_function_test <- function(y, eurostat = FALSE){
    list(var = a + abs(colSums(y)))
  }
  define_variance_wrapper(
    variance_function = variance_function_test
    , default_id = substitute(id3), default_stat = "mean"
    , reference_id = ref$idref, reference_weight = ref$wref
    , objects_to_include = c("a", "ref")
  )
}

test_that("varwrap_test can be defined", {
  expect_error(
    varwrap_test <<- prepare_test()
    , regexp = NA)
})

test_that("varwrap_test works", {
  expect_error(
    varwrap_test(survey, quanti)
    , regexp = NA)
})


