

context("define_variance_wrapper - Function and data defined in globalenv()")

source("data.R")

test_that("variance_wrapper can be defined", {
  expect_error(
    variance_wrapper <<- define_variance_wrapper(
      variance_function = function(y, eurostat = FALSE){
        return(if(!eurostat) abs(colSums(y)) else 0)
      }
      , reference_id = ref$idref
      , default = list(id = "id3", weight = "w3", stat = "mean")
    )
    , regexp = NA)
})

test_that("variance_wrapper works", {
  expect_error(
    variance_wrapper(survey, quanti, by = bynoNA)
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
    , reference_id = ref$idref
    , default = list(id = "id3", weight = "w3")
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


