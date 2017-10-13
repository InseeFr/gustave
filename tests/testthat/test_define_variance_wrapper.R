

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

# variance_wrapper(survey, quali)
# 
# library(microbenchmark)
# microbenchmark(
#   t(Matrix::fac2sparse(d[[1]]$data[[1]], giveCsparse = FALSE))  
#   , model.matrix(~ . -1, model.frame(~ ., d[[1]]$data[[1]]))
# )

test_that("variance_wrapper works", {
  expect_error(
    variance_wrapper(survey, quanti, by = bynoNA)
    , regexp = NA)
})

test_that("variance_wrapper works also when NULL variable are used", {
  expect_error(
    variance_wrapper(survey, quanti, by = byNA)
    , regexp = NA)
})

test_that("point estimates work for qualitative variables", {
  expect_equal(
    variance_wrapper(survey, total(quali))$est
    , as.vector(tapply(survey$w3, survey$quali, sum))
  )
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
    varwrap_test(survey, quanti, quali)
    , regexp = NA)
})


context("define_variance_wrapper - Comparison to reference values")

test_that("the estimated values are consistent with reference values", {
  expect_equal(variance_wrapper(survey, total(quanti))$est, -8316.08, tolerance = 1e-2)
  expect_equal(variance_wrapper(survey, total(quanti))$variance, 6.242553, tolerance = 1e-6)
  expect_equal(variance_wrapper(survey, total(quali))$est, c(449650, 450790, 449624), tolerance = 1e-0)
  expect_equal(variance_wrapper(survey, total(quali))$variance, c(333, 334, 333), tolerance = 1e-0)
  expect_equal(variance_wrapper(survey, total(quanti), by = quali)$est, c(-11087.64, 31074.47, -28302.91), tolerance = 1e-2)
  expect_equal(variance_wrapper(survey, total(quanti), by = quali)$variance, c(8.501667, 23.339394, 21.080280), tolerance = 1e-6)
})


