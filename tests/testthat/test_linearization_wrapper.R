
context("linearization_wrapper")

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y)), 
  reference_id = ict_survey$firm_id, 
  default = list(id = "firm_id", weight = "w_calib", stat = "mean")
)

test_that("standard linearization wrappers work", {
  expect_error(variance_wrapper(ict_survey, mean(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, mean(speed_quanti, by = division)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, ratio(speed_quanti, turnover)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, ratio(speed_quanti, turnover, by = division)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, diffratio(speed_quanti, turnover, speed_quanti, employees)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, diffratio(speed_quanti, turnover, speed_quanti, employees, by = division)), regexp = NA)
})


test_that("complex linearization wrappers relying on the vardpoor package work", {
  expect_error(variance_wrapper(ict_survey, arpr(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, arpr(speed_quanti, by = division)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, gini(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, gini(speed_quanti, by = division)), regexp = NA)
})
