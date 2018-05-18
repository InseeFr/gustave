
context("linearization_wrapper")

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y))
  , reference_id = ict_survey$firm_id
  , default = list(id = "firm_id", weight = "w_calib", stat = "mean")
)

test_that("complex inearization wrappers relying on the vardpoor package do work", {
  expect_error(variance_wrapper(ict_survey, arpr(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, arpr(speed_quanti, by = division)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, gini(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, gini(speed_quanti, by = division)), regexp = NA)
})
