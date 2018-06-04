

context("linearization_wrapper_laeken")

test_that("complex linearization wrappers relying on the vardpoor package work", {
  expect_error(variance_wrapper(ict_survey, arpr(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, arpr(speed_quanti, by = division)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, gini(speed_quanti)), regexp = NA)
  expect_error(variance_wrapper(ict_survey, gini(speed_quanti, by = division)), regexp = NA)
})
