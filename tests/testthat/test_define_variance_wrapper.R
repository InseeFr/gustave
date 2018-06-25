

context("define_variance_wrapper")

test_that("common error messages do work", {
  expect_error(
    define_variance_wrapper(), 
    regexp = "The following arguments are missing: variance_function, reference_id, reference_weight."
  )
  expect_error(
    define_variance_wrapper(variance_function = function(y) abs(colSums(y))),
    regexp = "The following arguments are missing: reference_id, reference_weight."
  )
  expect_error(
    define_variance_wrapper(
      variance_function = function(y) abs(colSums(y)),
      reference_id = "firm_id"
    ),
    regexp = "The following arguments are missing: reference_weight."
  )
})

test_that("variance_wrapper can be defined in globalenv()", {
  expect_error({
    variance_wrapper <<- define_variance_wrapper(
      variance_function = function(y) abs(colSums(y)), 
      reference_id = ict_survey$firm_id,
      reference_weight = ict_survey$w_calib,
      default_id = "firm_id"
    )    
    variance_wrapper(ict_survey, speed_quanti)
  }, regexp = NA)
})


test_that("variance_wrapper can be defined in another function", {
  expect_error({
    preparation_function <- function(){
      a <- 1
      define_variance_wrapper(
        variance_function = function(y, a) abs(colSums(y)) + a, 
        reference_id = ict_survey$firm_id,
        reference_weight = ict_survey$w_calib,
        technical_data = list(a = a),
        default_id = "firm_id"
      )
    }
    variance_wrapper2 <- preparation_function()
    variance_wrapper2(ict_survey, speed_quanti)
  }, regexp = NA)
  expect_equal(
    variance_wrapper(ict_survey, speed_quanti)$variance + 1,
    variance_wrapper2(ict_survey, speed_quanti)$variance
  )
})

test_that("variance_wrapper may use a reference_id and a reference_weights specified as an unevaluated expression", {
  expect_error({
    id_list <- list(firm = ict_survey$firm_id)
    weight_list <- list(firm = ict_survey$w_calib)
    variance_wrapper <- define_variance_wrapper(
      variance_function = function(y, level = "firm") abs(colSums(y)), 
      reference_id = quote(id_list[[level]]),
      reference_weight = quote(weight_list[[level]]),
      default_id = "firm_id",
      objects_to_include = c("id_list", "weight_list")
    )
    rm(id_list, weight_list)
    variance_wrapper(ict_survey, speed_quanti)
  }, regexp = NA)
})

test_that("variance_wrapper may use a default id specified as an unevaluated expression", {
  expect_error({
    variance_wrapper <- define_variance_wrapper(
      variance_function = function(y, level = "firm") abs(colSums(y)), 
      reference_id = ict_survey$firm_id,
      reference_weight = ict_survey$w_calib,
      default_id = quote(paste0(firm_id, ""))
    )
    variance_wrapper(ict_survey, speed_quanti)
  }, regexp = NA)
})


test_that("variance_wrapper works in common situations", {
  expect_error(variance_wrapper(ict_survey, speed_quanti), regexp = NA)
  expect_error(variance_wrapper(ict_survey, speed_quanti_NA), regexp = NA)
  expect_error(variance_wrapper(ict_survey, speed_quali), regexp = NA)
  expect_error(variance_wrapper(ict_survey, speed_quali_NA), regexp = NA)
  expect_error(variance_wrapper(ict_survey, big_data), regexp = NA)
  expect_error(variance_wrapper(ict_survey, big_data_NA), regexp = NA)
  expect_error(variance_wrapper(ict_survey, speed_quanti, by = division), regexp = NA)
  expect_error(variance_wrapper(ict_survey, big_data, by = speed_quali_NA), regexp = NA)
  expect_error(variance_wrapper(ict_survey, big_data, NULL), regexp = NA)
})


test_that("expected error messages do appear", {
  expect_error(variance_wrapper(ict_survey), "No variable to estimate variance on.")
})


test_that("point estimates do match by-hand estimators", {
  expect_equal(
    variance_wrapper(ict_survey, total(speed_quanti_NA))$est,
    sum(ict_survey$speed_quanti_NA * ict_survey$w_calib, na.rm = TRUE)
  )
  expect_equal(
    variance_wrapper(ict_survey, mean(speed_quanti_NA))$est,
    weighted.mean(ict_survey$speed_quanti_NA, ict_survey$w_calib, na.rm = TRUE)
  )
  expect_equal(
    variance_wrapper(ict_survey, mean(speed_quanti_NA), by = division)$est,
    as.vector(sapply(split(ict_survey, ict_survey$division), function(x)
      weighted.mean(x$speed_quanti_NA, x$w_calib, na.rm = TRUE)
    ))
  )
  expect_equal(
    variance_wrapper(ict_survey, total(speed_quali))$est,
    as.vector(tapply(ict_survey$w_calib, ict_survey$speed_quali, sum))
  )
})

test_that("estimated values do match reference values", {
  expect_equal(variance_wrapper(ict_survey, speed_quanti_NA)$est, 178409.7, tolerance = 1e-0)
  expect_equal(variance_wrapper(ict_survey, speed_quanti_NA)$variance, 15817, tolerance = 1e-0)
  expect_equal(variance_wrapper(ict_survey, speed_quali_NA)$est, c(154, 1748, 2163, 734, 640), tolerance = 1e0)
  expect_equal(variance_wrapper(ict_survey, speed_quali_NA)$variance, c(10, 138, 170, 67, 59), tolerance = 1e0)
  expect_equal(variance_wrapper(ict_survey, big_data_NA, by = speed_quali_NA)$est, c(0, 18.5, 0, 164.8, 146.1), tolerance = 1e0)
  expect_equal(variance_wrapper(ict_survey, big_data_NA, by = speed_quali_NA)$variance, c(0, 1, 0, 15, 14), tolerance = 1e-0)
})
