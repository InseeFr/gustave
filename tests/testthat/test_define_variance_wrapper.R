

context("define_variance_wrapper")

test_that("variance_wrapper can be defined in globalenv()", {
  expect_error({
    variance_wrapper <<- define_variance_wrapper(
      variance_function = function(y) abs(colSums(y)), 
      reference_id = ict_survey$firm_id, 
      default = list(id = "firm_id", weight = "w_calib", stat = "mean")
    )    
    variance_wrapper(ict_survey, speed_quanti)
  }, regexp = NA)
})


test_that("variance_wrapper can be defined in another function", {
  expect_error({
    preparation_function <- function(){
      a <- 1
      define_variance_wrapper(
        variance_function = function(y) abs(colSums(y)) + 1, 
        reference_id = ict_survey$firm_id, 
        default = list(id = "firm_id", weight = "w_calib", stat = "mean"), 
        objects_to_include = "a"
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

test_that("variance_wrapper may use a reference_id specified as an unevaluated expression", {
  expect_error({
    id_list <- list(firm = ict_survey$firm_id)
    variance_wrapper <<- define_variance_wrapper(
      variance_function = function(y, level = "firm") abs(colSums(y)), 
      reference_id = quote(id_list[[level]]), 
      default = list(id = "firm_id", weight = "w_calib", stat = "mean"),
      objects_to_include = "id_list"
    )
    rm(id_list)
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
  expect_equal(variance_wrapper(ict_survey, speed_quanti_NA)$est, 32.80242, tolerance = 1e-4)
  expect_equal(variance_wrapper(ict_survey, speed_quanti_NA)$variance, 0.230266, tolerance = 1e-7)
  expect_equal(variance_wrapper(ict_survey, speed_quali_NA)$est, c(0.02834094, 0.32141225, 0.39763176, 0.13490418, 0.11771088), tolerance = 1e-8)
  expect_equal(variance_wrapper(ict_survey, speed_quali_NA)$variance, c(0.0004749795, 0.0008654360, 0.0012040079, 0.0013058744, 0.0012385490), tolerance = 1e-8)
  expect_equal(variance_wrapper(ict_survey, big_data_NA, by = speed_quali_NA)$est, c(0.00000000, 0.01416254, 0.00000000, 0.28487253, 0.30151894), tolerance = 1e-8)
  expect_equal(variance_wrapper(ict_survey, big_data_NA, by = speed_quali_NA)$variance, c(0.0000000000, 0.0003726230, 0.0000000000, 0.0003226717, 0.0015129483), tolerance = 1e-8)
})
