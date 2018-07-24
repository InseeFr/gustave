
context("define_statistic_wrapper")

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y)), 
  reference_id = ict_survey$firm_id,
  reference_weight = ict_survey$w_calib,
  default_id = "firm_id"
)

test_that("standard and non-standard evaluation yields the same results", {
  
  expect_identical(
    variance_wrapper(ict_survey, speed_quanti),
    variance_wrapper(ict_survey, "speed_quanti")
  )
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti)),
    variance_wrapper(ict_survey, mean("speed_quanti"))
  )
  var <- "speed_quanti"
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti)),
    variance_wrapper(ict_survey, mean(var))
  )
  var <- c("speed_quanti", "speed_quali")
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti), mean(speed_quali)),
    variance_wrapper(ict_survey, mean(var))
  )
  speed_quanti2 <- ict_survey$speed_quanti
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti))[ - 1],
    variance_wrapper(ict_survey, mean(speed_quanti2))[ - 1]
  )

  num <- c("turnover")
  denom <- c("employees")
  expect_identical(
    variance_wrapper(ict_survey, ratio(turnover, employees)),
    variance_wrapper(ict_survey, ratio(num, denom))
  )
  num <- c("turnover", "employees")
  denom <- c("employees", "turnover")
  expect_identical(
    variance_wrapper(ict_survey, ratio(turnover, employees), ratio(employees, turnover)),
    variance_wrapper(ict_survey, ratio(num, denom))
  )

  expect_identical(
    variance_wrapper(ict_survey, total(speed_quanti), by = division),
    variance_wrapper(ict_survey, total(speed_quanti, by = division))
  )
  expect_identical(
    variance_wrapper(ict_survey, total(speed_quanti))[, -1],
    variance_wrapper(ict_survey, total(speed_quanti, by = NULL), by = division)[, -1]
  )
  expect_identical(
    variance_wrapper(ict_survey, total(speed_quanti), by = division),
    variance_wrapper(ict_survey, total(speed_quanti), by = "division")
  )
  expect_identical(
    variance_wrapper(ict_survey, total(speed_quanti, by = division)),
    variance_wrapper(ict_survey, total(speed_quanti, by = "division"))
  )
  
  ict_survey$domain <- ict_survey$division == "59"
  expect_identical(
    variance_wrapper(ict_survey, total(speed_quanti), where = division == "59")[, -1],
    variance_wrapper(ict_survey, total(speed_quanti), where = "domain")[, -1]
  )
  rm(ict_survey)

})




# define a new linearization_wrapper and include it in the variance_wrapper

total2 <- define_statistic_wrapper(
  statistic_function = function(y, w, w2){
    na <- is.na(y)
    y[na] <- 0
    point <- sum(y * w)
    list(point = point, lin = y, metadata = list(n = sum(!na)))
  }, 
  arg_type = list(data = "y" , weight = c("w", "w2")),
  arg_not_affected_by_domain = "w2"
)

variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y)), 
  reference_id = ict_survey$firm_id,
  reference_weight = ict_survey$w_calib,
  default_id = "firm_id",
  objects_to_include = "total2"
)
rm(total2)

test_that("a new linearization wrapper can be defined", {
  expect_error(
    variance_wrapper(ict_survey, total2(speed_quanti)),
    regexp = NA
  )
})