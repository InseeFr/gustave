
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
  
  assign(x = "var", value = "speed_quanti", envir = globalenv())
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti)),
    variance_wrapper(ict_survey, mean(var))
  )
  
  assign(x = "var", value = c("speed_quanti", "speed_quali"), envir = globalenv())
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti), mean(speed_quali)),
    variance_wrapper(ict_survey, mean(var))
  )
  speed_quanti2 <- ict_survey$speed_quanti
  expect_identical(
    variance_wrapper(ict_survey, mean(speed_quanti))[ - 1],
    variance_wrapper(ict_survey, mean(speed_quanti2))[ - 1]
  )
  
  assign(x = "num", value = "turnover", envir = globalenv())
  assign(x = "denom", value = "employees", envir = globalenv())
  expect_identical(
    variance_wrapper(ict_survey, ratio(turnover, employees)),
    variance_wrapper(ict_survey, ratio(num, denom))
  )
  assign(x = "num", value = c("turnover", "employees"), envir = globalenv())
  assign(x = "denom", value = c("employees", "turnover"), envir = globalenv())
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

test_that("non-standard evaluation works when a character vector with same name exists outside of data", {
  speed_quanti <- "blabla"
  expect_error(
    variance_wrapper(ict_survey, speed_quanti),
    regexp = NA
  )
})




# Define a new statistic_wrapper and include it in the variance_wrapper

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



# Define a statistic_wrapper that produces two linearized variables
total3 <- define_statistic_wrapper(
  statistic_function = function(y, w){
    na <- is.na(y)
    y[na] <- 0
    point <- sum(y * w)
    list(point = point, lin = list(y, y), metadata = list(n = sum(!na)))
  }, 
  arg_type = list(data = "y" , weight = "w")
)
variance_wrapper <- define_variance_wrapper(
  variance_function = function(y) abs(colSums(y)), 
  reference_id = ict_survey$firm_id,
  reference_weight = ict_survey$w_calib,
  default_id = "firm_id",
  objects_to_include = "total3"
)
test_that("a statistical wrapper producing more than one linearized variables is handled correctly", {
  expect_error(
    variance_wrapper(ict_survey, total3(speed_quanti)), 
    regexp = "The number of estimated variances does not match the number of point estimates. A specific display function could be needed."
  )
})




# Define a new statistic_wrapper based on auto_statistic_function

arg_ratio <- list(data = c("y","x") , weight = c("w"))
arg_dor <- list(data = c("num1","denom1","num2", "denom2") , weight = c("w"))
arg_dor_permuted <- list(data = c("num2", "denom1", "denom2", "num1") , weight = c("w"))


test_that("auto-linearization and classic linearization lead to same results", {
  skip_if_not_installed("torch")
  
  dor_autostat <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(num1, denom1, num2, denom2){num1/denom1 - num2/denom2}, 
                                                 arg_type = arg_dor),
    arg_type = arg_dor)
  
  ror_autostat <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(num1, denom1, num2, denom2){(num1/denom1)/(num2/denom2)}, 
                                                 arg_type = arg_dor),
    arg_type = arg_dor)
  
  ratio_autostat <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(y,x){y/x}, arg_type = arg_ratio),
    arg_type = arg_ratio)
  
  
  variance_wrapper <- define_variance_wrapper(
    variance_function = function(y) abs(colSums(y)), 
    reference_id = ict_survey$firm_id,
    reference_weight = ict_survey$w_calib,
    default_id = "firm_id",
    objects_to_include = c("ratio_autostat", "ror_autostat", "dor_autostat")
  )
  
  
  expect_equal(
    variance_wrapper(ict_survey, ratio(speed_quanti, employees))$variance,
    variance_wrapper(ict_survey, ratio_autostat(speed_quanti, employees))$variance
  )
  
  expect_equal(
    variance_wrapper(ict_survey, diff_of_ratio(turnover, speed_quanti, turnover, employees))$variance,
    variance_wrapper(ict_survey, dor_autostat(turnover, speed_quanti, turnover, employees))$variance,
    tolerance = 1e-5
  )
  
  expect_equal(
    variance_wrapper(ict_survey, ratio_of_ratio(turnover, speed_quanti, turnover, employees))$variance,
    variance_wrapper(ict_survey, ror_autostat(turnover, speed_quanti, turnover, employees))$variance
  )
})





test_that("Auto-linearization leads to the same results even if arg_type is not sorted in the same order as the arguments of fn.", {
  skip_if_not_installed("torch")
  
  dor_autostat <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(num1, denom1, num2, denom2){num1/denom1 - num2/denom2}, 
                                                 arg_type = arg_dor),
    arg_type = arg_dor)
  
  dor_autostat_permuted <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(num1, denom1, num2, denom2){num1/denom1 - num2/denom2}, 
                                                 arg_type = arg_dor_permuted),
    arg_type = arg_dor_permuted)
  
  
  variance_wrapper <- define_variance_wrapper(
    variance_function = function(y) abs(colSums(y)), 
    reference_id = ict_survey$firm_id,
    reference_weight = ict_survey$w_calib,
    default_id = "firm_id",
    objects_to_include = c("dor_autostat", "dor_autostat_permuted")
  )
  
  
  expect_equal(
    variance_wrapper(ict_survey, dor_autostat_permuted(turnover, speed_quanti, turnover, employees))$variance,
    variance_wrapper(ict_survey, dor_autostat(turnover, speed_quanti, turnover, employees))$variance,
    tolerance = 1e-5
  )
})


test_that("Auto-linearization raises an error when `fn` does not return a numerical vector of size 1", {
  skip_if_not_installed("torch")
  
  ratio_autostat_NA <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(y,x){NA}, arg_type = arg_ratio),
    arg_type = arg_ratio)
  
  ratio_autostat_character <- define_statistic_wrapper(
    statistic_function = auto_statistic_function(fn = function(y,x){"blablabla"}, arg_type = arg_ratio),
    arg_type = arg_ratio)
  
  variance_wrapper <- define_variance_wrapper(
    variance_function = function(y) abs(colSums(y)), 
    reference_id = ict_survey$firm_id,
    reference_weight = ict_survey$w_calib,
    default_id = "firm_id",
    objects_to_include = c("ratio_autostat_NA", "ratio_autostat_character")
  )
  
  expect_error(
    variance_wrapper(ict_survey, ratio_autostat_NA(speed_quanti, employees)),
    regexp = "Results from `fn` must be a numerical vector of size 1."
  )
  expect_error(
    variance_wrapper(ict_survey, ratio_autostat_character(speed_quanti, employees)),
    regexp = "Results from `fn` must be a numerical vector of size 1."
  )
})


test_that("Auto-linearization raises an error when `fn` is not a function or `arg_type` is not a list", {
  skip_if_not_installed("torch")
  
  expect_error(
    auto_statistic_function(fn = "toto", arg_type = arg_ratio),
    regexp = "`fn` must be a function."
  )
  expect_error(
    auto_statistic_function(fn = function(y,x){y/x}, arg_type = "toto"),
    regexp = "`arg_type` must be a list."
  )
})