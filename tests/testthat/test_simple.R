
context("simple")

technical_data_ict <- list(
  calib = list(
    x = as.matrix(ict_survey[
      order(ict_survey$firm_id),
      c(paste0("N_", 58:63), paste0("turnover_", 58:63))
      ]),
    w = ict_survey$w_calib[order(ict_survey$firm_id)]
  ),
  nr = list(
    id = ict_sample$firm_id,
    response_prob = ict_sample$response_prob_est,
    w_sample = ict_sample$w_sample
  ),
  samp = list(
    w_sample = ict_sample$w_sample,
    strata = ict_sample$division
  )
)


y <- matrix(ict_survey$speed_quanti, dimnames = list(ict_survey$firm_id))

test_that("var_simple works", {
  expect_error(
    with(technical_data_ict, var_simple(y, samp = samp, nr = nr, calib = calib)), 
    regexp = NA
  )
})

test_that("a variance wrapper can be manually defined on top of var_simple", {
  expect_error({
    variance_wrapper_ict <- define_variance_wrapper(
      variance_function = var_simple,
      reference_id = ict_survey$firm_id,
      technical_data = technical_data_ict,
      default = list(id = "firm_id", weight = "w_calib", stat = "mean")
    )
    variance_wrapper_ict(ict_survey, speed_quanti)
  },
  regexp = NA
  )
})

test_that("inconsitency detection works as expected", {
  expect_error(
    define_simple_wrapper(), 
    regexp = "A data file"
  )
  expect_error(
    define_simple_wrapper(data = blabla), 
    regexp = "An identifier of the units must be provided"
  )
  expect_error(
    define_simple_wrapper(data = blabla, id = "blabla"), 
    regexp = "A sampling weight"
  )
  expect_error(
    define_simple_wrapper(
      data = blabla, id = "blabla",
      sampling_weight = "blabla",
      nrc_weight = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - weights after non-response"
  )
  expect_error(
    define_simple_wrapper(
      data = blabla, id = "blabla", 
      sampling_weight = "blabla",
      resp = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - a variable indicating responding units"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "blabla", 
      sampling_weight = "blabla",
      calib = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - a variable indicating the units taking part"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "blabla", 
      sampling_weight = "blabla",
      calib_weight = "blabla"
    ), regexp = "Some arguments are inconsistent:\n  - calibrated weights are provided"
  )
})
# 
# test_that("welcome message works as expected", {
#   welcome <- "Variance wrapper definition using the dataset : blabla\n\nThe following features are taken into account:"
#   expect_message(
#     define_simple_wrapper(
#       data = blabla, 
#       sampling_weight = "blabla"
#     ), regexp = paste0(welcome, "\n  - simple random sampling WITHOUT stratification")
#   )
#   expect_message(
#     define_simple_wrapper(
#       data = blabla, 
#       sampling_weight = "blabla", strata = "blabla"
#     ), regexp = tmp <- paste0(welcome, "\n  - stratified simple random sampling")
#   )
#   expect_message(
#     define_simple_wrapper(
#       data = blabla, 
#       sampling_weight = "blabla", strata = "blabla",
#       scope = "blabla"
#     ), regexp = tmp <- paste0(tmp, "\n  - out-of-scope units")
#   )
#   expect_message(
#     define_simple_wrapper(
#       data = blabla, 
#       sampling_weight = "blabla", strata = "blabla",
#       scope = "blabla",
#       nrc_weight = "blabla", resp = "blabla"
#     ), regexp = tmp <- paste0(tmp, "\n  - non-response correction through reweighting")
#   )
#   expect_message(
#     define_simple_wrapper(
#       data = blabla, 
#       sampling_weight = "blabla", strata = "blabla",
#       scope = "blabla",
#       nrc_weight = "blabla", resp = "blabla",
#       calib_weight = "blabla", calib_var = "blabla"
#     ), regexp = paste0(tmp, "\n  - calibration on margins")
#   )
# })

test_that("argument validity controls work as expected", {
  expect_error(
    define_simple_wrapper(data = blabla, id = "blabla", sampling_weight = "blabla"), 
    regexp = "obj"
  )
  expect_error(
    define_simple_wrapper(data = matrix(1:10), id = "blabla", sampling_weight = "blabla"), 
    regexp = "data argument must refer to a data.frame"
  )
  expect_error(
    define_simple_wrapper(data = ict_sample, id = "firm_id", sampling_weight = "w_sample"), 
    regexp = NA
  )
  expect_error(
    define_simple_wrapper(
      data = tibble::as.tibble(ict_sample), 
      id = "firm_id", 
      sampling_weight = "w_sample"
    ), 
    regexp = NA
  )
  expect_error(
    define_simple_wrapper(
      data = data.table::as.data.table(ict_sample), 
      id = "firm_id", 
      sampling_weight = "w_sample"
    ), 
    regexp = NA
  )
  expect_error(
    define_simple_wrapper(data = ict_sample, id = "blabla", sampling_weight = c("blabla", "bloblo")),
    regexp = "The following arguments do not refer to a variable name"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "blabla", sampling_weight = "blabla",
      calib_weight = "blabla", calib_var = 2
    ), 
    regexp = "The following arguments do not refer to a vector of variable names"
  )
  expect_error(
    define_simple_wrapper(
      data = ict_sample, id = "firm_id",
      sampling_weight = "w_sample", strata = "division",
      nrc_weight = "w_nr", resp = "blabla"
    ), 
    regexp = "Some variables do not exist in ict_sample: \n  - resp argument: blabla"
  )
})
