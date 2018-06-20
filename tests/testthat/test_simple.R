
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

