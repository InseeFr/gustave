
context("data")

test_that("non-response correction and calibration went well", {
  expect_equal(sum(ict_sample$w_sample), sum(ict_sample$w_nr[ict_sample$resp]))
  expect_equal(sum(ict_sample$w_sample), sum(ict_survey$w_nr))
  expect_equal(sum(ict_sample$w_sample), sum(ict_survey$w_calib))
  expect_equal(
    tapply(ict_pop$turnover, ict_pop$division, sum),
    tapply(ict_survey$turnover * ict_survey$w_calib, ict_survey$division, sum)
  )
  expect_equal(
    tapply(ict_pop$firm_id, ict_pop$division, length),
    tapply(ict_survey$w_calib, ict_survey$division, sum)
  )
})