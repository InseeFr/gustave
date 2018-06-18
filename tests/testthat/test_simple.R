
context("simple")

calib <- list(
  x = as.matrix(ict_survey[
    order(ict_survey$firm_id),
    c(paste0("N_", 58:63), paste0("turnover_", 58:63))
    ]),
  w = ict_survey$w_calib[order(ict_survey$firm_id)]
)

nr <- list(
  id = ict_sample$firm_id,
  response_prob = ict_sample$response_prob_est,
  w_sample = ict_sample$w_sample
)

samp <- list(
  w_sample = ict_sample$w_sample,
  strata = ict_sample$division
)

y <- as.matrix(ict_survey$speed_quanti)
rownames(y) <- ict_survey$firm_id

test_that("var_simple works", {
  expect_error(var_simple(y, samp = samp, nr = nr, calib = calib), regexp = NA)
})

