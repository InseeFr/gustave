
context("variance_function")

N <- 1000
n <- 10
pikl <- matrix(rep(n*(n - 1) / (N * (N - 1)), n^2), ncol = n)
diag(pikl) <- rep(n/N, rep = n)
y <- rnorm(n)

test_that("varDT works", {
  precalcDT <- varDT(y = NULL, pik = diag(pikl))
  expect_error(varDT(y, pik = diag(pikl)), regexp = NA)
  expect_error(varDT(y, precalc = precalcDT), regexp = NA)
  expect_equal(varDT(y, pik = diag(pikl)), varDT(y, precalc = precalcDT))
})

test_that("varSYG works", {
  precalcSYG <- varSYG(y = NULL, pikl = pikl)
  expect_error(varSYG(y, pikl = pikl), regexp = NA)
  expect_error(varSYG(y = y, precalc = precalcSYG), regexp = NA)
  expect_equal(varSYG(y, pikl = pikl), varSYG(y = y, precalc = precalcSYG))
})

test_that("varSYG and varDT yield the same results in the SRS case", {
  expect_equal(varDT(y, pik = diag(pikl)), varSYG(y, pikl = pikl))
})


# More detailed tests about varDT()
test_that("colinearity detection works", {
  pik <- 1 / ict_sample$w_sample
  strata <- ict_sample$division
  y <- ict_sample$turnover
  expect_warning(
    varDT(y = NULL, pik = pik, x = matrix(rep(pik, 2), ncol = 2), strata = strata),
    regexp = "Some variables in x were discarded due to collinearity."
  )
  expect_identical(
    suppressWarnings(varDT(y = NULL, pik = pik, x = matrix(rep(pik, 2), ncol = 2), strata = strata)),
    varDT(y = NULL, pik = pik, x = pik, strata = strata)
  )
  skip("Not functional yet")
  x_tmp <- block_matrix(matrix(rep(pik, 2), ncol = 2), strata)$y[, -c(2, 4)]
  expect_identical(
    varDT(y = NULL, pik = pik, x = x_tmp),
    varDT(y = NULL, pik = pik, x = pik, strata = strata)
  )
})
test_that("exhaustive units are handled correctly", {
  pik <- 1 / ict_sample$w_sample
  strata <- ict_sample$division
  y <- ict_sample$turnover
  pik[1:10] <- 1
  expect_warning(
    varDT(y = NULL, pik = pik, strata = strata),
    regexp = "units are exhaustive \\(pik = 1\\). They are discarded from the variance estimation process."
  )
  pik <- 1 / ict_sample$w_sample
  pik[strata == "62"] <- 1
  expect_warning(
    varDT(y = NULL, pik = pik, strata = strata),
    regexp = "units are exhaustive \\(pik = 1\\). They are discarded from the variance estimation process."
  )
})
test_that("non-matching id raise an error", {
  id <- ict_sample$firm_id
  pik <- 1 / ict_sample$w_sample
  strata <- ict_sample$division
  precalc <- varDT(y = NULL, pik = pik, strata = strata, id = id)
  y <- setNames(ict_sample$turnover, id)
  expect_error(varDT(y, precalc = precalc), regexp = NA)
  expect_error(
    varDT(y[rev(seq_along(y))], precalc = precalc),
    regexp = "The names of the data matrix \\(y argument\\) do not match"
  )

})