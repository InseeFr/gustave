
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
