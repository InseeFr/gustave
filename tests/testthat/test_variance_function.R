
context("variance_function")

# res_cal


test_that("res_cal works as expected", {
  y <- ict_sample$employees
  x <- ict_sample$turnover
  expect_equal(res_cal(y, x), unname(lm(y ~ x - 1)$residuals))
  x <- make_block(x, ict_sample$division)
  expect_equal(res_cal(y, x), unname(lm(y ~ as.matrix(x) - 1)$residuals))
  x <- x[, c(1:NCOL(x), 1)]
  expect_message(res_cal(y, x), regexp = "Some variables in x were discarded due to collinearity.")
  expect_equal(suppressWarnings(res_cal(y, x)), unname(lm(y ~ as.matrix(x) - 1)$residuals))
})





# varDT and varSYG

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

test_that("varSYG works with id", {
  precalcSYG <- varSYG(y = NULL, pikl = pikl, id = paste0("id_",1:n))
  y0 <- matrix(y, ncol = 1)
  rownames(y0) <- paste0("id_",1:n)
  y0_permute <- y0[nrow(y0):1, ,drop = FALSE]
  expect_error(varSYG(y = y0_permute, precalc = precalcSYG), "The names of the data matrix*.")
  expect_equal(varSYG(y0, pikl = pikl), varSYG(y = y0, precalc = precalcSYG))
})

test_that("var_pois works", {
  precalc_pois <- var_pois(y = NULL, diag(pikl), id = paste0("id_",1:n))
  y0 <- matrix(y, ncol = 1)
  rownames(y0) <- paste0("id_",1:n)
  y0_permute <- y0[nrow(y0):1, ,drop = FALSE]
  var_comp <- sum((y0[, 1]/diag(pikl))^2 * (1-diag(pikl)))
  expect_equal(var_pois(y = y0, precalc = precalc_pois), var_comp)
  expect_error(var_pois(y = y0_permute, precalc = precalc_pois), "The names of the data matrix*.")
  expect_named(var_pois(y = NULL, precalc = precalc_pois), c("pik", "diago", "id"))
})

test_that("varSYG and varDT yield the same results in the SRS case", {
  expect_equal(varDT(y, pik = diag(pikl)), varSYG(y, pikl = pikl))
})


# varDT()
test_that("colinearity detection works", {
  pik <- 1 / ict_sample$w_sample
  strata <- ict_sample$division
  y <- ict_sample$turnover
  expect_message(
    varDT(y = NULL, pik = pik, x = matrix(rep(pik, 2), ncol = 2), strata = strata),
    regexp = "Some variables in x were discarded due to collinearity."
  )
  expect_identical(
    suppressWarnings(varDT(y = NULL, pik = pik, x = matrix(rep(pik, 2), ncol = 2), strata = strata)),
    suppressWarnings(varDT(y = NULL, pik = pik, x = pik, strata = strata))
  )
  x_tmp <- make_block(matrix(rep(pik, 2), ncol = 2), strata)[, -c(2, 4)]
  expect_identical(
    suppressWarnings(varDT(y = NULL, pik = pik, x = x_tmp, strata = strata)),
    suppressWarnings(varDT(y = NULL, pik = pik, x = pik, strata = strata))
  )
})
test_that("exhaustive units are handled correctly", {
  pik <- 1 / ict_sample$w_sample
  strata <- ict_sample$division
  y <- ict_sample$turnover
  pik[1:10] <- 1
  expect_message(
    varDT(y = NULL, pik = pik, strata = strata),
    regexp = "units are exhaustive \\(pik = 1\\). They are discarded from the variance estimation process."
  )
  pik <- 1 / ict_sample$w_sample
  pik[strata == "62"] <- 1
  expect_message(
    varDT(y = NULL, pik = pik, strata = strata),
    regexp = "units are exhaustive \\(pik = 1\\). They are discarded from the variance estimation process."
  )
})
test_that("non-matching id raise an error", {
  id <- ict_sample$firm_id
  pik <- 1 / ict_sample$w_sample
  strata <- ict_sample$division
  precalc <- suppressWarnings(varDT(y = NULL, pik = pik, strata = strata, id = id))
  y <- setNames(ict_sample$turnover, id)
  expect_error(varDT(y, precalc = precalc), regexp = NA)
  expect_error(
    varDT(y[rev(seq_along(y))], precalc = precalc),
    regexp = "The names of the data matrix \\(y argument\\) do not match"
  )

})


