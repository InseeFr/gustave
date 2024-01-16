
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

test_that("varDT with exhaustive", {
  pik <- c(1,rep(c(0.2,0.5),4),1)
  precalcDT <- varDT(y = NULL, pik = pik, keep_exh = TRUE)
  precalcDT_exh <- varDT(y = NULL, pik = pik[2:9])
  expect_equal(precalcDT$pik[2:9], precalcDT_exh$pik)
  expect_equal(precalcDT$diago[2:9], precalcDT_exh$diago)
  expect_equal(varDT(y, pik = pik), varDT(y[2:9], pik = pik[2:9]))
  expect_equal(varDT(y, precalc = precalcDT), varDT(y[2:9], precalc = precalcDT_exh))
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


test_that("var_Wolter works", {
  #Units collapsed
  n_units_coll <- 50L
  strat_before_coll <- paste0("coll_",1:n_units_coll)
  #Collapse all n_units_coll into 3 collapses stratum (A, B and C)
  strat_after_coll <- sample(c("A","B","C"), n_units_coll, replace = TRUE)
  
  #Units non collapsed
  n_units_non_coll <- 200L
  strat_before_non_coll <- rep(as.character(1:5), 40)
  strat_after_non_coll <- rep(NA, n_units_non_coll)
  
  old_strata <- c(strat_before_coll, strat_before_non_coll)
  new_strata <- c(strat_after_coll, strat_after_non_coll)
  pik_per_srs <- setNames(c(0.2,0.4,0.2,0.4,0.4), as.character(1:5))
  pik_per_new <- setNames(c(0.1,0.5,0.6), c("A","B","C"))
  
  
  pik <- c(pik_per_new[strat_after_coll], pik_per_srs[strat_before_non_coll])
  id <- paste0("id",1:(n_units_coll + n_units_non_coll))
  names(old_strata) <- id
  names(new_strata) <- id
  names(pik) <- id
  precalc_var <- var_wolter(y = NULL,
                            old_strata = old_strata, 
                            new_strata = new_strata,
                            pik = pik,
                            id = id)
  
  
  
  y <- as.matrix(rnorm(length(pik)), ncol = 1)
  rownames(y) <- id
  
  var_from_wolter_fn <- var_wolter(y = y, precalc = precalc_var, id)
  
  
  #Manually
  y_collapse <- y[!is.na(new_strata)]
  y_tilde <- y_collapse/(pik[!is.na(new_strata)])
  tot_h <- sum_by(y_tilde, old_strata[!is.na(new_strata)])
  tot_p <- sum_by(y_tilde, new_strata[!is.na(new_strata)])
  hp <- table(new_strata)
  hp <- setNames(hp, names(hp))
  
  links <- unique(cbind(old_strata[!is.na(new_strata)],new_strata[!is.na(new_strata)]))
  links <- setNames(links[,2], links[,1])
  
  var_wolter_part <- sum(((tot_p[links]/hp[links] - tot_h[names(links)])^2)*(1/((1 - 1/hp[links]))))
  var_srs_part <- var_srs(y[is.na(new_strata)], pik[is.na(new_strata)], old_strata[is.na(new_strata)])
  
  
  testthat::expect_equal(var_srs_part + var_wolter_part, var_from_wolter_fn)
})