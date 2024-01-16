
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

test_that("varSYG works with w", {
  w <- rep(3,n)
  
  precalcSYG <- varSYG(y = NULL, pikl = pikl)
  precalcSYG_w <- varSYG(y = NULL, pikl = pikl, w = w)
  expect_error(varSYG(y, pikl = pikl), regexp = NA)
  expect_error(varSYG(y = y, precalc = precalcSYG), regexp = NA)
  expect_equal(varSYG(y = y, precalc = precalcSYG_w), 
               varSYG(y = y*sqrt(w), precalc = precalcSYG))
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




test_that("var_Rao works well - SRS/Poisson",{
  #Define PSU and SSU sample
  n_psu <- 10
  n_per_psu <- sample(5:10, n_psu, replace = TRUE)
  id_psu <- paste0("id_psu", 1:n_psu)
  #psu_by_ssu is a ssu size sample that contains for each ssu, 
  #the label of the psu to which it belongs.
  psu_by_ssu <- rep(paste0("id_psu",1:n_psu), n_per_psu)
  id_ssu <- paste0("id_ssu", 1:sum(n_per_psu))
  pi_psu <- stats::setNames(sample(c(0.1,0.2,0.5,0.6), replace = TRUE, n_psu), id_psu)
  pi_ssu <- stats::setNames(runif(sum(n_per_psu), 0.2, 0.8), id_ssu)
  
  #Compute descriptions
  precalc1 <- var_srs(y = NULL,
                      pik = pi_psu,
                      id = id_psu)
  descrip1 <- list("var_fn" = "var_srs",
                   "precalc" = precalc1)
  
  precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    return(list("var_fn" = "var_pois",
                "precalc" = var_pois(y = NULL, pik = pi_ssu[ind], id = id_ssu[ind])))
  })
  names(precalc2) <- unique(psu_by_ssu)
  descrip2 <- precalc2
  
  #Compute the resulting variance manually
  y <- as.matrix(rnorm(sum(n_per_psu)), ncol = 1)
  rownames(y) <- id_ssu
  
  y_ssu <- y[id_ssu,]
  y_ssu <- matrix(y_ssu, ncol = 1, dimnames = list(names(y_ssu),NULL))
  y_psu <- sum_by(y_ssu, psu_by_ssu, 1/pi_ssu)
  y_psu <- matrix(y_psu, ncol = 1, dimnames = list(rownames(y_psu),NULL))
  
  
  var_psu <- var_srs(y = y_psu, pik = pi_psu[rownames(y_psu)], id = rownames(y_psu))
  
  q_psu <- var_srs(y = NULL, pik = pi_psu[rownames(y_psu)], id = rownames(y_psu))$diago
  pond <- stats::setNames((1/(pi_psu^2) - q_psu[names(pi_psu)]), names(pi_psu))
  pond <- pond[psu_by_ssu]
  var_ssu <- var_pois(y = y_ssu, pik = pi_ssu[rownames(y_ssu)], w = pond)
  
  var_manually <- var_psu + var_ssu
  
  
  
  #Tests
  res1 <- var_Rao(y = NULL, 
                  description_psu = descrip1, 
                  description_ssu = descrip2, 
                  id = id_ssu)
  
  var_with_Rao <- var_Rao(y, precalc = res1)
  
  expect_equal(var_with_Rao, var_manually)
})

test_that("var_Rao works well with permutation/unnamed y - SRS/Poisson",{
  #Define PSU and SSU sample
  n_psu <- 10
  n_per_psu <- sample(5:10, n_psu, replace = TRUE)
  id_psu <- paste0("id_psu", 1:n_psu)
  #psu_by_ssu is a ssu size sample that contains for each ssu, 
  #the label of the psu to which it belongs.
  psu_by_ssu <- rep(paste0("id_psu",1:n_psu), n_per_psu)
  id_ssu <- paste0("id_ssu", 1:sum(n_per_psu))
  pi_psu <- stats::setNames(sample(c(0.1,0.2,0.5,0.6), replace = TRUE, n_psu), id_psu)
  pi_ssu <- stats::setNames(runif(sum(n_per_psu), 0.2, 0.8), id_ssu)
  
  #Compute descriptions
  precalc1 <- var_srs(y = NULL,
                      pik = pi_psu,
                      id = id_psu)
  descrip1 <- list("var_fn" = "var_srs",
                   "precalc" = precalc1)
  
  precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    return(list("var_fn" = "var_pois",
                "precalc" = var_pois(y = NULL, pik = pi_ssu[ind], id = id_ssu[ind])))
  })
  names(precalc2) <- unique(psu_by_ssu)
  descrip2 <- precalc2
  names(precalc2)
  
  descrip2_permuted <- precalc2[sample(names(precalc2), length(precalc2))]
  #Compute the resulting variance manually
  y <- as.matrix(rnorm(sum(n_per_psu)), ncol = 1)
  rownames(y) <- id_ssu
  
  y_ssu <- y[id_ssu,]
  y_ssu <- matrix(y_ssu, ncol = 1, dimnames = list(names(y_ssu),NULL))
  y_psu <- sum_by(y_ssu, psu_by_ssu, 1/pi_ssu)
  y_psu <- matrix(y_psu, ncol = 1, dimnames = list(rownames(y_psu),NULL))
  
  
  var_psu <- var_srs(y = y_psu, pik = pi_psu[rownames(y_psu)], id = rownames(y_psu))
  
  q_psu <- var_srs(y = NULL, pik = pi_psu[rownames(y_psu)], id = rownames(y_psu))$diago
  pond <- stats::setNames((1/(pi_psu^2) - q_psu[names(pi_psu)]), names(pi_psu))
  pond <- pond[psu_by_ssu]
  var_ssu <- var_pois(y = y_ssu, pik = pi_ssu[rownames(y_ssu)], w = pond)
  
  var_manually <- var_psu + var_ssu
  
  
  
  #Tests
  res1 <- var_Rao(y = NULL, 
                  description_psu = descrip1, 
                  description_ssu = descrip2, 
                  id = id_ssu)
  
  res1_permuted <- var_Rao(y = NULL, 
                           description_psu = descrip1, 
                           description_ssu = descrip2_permuted, 
                           id = id_ssu)
  
  
  #Define others y
  y_unnamed <- y
  rownames(y_unnamed) <- NULL
  y_permuted <- y
  y_permuted <- y_permuted[sample(1:length(y_permuted), length(y_permuted)),,drop = FALSE]
  y_permuted_und <- y_permuted
  rownames(y_permuted_und) <- NULL
  
  var_with_Rao <- var_Rao(y, precalc = res1)
  expect_warning(var_with_Rao_prec_permuted <- var_Rao(y, precalc = res1_permuted),
                 "y's rows have been sorted*.")  
  expect_warning(var_with_Rao_y_permuted <- var_Rao(y_permuted, precalc = res1),
                 "y's rows have been sorted*.")  
  expect_warning(var_with_Rao_both_permuted <- var_Rao(y_permuted, precalc = res1_permuted),
                 "y's rows have been sorted*.") 
  expect_warning(var_with_Rao_y_permuted_und <- var_Rao(y_permuted_und, precalc = res1),
                 "y must be a row-named matrix : *.") 
  expect_warning(var_with_Rao_y_permuted_und <- var_Rao(y_permuted_und, precalc = res1_permuted),
                 "y must be a row-named matrix : *.") 
  
  expect_equal(var_with_Rao, var_manually)
  expect_equal(var_with_Rao_prec_permuted, var_with_Rao)
  expect_equal(var_with_Rao_y_permuted, var_with_Rao)
  expect_equal(var_with_Rao_both_permuted, var_with_Rao)
  
  
  expect_warning(var_with_Rao_unnamed <- var_Rao(y_unnamed, precalc = res1),
                 regexp = 'y must be a row-named matrix:*.')
})

test_that("var_Rao errors", {
  n_psu <- 10L
  n_per_psu <- sample(5:10, 10, replace = TRUE)
  id_psu <- paste0("id_psu", 1:n_psu)
  psu_by_ssu <- rep(paste0("id_psu",1:10), n_per_psu)
  
  
  id_ssu <- paste0("id_ssu", 1:sum(n_per_psu))
  pi_psu <- stats::setNames(sample(c(0.1,0.2,0.5,0.6), replace = TRUE, n_psu), id_psu)
  pi_ssu <- stats::setNames(runif(sum(n_per_psu), 0.2, 0.8), id_ssu)
  
  
  precalc1 <- var_srs(y = NULL,
                      pik = pi_psu,
                      id = id_psu)
  descrip1 <- list("var_fn" = "var_srs",
                   "precalc" = precalc1)
  descrip1_unnamed <- descrip1
  names(descrip1_unnamed) <- NULL
  
  precalc1_witout_id <-  var_srs(y = NULL,
                                 pik = pi_psu)
  descrip1_without_id <- list("var_fn" = "var_srs",
                              "precalc" = var_srs(y = NULL,
                                                  pik = pi_psu))
  
  
  
  precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    return(list("var_fn" = "var_pois",
                "precalc" = var_pois(y = NULL, pik = pi_ssu[ind], id = id_ssu[ind])))
  })
  names(precalc2) <- unique(psu_by_ssu)
  
  precalc2_without_id <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    return(list("var_fn" = "var_pois",
                "precalc" = var_pois(y = NULL, pik = pi_ssu[ind])))
  })
  names(precalc2_without_id) <- unique(psu_by_ssu)
  
  precalc2_unnamed <- precalc2
  names(precalc2_unnamed) <- NULL
  
  precalc2_without_inner_name <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    return(list("var_pois",
                var_pois(y = NULL, pik = pi_ssu[ind], id = id_ssu[ind])))
  })
  names(precalc2_without_inner_name) <- unique(psu_by_ssu)
  
  
  
  descrip2_without_id <- precalc2_without_id
  descrip2 <- precalc2
  descrip2_unnamed <- precalc2_unnamed
  descrip2_inner_unnamed <- precalc2_without_inner_name
  descrip2_renamed <- descrip2
  names(descrip2_renamed) <- paste0("new_name",names(descrip2_renamed))
  #diff between description_psu$precalc$id
  #and description_ssu
  
  expect_error(res1_witout_id2 <- var_Rao(y = NULL, 
                                          description_psu = descrip1, 
                                          description_ssu = descrip2_without_id, 
                                          id = id_ssu),
               regexp = "Id's are missing in some precalc from description_ssu*.")
  
  expect_error(res1_witout_id1 <- var_Rao(y = NULL, 
                                          description_psu = descrip1_without_id, 
                                          description_ssu = descrip2, 
                                          id = id_ssu),
               regexp = "precalc in description_psu must contain a id attribute : please, fullfill it*.")
  
  expect_error(res1_unnamed_2 <- var_Rao(y = NULL, 
                                         description_psu = descrip1, 
                                         description_ssu = descrip2_unnamed, 
                                         id = id_ssu),
               regexp = "description_ssu must be a named list of list*.")
  
  expect_error(res1_inner_unnamed_2 <- var_Rao(y = NULL, 
                                               description_psu = descrip1, 
                                               description_ssu = descrip2_inner_unnamed, 
                                               id = id_ssu),
               regexp = "Id's are missing in some precalc from description_ssu*.")
  
  expect_error(res1_unnamed_1 <- var_Rao(y = NULL, 
                                         description_psu = descrip1_unnamed, 
                                         description_ssu = descrip2, 
                                         id = id_ssu),
               regexp = "precalc in description_psu*.")
  
  expect_error(res1_diff_name <- var_Rao(y = NULL, 
                                         description_psu = descrip1, 
                                         description_ssu = descrip2_renamed, 
                                         id = id_ssu),
               regexp = "Element names of description_ssu must be the same*.")
})

test_that("var_Rao works well - Poisson/SYG",{
  #
  #SSU sampling : a SRSWOR with N = 100 and a 
  #sampling fraction in \{0.1,0.2,0.5,0.6\}
  #Define PSU and SSU sample
  n_psu <- 10
  n_per_psu <- sample(5:10, n_psu, replace = TRUE)
  id_psu <- paste0("id_psu", 1:n_psu)
  #psu_by_ssu is a ssu size sample that contains for each ssu, 
  #the label of the psu to which it belongs.
  psu_by_ssu <- rep(paste0("id_psu",1:n_psu), n_per_psu)
  id_ssu <- paste0("id_ssu", 1:sum(n_per_psu))
  pi_psu <- stats::setNames(runif(length(id_psu), 0.2, 0.8), id_psu)
  pi_ssu <- stats::setNames(sample(c(0.1,0.2,0.5,0.6), replace = TRUE, length(id_psu)), id_psu)
  pi_ssu <- pi_ssu[psu_by_ssu]
  
  #Compute descriptions
  precalc1 <- var_pois(y = NULL,
                       pik = pi_psu,
                       id = id_psu)
  descrip1 <- list("var_fn" = "var_pois",
                   "precalc" = precalc1)
  
  precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    n <- length(ind)
    N_in_each_psu <- n/unique(pi_ssu[ind])
    mat_pij <- matrix((n*(n-1))/(N_in_each_psu*(N_in_each_psu-1)), nrow = n, ncol = n)
    diag(mat_pij) <- n/N_in_each_psu
    return(list("var_fn" = "varSYG",
                "precalc" = varSYG(y = NULL, pikl = mat_pij, id = id_ssu[ind])))
  })
  names(precalc2) <- unique(psu_by_ssu)
  descrip2 <- precalc2
  
  #Compute the resulting variance manually
  y <- as.matrix(rnorm(sum(n_per_psu)), ncol = 1)
  rownames(y) <- id_ssu
  
  y_ssu <- y[id_ssu,]
  y_ssu <- matrix(y_ssu, ncol = 1, dimnames = list(names(y_ssu),NULL))
  y_psu <- sum_by(y_ssu, psu_by_ssu, 1/pi_ssu)
  y_psu <- matrix(y_psu, ncol = 1, dimnames = list(rownames(y_psu),NULL))
  
  
  var_psu <- var_pois(y = y_psu, pik = pi_psu[rownames(y_psu)], id = rownames(y_psu))
  
  q_psu <- var_pois(y = NULL, pik = pi_psu[rownames(y_psu)], id = rownames(y_psu))$diago
  pond <- stats::setNames((1/(pi_psu^2) - q_psu[names(pi_psu)]), names(pi_psu))
  
  
  pikls <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    n <- length(ind)
    N_in_each_psu <- n/unique(pi_ssu[ind])
    mat_pij <- matrix((n*(n-1))/(N_in_each_psu*(N_in_each_psu-1)), nrow = n, ncol = n)
    diag(mat_pij) <- n/N_in_each_psu
    return(mat_pij)
  })
  names(pikls) <- unique(psu_by_ssu)
  
  var_ssu <- sapply(X = unique(psu_by_ssu),
                    FUN = function(psu_name){
                      ind <- which(psu_by_ssu == psu_name);
                      varSYG(y = y_ssu[ind, , drop = FALSE], 
                             pikl = pikls[[psu_name]],
                             w = pond[psu_name])}
  )
  
  var_manually <- var_psu + sum(var_ssu)
  
  
  
  #Tests
  res1 <- var_Rao(y = NULL, 
                  description_psu = descrip1, 
                  description_ssu = descrip2, 
                  id = id_ssu)
  
  var_with_Rao <- var_Rao(y, precalc = res1)
  
  expect_equal(var_with_Rao, var_manually)
})

test_that("var_Rao works well - DT/SRS/Poisson",{
  #PSU sampling : DT on x
  #SSU sampling : a SRSWOR with N = 100 and a 
  #sampling fraction in \{0.1,0.2,0.5,0.6\}
  #TSU sampling : a Poisson scheme
  #Define PSU and SSU sample
  n_psu <- 10
  nb_bal_var <- 5
  n_per_psu <- sample(5:10, n_psu, replace = TRUE)
  id_psu <- paste0("id_psu", 1:n_psu)
  x_bal <- matrix(rnorm(n_psu*nb_bal_var), ncol = nb_bal_var)
  rownames(x_bal) <- id_psu
  #psu_by_ssu is a ssu size sample that contains for each ssu, 
  #the label of the psu to which it belongs.
  psu_by_ssu <- rep(paste0("id_psu",1:n_psu), n_per_psu)
  
  id_ssu <- paste0("id_ssu", 1:sum(n_per_psu))
  psu_by_ssu <- setNames(psu_by_ssu, id_ssu)
  pi_psu <- stats::setNames(runif(length(id_psu), 0.2, 0.8), id_psu)
  pi_ssu <- stats::setNames(sample(c(0.1,0.2,0.5,0.6), replace = TRUE, length(id_psu)), id_psu)
  pi_ssu <- pi_ssu[psu_by_ssu]
  pi_ssu <- setNames(pi_ssu, id_ssu)
  
  n_per_ssu <- sample(3:10, length(id_ssu), replace = TRUE)
  ssu_by_tsu <- rep(id_ssu, n_per_ssu)
  id_tsu <- paste0("id_tsu", 1:sum(n_per_ssu))
  ssu_by_tsu <- setNames(ssu_by_tsu, id_tsu)
  pi_tsu <- stats::setNames(runif(length(id_tsu), 0.2, 0.8), id_tsu)
  
  
  #Compute descriptions
  precalc1 <- varDT(y = NULL,
                    pik = pi_psu,
                    x = x_bal,
                    id = id_psu)
  descrip1 <- list("var_fn" = "varDT",
                   "precalc" = precalc1)
  
  precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
    ind <- which(psu_by_ssu == psu_name);
    return(list("var_fn" = "var_srs",
                "precalc" = var_srs(y = NULL,
                                    pik = pi_ssu[ind],
                                    id = id_ssu[ind])))
  })
  names(precalc2) <- unique(psu_by_ssu)
  descrip2 <- precalc2
  
  # precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
  #   ind <- which(psu_by_ssu == psu_name);
  #   return(list("var_fn" = "var_pois",
  #               "precalc" = var_pois(y = NULL,
  #                                    pik = rep(1,length(ind)),
  #                                    id = id_ssu[ind])))
  # })
  # names(precalc2) <- unique(psu_by_ssu)
  # descrip2 <- precalc2
  
  precalc3 <- lapply(X = unique(ssu_by_tsu), FUN = function(ssu_name){
    ind <- which(ssu_by_tsu == ssu_name);
    return(list("var_fn" = "var_pois",
                "precalc" = var_pois(y = NULL,
                                     pik = pi_tsu[ind],
                                     id = id_tsu[ind])))
  })
  names(precalc3) <- unique(ssu_by_tsu)
  descrip3 <- precalc3
  
  
  #Compute the resulting variance manually
  y <- as.matrix(rnorm(sum(n_per_ssu)), ncol = 1)
  rownames(y) <- id_tsu
  
  y_ssu <- sum_by(y, ssu_by_tsu, 1/pi_tsu)
  y_ssu <- y_ssu[id_ssu,,drop = FALSE]
  y_psu <- sum_by(y_ssu, psu_by_ssu, 1/(pi_ssu))
  y_psu <- matrix(y_psu, ncol = 1, dimnames = list(rownames(y_psu),NULL))
  
  
  var_psu <- varDT(y = NULL, 
                   pik = pi_psu[rownames(y_psu)], 
                   x = x_bal[rownames(y_psu), ,drop = FALSE],
                   id = rownames(y_psu))
  
  q_psu <- var_psu$diago
  var_psu <- varDT(y = y_psu, precalc = var_psu)
  pond_psu <- stats::setNames((1/(pi_psu^2) - q_psu[names(pi_psu)]), names(pi_psu))
  
  
  var_ssu <- var_srs(y = y_ssu,
                     pik = pi_ssu[rownames(y_ssu)], 
                     strata = psu_by_ssu[rownames(y_ssu)],
                     w = pond_psu[psu_by_ssu[rownames(y_ssu)]])
  
  qu <- setNames(q_psu[psu_by_ssu], names(psu_by_ssu)) #ici ^pb
  pi_u <- setNames(pi_psu[psu_by_ssu], names(psu_by_ssu))
  pi_i <- pi_ssu[names(psu_by_ssu)]
  nu <- setNames(setNames(n_per_psu, id_psu)[psu_by_ssu], names(psu_by_ssu))
  qu_upus <- qu/(pi_i^2) + ((1/(pi_u^2)) - qu)*(1-pi_i)*(nu/(nu))/(pi_i^2)
  pond_psu_ssu <- (1/(setNames(pi_psu[psu_by_ssu], id_ssu)[names(qu_upus)]*pi_ssu[names(qu_upus)])^2) - 
    qu_upus 
  var_tsu <- var_pois(y = y,
                      pik = pi_tsu[rownames(y)], 
                      w = pond_psu_ssu[ssu_by_tsu[rownames(y)]])
  
  var_tot <- var_psu + var_ssu + var_tsu
  
  #Tests
  res1 <- var_Rao_multiple(y = NULL,
                           description1 = descrip1,
                           description2 = descrip2,
                           description3 = descrip3)
  
  var_with_Rao <- var_Rao_multiple(y, precalc = res1)
  
  expect_equal(var_with_Rao, var_tot)
})
