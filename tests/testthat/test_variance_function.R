
N <- 1000
n <- 10
pikl <- matrix(rep(n*(n - 1) / (N * (N - 1)), n^2), ncol = n)
diag(pikl) <- rep(n/N, rep = n)
y <- rnorm(n)

test_that("varDT works", {
  expect_error({
    varDT(y, pik = diag(pikl))
    precalcDT <- varDT(y = NULL, pik = diag(pikl))
    varDT(y = y, precalc = precalcDT)
  }, regexp = NA)
})

test_that("varYG works", {
  expect_error({
    varSYG(y, pikl = pikl)
    precalcSYG <- varSYG(y = NULL, pikl = pikl)
    varSYG(y = y, precalc = precalcSYG)
  }, regexp = NA)
})


# x <- Matrix(rnorm(n*5), ncol = 5)
# y <- matrix(y)
# dimnames(y) <- list(letters[1:10], NULL)
# rescal(y, x)



# Tests
# set.seed(1); n <- 2332; q <- 1; p <- 14; H <- 22; y <- matrix(rnorm(q*n),ncol=q); pik <- runif(n); x <- matrix(rnorm(p*n),ncol=p); x <- cbind(x, x[, 1]); strata <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)]; inv <- NULL; w <- NULL
# precalc <- varDT(pik = pik, x = x, strata = strata)$precalc
# t <- varDT(pik = pik, x = x, strata = strata)
# microbenchmark(times = 10, varDT(y, pik = pik, x = x, strata = strata), varDT(y, precalc = precalc))
# diago0 <- varDT(pik = pik, x = x, strata = strata)$diago
# abs(varDT(y,pik,x = pik) - varD(y,pik))
# abs(varDT(y,pik) - varDT(y,pik,strata = rep(1,NROW(y))))
# abs(varDT(y,pik,strata = strata) - varDst(y,pik,strata))
# abs(varDT(y,pik,x) - varDT2(y,pik,x))
# varDT(y, pik, x = x, strata = strata, w = rnorm(n))

# max(abs(varDT(y = NULL,pik,strata = strata)[[1]] - varDst(y = NULL,pik,strata = strata)))

# set.seed(1); n <- 100; N <- 1000; pik <- rep(n/N,n); x <- pik; strata <- rep(1,n)
# varDT(y = NULL,pik,x = x, strata = strata)


# varDT(y,pik,x,strata = strata) / varDT(y,pik,strata = strata, x)

# library(microbenchmark)
# n <- 2600; q <- 1; p <- 15; H <- 22; y <- matrix(rnorm(q*n),ncol=q); pik <- runif(n); x <- matrix(c(pik,rnorm((p-1)*n)),ncol=p); strata <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)]; inv <- NULL; w <- NULL
# microbenchmark(varDT(y,pik, x = x, strata = strata), times = 10)
# n <- 80000; q <- 100; p <- 1; H <- 2600; y <- matrix(rnorm(q*n),ncol=q); pik <- runif(n); x <- matrix(c(pik,rnorm((p-1)*n)),ncol=p); strata <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)]; inv <- NULL; w <- NULL
# microbenchmark(varDT(y,pik, strata = strata), times = 10)
# n <- 20000; q <- 100; p <- 1; H <- 600; y <- matrix(rnorm(q*n),ncol=q); pik <- runif(n); x <- matrix(c(pik,rnorm((p-1)*n)),ncol=p); strata <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)]; inv <- NULL; w <- NULL
# inv <- varDT(y = NULL,pik)$inv
# microbenchmark(varDT(y,pik,strata = strata,inv = inv),varDst(y,pik,strata = strata), times = 1)



# Banc de tests
# N <- 100
# n <- 10
# y <- rnorm(n)
# pik <- rep(n/N,n)
# pikl <- matrix(rep((n*(n-1))/(N*(N-1))),ncol=n,nrow=10)
# diag(pikl) <- pik
# varYG(y, pikl = pikl)
# varDT(y, pik = pik)
#
# y <- c(4,2)
# pikl <- matrix(c(0.1111,0.1111,0,0,0.1111,1,0.3333,0.5556,0,0.3333,0.3333,0,0,0.5556,0,0.5556),ncol=4)[1:2,1:2]
# pik <- diag(pikl)
# varYG(y, pikl = pikl)
# varD(y, pik = pik)
