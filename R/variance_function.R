

#' Linear Regression Residuals Calculation
#'
#' @description \code{rescal} calculates linear regression residuals in an 
#' efficient way : handling several dependant variables at a time, using 
#' Matrix::TsparseMatrix capabilities and allowing for precalculation of 
#' the matrix inverse.
#'
#' @param y A numerical matrix of dependant variable(s). May be a 
#' Matrix::TsparseMatrix.
#' @param x A numerical matrix of independant variable(s). May be a 
#' Matrix::TsparseMatrix.
#' @param w An optional numerical vector of row weights.
#' @param by An optional categorical vector (factor or character)
#' when residuals calculation is to be conducted within by-groups 
#' (see Details).
#' @param colinearity.check A boolean (\code{TRUE} or \code{FALSE}) or 
#' \code{NULL} indicating whether to perform a check for colinearity or 
#' not (see Details).
#' @param precalc A list of precalculated results (see Details).
#'
#' @details In the context of the \code{gustave} package, linear 
#' regression residual calculation is solely used to take into account 
#' the effect of calibration on variance estimation. Independant variables 
#' are therefore most likely to be the same from one variance estimation 
#' to another, hence the inversion of the matrix 
#' \code{t(x) \%*\% Diagonal(x = w) \%*\% x} can be done once and for all 
#' at a precalculation step.
#'
#' The parameters \code{y} and \code{precalc} determine whether a list of 
#' precalculated data should be used in order to speed up the regression 
#' residuals computation at execution time:
#' \itemize{
#'  \item if \code{y} not \code{NULL} and \code{precalc} \code{NULL} : 
#'  on-the-fly calculation of the matrix inverse and the regression residuals 
#'  (no precalculation).
#'  \item if \code{y} \code{NULL} and \code{precalc} \code{NULL} : 
#'  precalculation of the matrix inverse which is stored in a list of 
#'  precalculated data.
#'  \item if \code{y} not \code{NULL} and \code{precalc} not \code{NULL} : 
#'  calculation of the regression residuals using the list of precalculated 
#'  data.
#' }
#'
#' The \code{by} paramater allows for calculation within by-groups : all 
#' calculation are made separately for each by-group (when calibration was 
#' conducted separately on several subsamples), but in an efficient way using 
#' Matrix::TsparseMatrix capabilities (especiellay when the matrix inverse is 
#' precalculated).
#' 
#' If \code{colinearity.check} is \code{NULL}, a test for colinearity in the 
#' independant variables (\code{x}) is conducted if and only if \code{det(t(x)
#' \%*\% x) == 0}.
#' 
#'
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical matrix with same structure (regular base::matrix or
#'   Matrix::TsparseMatrix) and dimensions as \code{y}. \item if \code{y} is
#'   \code{NULL} (precalculation step) : a list containing precalculated data:
#'   \itemize{ \item \code{x}: the numerical matrix of independant variables. 
#'   \item \code{w}: the numerical vector of row weights (vector of 1 by
#'   default). \item \code{inv}: the inverse of \code{t(x) \%*\%
#'   Matrix::Diagonal(x = w) \%*\% x} } }
#'   
#' @author Martin Chevalier (Insee)
#'
#' @seealso \code{\link{block_matrix}} \code{\link{varDT}}
#'
#' @examples # Generating random data
#' set.seed(1)
#' n <- 100
#' H <- 5
#' y <- matrix(rnorm(2*n), nrow = n)
#' x <- matrix(rnorm(10*n), nrow = n)
#' by <- letters[sample(1:H, n, replace = TRUE)]
#'
#' # Direct calculation
#' rescal(y, x)
#'
#' # Calculation with precalculated data
#' precalc <- rescal(y = NULL, x)
#' rescal(y, precalc = precalc)
#' identical(rescal(y, x), rescal(y, precalc = precalc))
#'
#' # Colinearity check
#' rescal(y, cbind(x, x[, 1]), colinearity.check = TRUE)
#'
#' # Matrix::TsparseMatrix capability
#' require(Matrix)
#' X <- as(x, "TsparseMatrix")
#' Y <- as(y, "TsparseMatrix")
#' rescal(Y, X)
#'
#' # by parameter for within by-groups calculation
#' rescal(Y, X, by = by)
#' identical(
#'  rescal(Y, X, by = by)[by == "a", ]
#'  , rescal(Y[by == "a", ], X[by == "a", ])
#' )
#'
#' @export rescal

rescal <- function(y = NULL, x, w = NULL, by = NULL, colinearity.check = NULL, precalc = NULL){

  if(is.null(precalc)){

    if(is.null(w)) w <- rep(1, NROW(x))

    # Taking the by into account
    if(!is.null(by)) x <- block_matrix(x, by)$y

    # Checking for colinearity
    if(isTRUE(colinearity.check) || (is.null(colinearity.check) && det(t(x) %*% x) == 0)){
      t <- as.vector(is.na(stats::lm(rep(1, NROW(x)) ~ . - 1, data = as.data.frame(as.matrix(x)))$coef))
      if(any(t)) warning("Some variables in x where discarted due to colinearity.")
      x <- x[, !t]
    }

    # Matrix inversion
    inv <- solve(t(x) %*% Matrix::Diagonal(x = w) %*% x)

  }else list2env(precalc, envir = environment())

  if(is.null(y)){
    return(list(x = x, w = w, inv = inv))
  }else{
    e <- y - x %*% ( inv  %*% (t(x) %*% Matrix::Diagonal(x = w) %*% y) )
    if(class(e) != class(y)) e <- methods::as(e, class(y))
    return(e)
  }

}

# n <- 70000; p <- 10; q <- 150; H <- 6; y <- matrix(rnorm(n*p),ncol=p); x <- Matrix(rnorm(n*q)*(runif(n*q) > 0.98),ncol=q); w <- runif(n); by <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)];
# precalc <- rescal(y = NULL, x = x, w = w, by = by)
# microbenchmark(times = 10, rescal(y, precalc = precalc), rescal(y, x = x, w = w, by = by))
# inv <- ginv(as.matrix(t(x * w) %*% x))
# t2 <- resCalib(y,x,w,inv)
# identical(t,t2)
# microbenchmark(rescal(y,x,w),rescal(y,x,w,inv),times = 10)

#' Variance approximation with Deville-Tillé (2005) formula
#' 
#' @description \code{varDT} estimates the variance of the total in the case of
#'   a balanced sampling design with equal or unequal probabilities. Without 
#'   balancing variables, it falls back to Deville's (1993) classical
#'   approximation. Without balancing variables and with equal probabilities, it
#'   falls back to the classical variance estimator for the total in the case of
#'   simple random sampling. Stratification is natively supported.
#'   
#' @param y A numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated. variable(s). May be a Matrix::TsparseMatrix.
#' @param pik A numerical vector of first-order inclusion probabilties.
#' @param x An optional numerical matrix of balancing variable(s). May be a
#'   Matrix::TsparseMatrix.
#' @param strata An optional categorical vector (factor or character) when
#'   variance estimation is to be conducted within strata.
#' @param w An optional numerical vector of row weights (see Details).
#' @param colinearity.check A boolean (\code{TRUE} or \code{FALSE}) or
#'   \code{NULL} indicating whether to perform a check for colinearity or not
#'   (see Details).
#' @param precalc A list of precalculated results (see Details).
#'   
#' @details \code{varDT} is the workhorse of most variance estimation conducted
#'   with the \code{gustave} package. It might be used to estimate the variance
#'   of a total in the case of (stratified) simple random sampling, (stratified)
#'   unequal probability sampling and (stratified) balanced sampling. The native
#'   integration of stratification based on Matrix::TsparseMatrix allows for
#'   significant performance gains compared to higher level vectorizations
#'   (\code{*apply} especially).
#'   
#'   Several time-consuming operations (e.g. colinearity-check, matrix
#'   inversion) can be precalculated in order to speed up the estimation at
#'   execution time. This is determined by the value of the parameters \code{y}
#'   and \code{precalc}: \itemize{ \item if \code{y} not \code{NULL} and
#'   \code{precalc} \code{NULL} : on-the-fly calculation (no precalculation). 
#'   \item if \code{y} \code{NULL} and \code{precalc} \code{NULL} :
#'   precalculation whose results are stored in a list of precalculated data. 
#'   \item if \code{y} not \code{NULL} and \code{precalc} not \code{NULL} :
#'   calculation using the list of precalculated data. }
#'   
#'   If \code{colinearity.check} is \code{NULL}, a test for colinearity in the
#'   independant variables (\code{x}) is conducted only if \code{det(t(x) \%*\%
#'   x) == 0)}.
#'   
#'   \code{w} is a row weight used at the final summation step. It is useful
#'   when \code{varDT} is used on the second stage of a two-stage sampling
#'   design applying the Rao (1975) formula.
#'   
#' @section Difference with \code{varest}:
#'   
#'   \code{varDT} differs from \code{sampling::varest} in several ways: 
#'   \itemize{ \item The formula implemented in \code{varDT} is more general and
#'   encompasses balanced sampling. \item \code{varDT} does not natively
#'   implements the calibration estimator (i.e. the sampling variance estimator
#'   that takes into account the effect of calibration). In the context of the
#'   \code{gustave} package, \code{\link{rescal}} could be called before 
#'   \code{varDT} in order to achieve the same result. \item Even in its reduced
#'   form (without balancing variables), the formula implemented in \code{varDT}
#'   slightly differs from the one implemented in \code{sampling::varest}.
#'   Caron, Deville and Sautory (1998, pp. 7-8) compares the two estimators
#'   (\code{sampling::varest} implements V_2, \code{varDT} implements V_1). 
#'   \item \code{varDT} introduces several optimizations: \itemize{ \item
#'   matrixwise operations allow to estimate variance on several interest
#'   variables at once \item Matrix::TsparseMatrix capability and the native
#'   integration of stratification induce significant performance gains. \item
#'   the ability to precalculate some time-consuming operations speeds up the
#'   estimation at execution time. } }
#'   
#'   
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical vector of sizz the number of columns of y. \item if \code{y} is
#'   \code{NULL} (precalculation step) : a list containing precalculated data:
#'   \itemize{ \item \code{pik}: the numerical vector of first-order inclusion
#'   probabilities. \item \code{A}: the numerical matrix denoted A in (Deville,
#'   Tillé, 2005). \item \code{ck}: the numerical vector denoted ck2 in
#'   (Deville, Tillé, 2005). \item \code{inv}: the inverse of \code{A \%*\%
#'   Matrix::Diagonal(x = ck) \%*\% t(A)} \item \code{diago}: the diagonal term
#'   of the variance estimator } }
#'   
#' @author Martin Chevalier (Insee)
#'   
#' @seealso \code{\link{block_matrix}} \code{\link{rescal}}
#'   
#' @references Caron N., Deville J.-C., Sautory O. (1998), \emph{Estimation de
#'   précision de données issues d'enquêtes : document méthodologique sur le
#'   logiciel POULPE}, Insee working paper, n°9806
#'   
#'   Deville, J.-C. (1993), \emph{Estimation de la variance pour les enquêtes en
#'   deux phases}, Manuscript, INSEE, Paris.
#'   
#'   Deville, J.-C., Tille, Y. (2005), "Variance approximation under balanced
#'   sampling", \emph{Journal of Statistical Planning and Inference}, 128, issue
#'   2 569-591
#'   
#'   Rao, J.N.K (1975), "Unbiased variance estimation for multistage designs",
#'   \emph{Sankhya}, C n°37
#'
#' @examples require(sampling)
#' set.seed(1)
#'
#' # Simple random sampling case
#' N <- 1000
#' n <- 100
#' y <- rnorm(N)[as.logical(sampling::srswor(n, N))]
#' pik <- rep(n/N, n)
#' varDT(y, pik)
#' sampling::varest(y, pik = pik)
#' N^2 * (1 - n/N) * var(y) / n
#'
#' # Unequal probability sampling case
#' N <- 1000
#' n <- 100
#' pik <- runif(N)
#' s <- as.logical(sampling::UPsystematic(pik))
#' y <- rnorm(N)[s]
#' pik <- pik[s]
#' varDT(y, pik)
#' sampling::varest(y, pik = pik)
#' # The small difference is expected (see above).
#'
#' # Balanced sampling case
#' N <- 1000
#' n <- 100
#' pik <- runif(N)
#' x <- matrix(rnorm(N*3), ncol = 3)
#' s <- as.logical(sampling::samplecube(x, pik))
#' y <- rnorm(N)[s]
#' pik <- pik[s]
#' x <- x[s, ]
#' varDT(y, pik, x)
#'
#' # Balanced sampling case (variable of interest
#' # among the balancing variables)
#' N <- 1000
#' n <- 100
#' pik <- runif(N)
#' y <- rnorm(N)
#' x <- cbind(matrix(rnorm(N*3), ncol = 3), y)
#' s <- as.logical(sampling::samplecube(x, pik))
#' y <- y[s]
#' pik <- pik[s]
#' x <- x[s, ]
#' varDT(y, pik, x)
#' # As expected, the total of the variable of interest is perfectly estimated.
#'
#' # strata argument
#' n <- 100
#' H <- 2
#' pik <- runif(n)
#' y <- rnorm(n)
#' strata <- letters[sample.int(H, n, replace = TRUE)]
#' all.equal(
#'  varDT(y, pik, strata = strata)
#'  , varDT(y[strata == "a"], pik[strata == "a"]) + varDT(y[strata == "b"], pik[strata == "b"])
#' )
#'
#' # precalc argument
#' n <- 1000
#' H <- 50
#' pik <- runif(n)
#' y <- rnorm(n)
#' strata <- sample.int(H, n, replace = TRUE)
#' precalc <- varDT(y = NULL, pik, strata = strata)
#' identical(
#'  varDT(y, precalc = precalc)
#'  , varDT(y, pik, strata = strata)
#' )
#'
#' @export varDT

varDT <- function(y = NULL, pik, x = NULL, strata = NULL, w = NULL, colinearity.check = NULL, precalc = NULL){

  # set.seed(1); n <- 2600; q <- 10; p <- 15; H <- 22; y <- matrix(rnorm(q*n),ncol=q); pik <- runif(n); x <- matrix(rnorm(p*n),ncol=p); x <- cbind(x, x[, 1]); strata <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)]; precalc <- NULL;
  # y = NULL; pik = bisect$piup; x = up_x; strata = bisect$reg; colinearity.check = TRUE

  if(is.null(precalc)){

    if(is.null(x)){
      x <- pik
      if(is.null(colinearity.check)) colinearity.check <- FALSE
    }
    p <- NCOL(x)

    # Stratification
    if(!is.null(strata)){
      t <- block_matrix(x, strata)
      x <- t$y
      bycol <- t$bycol
      t <- table(strata)
      n <- as.vector(t[match(strata, names(t))])
    }else{
      bycol <- rep(1, p)
      n <- length(pik)
    }

    # Checking for colinearity
    if(isTRUE(colinearity.check) || (is.null(colinearity.check) && det(t(x) %*% x) == 0)){
      t <- as.vector(is.na(stats::lm(rep(1, NROW(x)) ~ . - 1, data = as.data.frame(as.matrix(x)))$coef))
      t2 <- sumby(!t, bycol)
      x <- x[, !t]
      if(any(t)) warning("Some variables in x where discarted due to colinearity.")
      p <- as.vector(t2[match(strata, names(t2))])
    }

    # A, ck and inv terms
    A <- t(x / pik)
    ck <- (1 - pik) * n / pmax(n - p, 1)
    u <- A %*% Matrix::Diagonal(x = ck) %*% t(A)
    inv <- methods::as(if(det(u) != 0) solve(u) else MASS::ginv(as.matrix(u)),"TsparseMatrix")

  }else list2env(precalc, envir = environment())

  if(is.null(y)){
    # Diagonal term of the variance estimator
    diago <- ck * (1 - diag(t(A) %*% inv %*% A) * ck)/pik^2
    names(diago) <- names(pik)
    return(list(pik = pik, A = A, ck = ck, inv = inv, diago = diago))
  }else{
    if(is.null(w)) w <- rep(1, length(pik))
    z <- y / pik
    zhat <- t(A) %*% inv %*% (A %*% Matrix::Diagonal(x = ck) %*% z)
    return(colSums(ck * w * (z - zhat)^2))
  }

}
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




#' TODO
#' @references Wolter, 2008, p. 53

varCollapse <- function(y, group, strata = NULL){

  if(is.null(strata)) Ygh <- y else{
    Ygh <- sumby(y, by = strata)
    m <- match(row.names(Ygh), strata)
    group <- group[m]; strata <- strata[m]
  }

  n <- NROW(Ygh)
  t <- sumby(cbind(matrix(1,nrow = n),Ygh),by=group)
  m <- match(group,row.names(t))
  Lg <- t[m,1]
  Yg <- t[m,-1,drop = FALSE]

  r <- colSums((Lg/(Lg-1))*(Ygh-Yg/Lg)^2)

  return(r)

}


# Tests
# set.seed(1)
# n <- 20000
# p <- 10
# y <- matrix(rnorm(p*n),ncol=p)
# pik <- rep(1,n)
# strata <- sample(apply(expand.grid(letters,letters,letters),1,paste0,collapse = ""),n,replace = TRUE)
# # strata <- sample.int(26^2,n,replace = TRUE)
# w <- rbinom(n,500,0.5)
# valid <- !duplicated(strata)
# source("X:/HAB-Bases-UMS/MC/projets/#commun/R/collapse.R")
# group <- collapse(strata,w,valid)
#
# microbenchmark(
#   varC(y/w,group,strata = strata)
#   , varCollapse(y/w,group,strata = strata)
#   , times = 10
# )

#' TODO
#' @references Wolter, 2007, pp. 298-353, v12

varsys <- function(y, pik){
  n <- nrow(as.matrix(y))
  return(
    ( (n - sum(pik)) / ( 2 * (n - 1)) ) *
      colSums(
        as.matrix(as.matrix(y)[-n,]/pik[-n] -
                    as.matrix(y)[-1,]/pik[-1])^2
        ,na.rm = T)
  )
}

varsysst <- function(y, pik, strata=rep(1,nrow(as.matrix(y)))){

  o <- order(strata);
  pik <- pik[o]; strata <- strata[o]
  p <- ncol(as.matrix(y))
  y <- matrix(matrix(y,ncol=p)[o,],ncol=p)

  id <- cumsum(!duplicated(strata))
  H <- max(id)
  f <- !duplicated(strata)
  l <- rev(!duplicated(rev(strata)))

  n <- c(which(f)[-1],length(strata) + 1) - which(f)
  sumpik <- (cumsum(pik) - c(0,cumsum(pik)[which(l)][-H])[id])[l]

  return(
    colSums(
      (n[id[-which(f)]] - sumpik[id[-which(f)]])/(2 * (n[id[-which(f)]] - 1)) *
        ( matrix(matrix(matrix(y,ncol=p)[-which(l),],ncol=p)/pik[-which(l)],ncol=p) -
            matrix(matrix(matrix(y,ncol=p)[-which(f),],ncol=p)/pik[-which(f)],ncol=p))^2
    )
  )
}


# y <- as.matrix(Y[,paste0("y",1:10),with = F]);strata <- Y$idzae;pik <- Y$piLog
# strata <- rep(1,nrow(Y))
#


#' TODO

varB <- function(y, pik){
  return(colSums((1 - pik) * (as.matrix(y)/pik)^2))
}

#' TODO
#' @export
#' @references doc de travail M2015 3 Gros Moussallam p. 19

varYG <- function(y = NULL,pikl){
  pik = diag(pikl)
  delta <- 1 - pik%*%t(pik)/pikl
  if(is.null(y)){
    diago <- - (1/pik^2)*rowSums(delta - diag(x = diag(delta)))
    names(diago) <- row.names(pikl)
    return(diago)
  }else{
    var <- colSums((y/pik) * (delta %*% (y/pik)) - delta %*% (y/pik)^2)
    return(var)
  }
}

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


#' TODO
#' @export
#' @references Doc de travail M2015 3 Gros Moussallam p.11. et p. 19 pour la version matricielle

varGR <- function(y, alphakl, pikl = matrix(1, ncol = NCOL(alphakl), nrow = NROW(alphakl)), g = rep(0, NROW(alphakl))){
  alphak <- diag(alphakl)
  delta <- (alphakl - alphak %*% t(alphak)) / (alphakl * pikl)
  return(colSums((y/alphak) * (delta %*% (y/alphak)) ) - colSums(delta)%*%((y/alphak)^2) + sum((g/diag(pikl))*(y/alphak)))
}
