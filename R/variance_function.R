
#' Linear Regression Residuals Calculation
#'
#' @description \code{res_cal} calculates linear regression residuals in an 
#' efficient way : handling several dependent variables at a time, using 
#' Matrix::TsparseMatrix capabilities and allowing for pre-calculation of 
#' the matrix inverse.
#'
#' @param y A (sparse) numerical matrix of dependent variable(s).
#' @param x A (sparse) numerical matrix of independent variable(s).
#' @param w An optional numerical vector of row weights.
#' @param by An optional categorical vector (factor or character)
#' when residuals calculation is to be conducted within by-groups 
#' (see Details).
#' @param precalc A list of pre-calculated results (see Details).
#' @param id A vector of identifiers of the units used in the calculation.
#'   Useful when \code{precalc = TRUE} in order to assess whether the ordering of the
#'   \code{y} data matrix matches the one used at the precalculation step.
#'
#' @details In the context of the \code{gustave} package, linear 
#' regression residual calculation is solely used to take into account 
#' the effect of calibration on variance estimation. Independent variables 
#' are therefore most likely to be the same from one variance estimation 
#' to another, hence the inversion of the matrix 
#' \code{t(x) \%*\% Diagonal(x = w) \%*\% x} can be done once and for all 
#' at a pre-calculation step.
#'
#' The parameters \code{y} and \code{precalc} determine whether a list of 
#' pre-calculated data should be used in order to speed up the regression 
#' residuals computation at execution time:
#' \itemize{
#'  \item if \code{y} not \code{NULL} and \code{precalc} \code{NULL} : 
#'  on-the-fly calculation of the matrix inverse and the regression residuals 
#'  (no pre-calculation).
#'  \item if \code{y} \code{NULL} and \code{precalc} \code{NULL} : 
#'  pre-calculation of the matrix inverse which is stored in a list of 
#'  pre-calculated data.
#'  \item if \code{y} not \code{NULL} and \code{precalc} not \code{NULL} : 
#'  calculation of the regression residuals using the list of pre-calculated 
#'  data.
#' }
#'
#' The \code{by} parameter allows for calculation within by-groups : all 
#' calculation are made separately for each by-group (when calibration was 
#' conducted separately on several subsamples), but in an efficient way using 
#' Matrix::TsparseMatrix capabilities (especially when the matrix inverse is 
#' pre-calculated).
#' 
#'
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical matrix with same structure (regular base::matrix or
#'   Matrix::TsparseMatrix) and dimensions as \code{y}. \item if \code{y} is
#'   \code{NULL} (pre-calculation step) : a list containing pre-calculated data.}
#'   
#' @author Martin Chevalier
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
#' res_cal(y, x)
#'
#' # Calculation with pre-calculated data
#' precalc <- res_cal(y = NULL, x)
#' res_cal(y, precalc = precalc)
#' identical(res_cal(y, x), res_cal(y, precalc = precalc))
#'
#' # Matrix::TsparseMatrix capability
#' require(Matrix)
#' X <- as(x, "TsparseMatrix")
#' Y <- as(y, "TsparseMatrix")
#' identical(res_cal(y, x), as.matrix(res_cal(Y, X)))
#'
#' # by parameter for within by-groups calculation
#' res_cal(Y, X, by = by)
#' all.equal(
#'  res_cal(Y, X, by = by)[by == "a", ],
#'   res_cal(Y[by == "a", ], X[by == "a", ])
#' )
#'
#' @export

res_cal <- function(y = NULL, x, w = NULL, by = NULL, precalc = NULL, id = NULL){

  # by <- NULL; w <- NULL
  
  if(is.null(precalc)){

    if(is.null(w)) w <- rep(1, NROW(x))

    # Taking the by into account
    x <- coerce_to_TsparseMatrix(x)
    if(!is.null(by)) x <- detect_block(x, by) %||% make_block(x, by)

    # Determine the inverse of the t(x) %*% x matrix while removing colinear variables
    while(TRUE){
      u <- crossprod(x,  Matrix::Diagonal(x = w) %*% x)
      if(Matrix::rankMatrix(u, method = "qr") != NROW(u)){
        is_colinear <- as.vector(is.na(stats::lm.fit(x = as.matrix(u), y = rep(1, NROW(u)))$coef))
        if(any(is_colinear)) note("Some variables in x were discarded due to collinearity.")
        x <- x[, !is_colinear, drop = FALSE]
      }else break
    }
    inv <- solve(u)

  }else list2env(precalc, envir = environment())


  if(is.null(y)) return(list(x = x, w = w, inv = inv, id = id)) else {
    is_sparse_y <- inherits(y, c("Matrix"))
    is_matrix_y <- !is_sparse_y && inherits(y, c("matrix"))
    dimnames_y <- dimnames(y)
    if(!is.null(precalc) && !is.null(id) && !is.null(rownames(y)) && !identical(as.character(id), rownames(y))) stop(
      "The names of the data matrix (y argument) do not match the reference id (id argument)."
    )
    e <- y - x %*% ( inv  %*% crossprod(x,  Matrix::Diagonal(x = w) %*% y) )
    if(!is_sparse_y) e <- if(is_matrix_y) as.matrix(e) else as.vector(e)
    dimnames(e) <- dimnames_y
    e
  }

}

# n <- 70000; p <- 10; q <- 150; H <- 6; y <- matrix(rnorm(n*p),ncol=p); x <- Matrix(rnorm(n*q)*(runif(n*q) > 0.98),ncol=q); w <- runif(n); by <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)];
# precalc <- res_cal(y = NULL, x = x, w = w, by = by)
# microbenchmark(times = 10, res_cal(y, precalc = precalc), res_cal(y, x = x, w = w, by = by))
# inv <- ginv(as.matrix(t(x * w) %*% x))
# t2 <- res_calib(y,x,w,inv)
# identical(t,t2)
# microbenchmark(res_cal(y,x,w),res_cal(y,x,w,inv),times = 10)

#' Variance approximation with Deville-Tillé (2005) formula
#' 
#' @aliases varDT var_srs
#' 
#' @description \code{varDT} estimates the variance of the estimator of a total
#'   in the case of a balanced sampling design with equal or unequal probabilities 
#'   using Deville-Tillé (2005) formula. Without balancing variables, it falls back 
#'   to Deville's (1993) classical approximation. Without balancing variables and 
#'   with equal probabilities, it falls back to the classical Horvitz-Thompson 
#'   variance estimator for the total in the case of simple random sampling. 
#'   Stratification is natively supported.
#'   
#'   \code{var_srs} is a convenience wrapper for the (stratified) simple random
#'   sampling case.
#'   
#' @param y A (sparse) numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated. 
#' @param pik A numerical vector of first-order inclusion probabilities.
#' @param x An optional (sparse) numerical matrix of balancing variable(s).
#' @param strata An optional categorical vector (factor or character) when
#'   variance estimation is to be conducted within strata.
#' @param w An optional numerical vector of row weights (see Details).
#' @param precalc A list of pre-calculated results (see Details).
#' @param id A vector of identifiers of the units used in the calculation.
#'   Useful when \code{precalc = TRUE} in order to assess whether the ordering of the
#'   \code{y} data matrix matches the one used at the pre-calculation step.
#' @param keep_exh A logical vector of size 1. If \code{TRUE}, diagonal terms of the quadratic form
#' are returned with a 0 for each exhaustive units and probabilities for exhaustive units are remained in
#' \code{pik}, otherwise, those terms are discarded.
#'   
#' @details \code{varDT} aims at being the workhorse of most variance estimation conducted
#'   with the \code{gustave} package. It may be used to estimate the variance
#'   of the estimator of a total in the case of (stratified) simple random sampling, 
#'   (stratified) unequal probability sampling and (stratified) balanced sampling. 
#'   The native integration of stratification based on Matrix::TsparseMatrix allows 
#'   for significant performance gains compared to higher level vectorizations
#'   (\code{*apply} especially).
#'   
#'   Several time-consuming operations (e.g. collinearity-check, matrix
#'   inversion) can be pre-calculated in order to speed up the estimation at
#'   execution time. This is determined by the value of the parameters \code{y}
#'   and \code{precalc}: \itemize{ \item if \code{y} not \code{NULL} and
#'   \code{precalc} \code{NULL} : on-the-fly calculation (no pre-calculation). 
#'   \item if \code{y} \code{NULL} and \code{precalc} \code{NULL} :
#'   pre-calculation whose results are stored in a list of pre-calculated data. 
#'   \item if \code{y} not \code{NULL} and \code{precalc} not \code{NULL} :
#'   calculation using the list of pre-calculated data. }
#'   
#'   \code{w} is a row weight used at the final summation step. It is useful
#'   when \code{varDT} or \code{var_srs} are used on the second stage of a 
#'   two-stage sampling design applying the Rao (1975) formula.
#'   
#' @section Difference with \code{varest} from package \code{sampling}:
#'   
#'   \code{varDT} differs from \code{sampling::varest} in several ways: 
#'   \itemize{ \item The formula implemented in \code{varDT} is more general and
#'   encompasses balanced sampling. \item Even in its reduced
#'   form (without balancing variables), the formula implemented in \code{varDT}
#'   slightly differs from the one implemented in \code{sampling::varest}.
#'   Caron (1998, pp. 178-179) compares the two estimators
#'   (\code{sampling::varest} implements V_2, \code{varDT} implements V_1). 
#'   \item \code{varDT} introduces several optimizations: \itemize{ \item
#'   matrixwise operations allow to estimate variance on several interest
#'   variables at once \item Matrix::TsparseMatrix capability and the native
#'   integration of stratification yield significant performance gains. \item
#'   the ability to pre-calculate some time-consuming operations speeds up the
#'   estimation at execution time. } \item \code{varDT} does not natively
#'   implements the calibration estimator (i.e. the sampling variance estimator
#'   that takes into account the effect of calibration). In the context of the
#'   \code{gustave} package, \code{\link{res_cal}} should be called before 
#'   \code{varDT} in order to achieve the same result.}
#'   
#' @seealso \code{\link{res_cal}}
#' 
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : 
#'   the estimated variances as a numerical vector of size the number of 
#'   columns of y. \item if \code{y} is \code{NULL} (pre-calculation step) : a list 
#'   containing pre-calculated data.}
#'   
#' @author Martin Chevalier
#'   
#' @references Caron N. (1998), "Le logiciel Poulpe : aspects méthodologiques", \emph{Actes 
#'   des Journées de méthodologie statistique} \url{http://jms-insee.fr/jms1998s03_1/}
#'   Deville, J.-C. (1993), \emph{Estimation de la variance pour les enquêtes en
#'   deux phases}, Manuscript, INSEE, Paris.
#'   
#'   Deville, J.-C., Tillé, Y. (2005), "Variance approximation under balanced
#'   sampling", \emph{Journal of Statistical Planning and Inference}, 128, issue
#'   2 569-591
#'   
#'   Rao, J.N.K (1975), "Unbiased variance estimation for multistage designs",
#'   \emph{Sankhya}, C n°37
#'
#' @examples library(sampling)
#' set.seed(1)
#'
#' # Simple random sampling case
#' N <- 1000
#' n <- 100
#' y <- rnorm(N)[as.logical(srswor(n, N))]
#' pik <- rep(n/N, n)
#' varDT(y, pik)
#' sampling::varest(y, pik = pik)
#' N^2 * (1 - n/N) * var(y) / n
#'
#' # Unequal probability sampling case
#' N <- 1000
#' n <- 100
#' pik <- runif(N)
#' s <- as.logical(UPsystematic(pik))
#' y <- rnorm(N)[s]
#' pik <- pik[s]
#' varDT(y, pik)
#' varest(y, pik = pik)
#' # The small difference is expected (see Details).
#'
#' # Balanced sampling case
#' N <- 1000
#' n <- 100
#' pik <- runif(N)
#' x <- matrix(rnorm(N*3), ncol = 3)
#' s <- as.logical(samplecube(x, pik))
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
#' s <- as.logical(samplecube(x, pik))
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
#'  varDT(y, pik, strata = strata),
#'  varDT(y[strata == "a"], pik[strata == "a"]) + varDT(y[strata == "b"], pik[strata == "b"])
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
#'  varDT(y, precalc = precalc),
#'  varDT(y, pik, strata = strata)
#' )
#'
#' @export

varDT <- function(y = NULL, pik, x = NULL, strata = NULL, w = NULL, precalc = NULL, 
                  id = NULL, keep_exh = FALSE){
  
  if(is.null(precalc)){
    
    if(any(pik <= 0 | pik > 1)) stop("All pik must be in ]0;1]")
    
    exh <- pik == 1
    if(any(exh)) note(
      sum(exh), " units are exhaustive (pik = 1). They are discarded from the variance estimation process."
    )
    pik <- pik[!exh]
    
    x <- if(!is.null(x)) coerce_to_TsparseMatrix(x)[!exh, , drop = FALSE] else pik
    
    # Stratification
    if(!is.null(strata)){
      strata <- droplevels(as.factor(strata[!exh]))
      if(any(tapply(strata, strata, length) == 1, na.rm = TRUE))
        stop("Some strata contain less than 2 samples units.")
      x <- detect_block(x, strata) %||% make_block(x, strata)
      colby <- attr(x, "colby")
      rowby <- attr(x, "rowby")
    }else{
      colby <- rep("1", NCOL(x))
      rowby <- rep("1", NROW(x))
    }
    
    # Determine A, ck and inv terms while removing colinear variables
    n <- as.vector(tapply(rowby, rowby, length)[rowby])
    A <- t(x) %*% Diagonal(x = 1 / pik)
    while(TRUE){
      p <- as.vector(tapply(colby, colby, length)[rowby])
      ck <- (1 - pik) * n / pmax(n - p, 1)
      u <- tcrossprod(A %*% Matrix::Diagonal(x = ck), A)
      if(Matrix::rankMatrix(u, method = "qr") != NROW(u)){
        # TODO: See if tol = 1e-12 in rankMatrix does not solve some issues
        is_colinear <- as.vector(is.na(stats::lm.fit(x = as.matrix(u), y = rep(1, NROW(u)))$coef))
        if(any(is_colinear)) note("Some variables in x were discarded due to collinearity.")
        A <- A[!is_colinear, , drop = FALSE]
        colby <- colby[!is_colinear]
      }else break
    }
    inv <- solve(u)
    
  }else list2env(precalc, envir = environment())
  
  if(is.null(y)){
    # Diagonal term of the variance estimator
    diago <- ck * (1 - colSums(A * (inv %*% A)) * ck)/pik^2
    names(diago) <- names(pik)
    
    if(!is.null(w)) stop("w is not to be included in the precalculated data.")
    
    if(keep_exh){
      pik_full <- rep(0, length(exh))
      pik_full[which(!exh)] <- pik
      pik <- pik_full
      
      diago_full <- rep(0, length(exh))
      diago_full[which(!exh)] <- diago
      diago <- diago_full
    }
    
    precalc <- list(id = id, pik = pik, exh = exh, A = A, ck = ck, 
                    inv = inv, diago = diago, keep_exh = keep_exh)
    
    return(precalc)
  }else{
    y <- coerce_to_TsparseMatrix(y)
    if(!is.null(precalc) && !is.null(id) && !is.null(rownames(y)) && !identical(as.character(id), rownames(y))) stop(
      "The names of the data matrix (y argument) do not match the reference id (id argument)."
    )
    if(keep_exh){
      pik <- pik[which(!exh)]
    }
    if(is.null(w)) w <- rep(1, length(pik))
    z <- Diagonal(x = 1 / pik) %*% y[!exh, , drop = FALSE]
    zhat <- t(A) %*% inv %*% (A %*% Matrix::Diagonal(x = ck) %*% z)
    return(Matrix::colSums(ck * w * (z - zhat)^2))
  }
  
}



#' @rdname varDT
#' @export
var_srs <- function(y, pik, strata = NULL, w = NULL, precalc = NULL, id = NULL, keep_exh = FALSE){
  if(is.null(precalc) && !is.null(strata) && any(tapply(pik, strata, stats::sd) > 1e-6, na.rm = TRUE))
    stop("First-order inclusion probabilities are not equal (within strata if any).")
  varDT(y = y, pik = pik, x = NULL, strata = strata, w = w, precalc = precalc, id = id, keep_exh = FALSE)
}



#' Variance estimator for a Poisson sampling design
#' 
#' @description \code{var_pois} estimates the variance of the estimator 
#' of a total for a Poisson sampling design.
#' 
#' @param y A (sparse) numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated.
#' @param pik A numerical vector of first-order inclusion probabilities.
#' @param w An optional numerical vector of row weights (see Details).
#' @param id A vector of identifiers of the units used in the calculation.
#'   Useful when \code{precalc = TRUE} in order to assess whether the ordering of the
#'   \code{y} data matrix matches the one used at the pre-calculation step.
#' @param precalc A list of pre-calculated results (see Details).
#' 
#' @details \code{w} is a row weight used at the final summation step. It is useful
#'   when \code{var_pois} is used on the second stage of a two-stage sampling
#'   design applying the Rao (1975) formula.
#' 
#' @return The estimated variances as a numerical vector of size the number of 
#'   columns of \code{y}. 
#'    
#' @author Martin Chevalier
#'   
#' @references Rao, J.N.K (1975), "Unbiased variance estimation for multistage designs",
#'   \emph{Sankhya}, C n°37

#' @export
var_pois <- function(y = NULL, pik, w = rep(1, length(pik)), precalc = NULL, id = NULL){
  if(!is.null(precalc)){
    list2env(precalc, envir = environment())
  } 
  if(is.null(y)){
    diago <- (1-pik)/pik^2
    names(diago) <- names(pik)
    return(list(pik = pik, diago = diago, id = id))
  }else{
    if(!is.null(precalc) && !is.null(id) && !is.null(rownames(y)) && !identical(as.character(id), rownames(y))) stop(
      "The names of the data matrix (y argument) do not match the reference id (id argument)."
    )
    var <- colSums(w * (1 - pik) * (y / pik)^2)
    return(var)
  }
}


#' Sen-Yates-Grundy variance estimator
#' 
#' @description \code{varSYG} computes the Sen-Yates-Grundy 
#' variance estimator.
#' 
#' @param y A (sparse) numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated.
#' @param pikl A numerical matrix of second-order inclusion probabilities.
#' @param w An optional numerical vector of row weights (see Details).
#' @param precalc A list of pre-calculated results (analogous to the one used by 
#'   \code{\link{varDT}}).
#' @param id A vector of identifiers of the units used in the calculation.
#'   Useful when \code{precalc = TRUE} in order to assess whether the ordering of the
#'   \code{y} data matrix matches the one used at the pre-calculation step.
#'   
#' @details \code{varSYG} aims at being an efficient implementation of the 
#'   Sen-Yates-Grundy variance estimator for sampling designs with fixed sample 
#'   size. It should be especially useful when several variance estimations are
#'   to be conducted, as it relies on (sparse) matrix linear algebra.
#' 
#'   Moreover, in order to be consistent with \code{\link{varDT}}, \code{varSYG}
#'   has a \code{precalc} argument allowing for the re-use of intermediary
#'   results calculated once and for all in a pre-calculation step (see 
#'   \code{\link{varDT}} for details).
#'   
#' @section Difference with \code{varHT} from package \code{sampling}:
#'   
#'   \code{varSYG} differs from \code{sampling::varHT} in several ways: 
#'   \itemize{ \item The formula implemented in \code{varSYG} is solely
#'   the Sen-Yates-Grundy estimator, which is the one calculated 
#'   by \code{varHT} when method = 2.
#'   \item \code{varSYG} introduces several optimizations: \itemize{ \item
#'   matrixwise operations allow to estimate variance on several interest
#'   variables at once \item Matrix::TsparseMatrix capability yields significant 
#'   performance gains.}}
#' 
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical vector of size the number of columns of y. \item if \code{y} is
#'   \code{NULL} (pre-calculation step) : a list containing pre-calculated data 
#'   (analogous to the one used by \code{\link{varDT}}).}
#' 
#' @author Martin Chevalier
#' 
#' @examples library(sampling)
#' set.seed(1)
#' 
#' # Simple random sampling case
#' N <- 1000
#' n <- 100
#' y <- rnorm(N)[as.logical(srswor(n, N))]
#' pikl <- matrix(rep((n*(n-1))/(N*(N-1)), n*n), nrow = n)
#' diag(pikl) <- rep(n/N, n)
#' varSYG(y, pikl)
#' sampling::varHT(y = y, pikl = pikl, method = 2)

#' @export

varSYG <- function (y = NULL, pikl, w = rep(1, length(pik)), precalc = NULL, id = NULL){
  if(is.null(precalc)){
    pik = diag(pikl)
    delta <- 1 - pik %*% t(pik)/pikl
  }else list2env(precalc, envir = environment())
  if(is.null(y)){
    diago <- -(1/pik^2) * rowSums(delta - diag(x = diag(delta)))
    names(diago) <- row.names(pikl)
    return(list(pikl = pikl, pik = pik, delta = delta, diago = diago, id = id, w = w))
  }else{
    if(!is.null(precalc) && !is.null(id) && !is.null(rownames(y)) && !identical(as.character(id), rownames(y))) stop(
      "The names of the data matrix (y argument) do not match the reference id (id argument)."
    )
    var <- colSums(w*((y/pik) * (delta %*% (y/pik)) - delta %*% (y/pik)^2))
    return(var)
  }
}

# TODO: Add a varHT() estimator 

#' Variance estimation with Wolter's estimator.
#' 
#' Wolter's estimator provides a variance estimator of the Horvitz-Thompson total estimator 
#' under a stratified SRSWOR scheme while a single unit is drawn in some stratum.
#' This estimator is conservative under some assumption : it leads to an upper bound
#' of the classical variance estimator under a SRSWOR scheme.
#'
#' @param y A (sparse) numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated. 
#' @param old_strata A character vector that contains for each unit the name of the stratum 
#' before collapsing.
#' @param new_strata A character vector that contains for each unit :
#' \itemize{\item the name of the collapsed stratum.
#' \item \code{NA} if the unit is not collapsed.}
#' @param psu_id An optionnal character vector that contains for each unit, 
#'  the name of the psu to which it belongs. Usefull when using \code{var_Rao_multiple}.
#' @param pik A numerical vector of first-order inclusion probabilities.
#' @param w An optional numerical vector of row weights (see Details).
#' @param precalc A list of pre-calculated results (see Details).
#' @param id A vector of identifiers of the units used in the calculation.
#'   Useful when \code{precalc = TRUE} in order to assess whether the ordering of the
#'   \code{y} data matrix matches the one used at the pre-calculation step.
#'
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical vector of size the number of columns of y. \item if \code{y} is
#'   \code{NULL} (pre-calculation step) : a list containing pre-calculated data 
#'   (analogous to the one used by \code{\link{varDT}}).}
#'   
#' @details \code{var_wolter} provides an estimator of the variance after collapsing units. 
#' Each collapse unit must contain at least one unit. 
#' 
#' Moreover, in order to be consistent with \code{\link{varDT}}, \code{var_wolter}
#'   has a \code{precalc} argument allowing for the re-use of intermediary
#'   results calculated once and for all in a pre-calculation step (see 
#'   \code{\link{varDT}} for details).
#'   
#' @export
#' 
#' @author Khaled Larbi
#'
#' @references Wolter K (2008), "Introduction to Variance Estimation", \emph{Springer}
#'
#' @examples
#' #Units collapsed
#' n_units_coll <- 50L
#' strat_before_coll <- paste0("coll_",1:n_units_coll)
#' #Collapse all n_units_coll into 3 collapses stratum (A, B and C)
#' strat_after_coll <- sample(c("A","B","C"), n_units_coll, replace = TRUE)
#' 
#' #Units non collapsed
#' n_units_non_coll <- 200L
#' strat_before_non_coll <- rep(as.character(1:5), 40)
#' strat_after_non_coll <- rep(NA, n_units_non_coll)
#' 
#' old_strata <- c(strat_before_coll, strat_before_non_coll)
#' new_strata <- c(strat_after_coll, strat_after_non_coll)
#' pik_per_srs <- stats::setNames(c(0.2,0.4,0.2,0.4,0.4), as.character(1:5))
#' pik_per_new <- stats::setNames(c(0.1,0.5,0.6), c("A","B","C"))
#' 
#' 
#' pik <- c(pik_per_new[strat_after_coll], pik_per_srs[strat_before_non_coll])
#' id <- paste0("id",1:(n_units_coll + n_units_non_coll))
#' names(old_strata) <- id
#' names(new_strata) <- id
#' names(pik) <- id
#' precalc_var <- var_wolter(y = NULL,
#'                           old_strata = old_strata, 
#'                           new_strata = new_strata,
#'                           pik = pik,
#'                           id = id)
#' 
#' 
#' 
#' y <- as.matrix(rnorm(length(pik)), ncol = 1)
#' rownames(y) <- id
#' 
#' var_from_wolter_fn <- var_wolter(y = y, precalc = precalc_var, id)

var_wolter <- function(y = NULL, old_strata, new_strata, pik, psu_id = NULL, 
                       w = NULL,  precalc = NULL, id = NULL){
  if(is.null(precalc)){
    if(is.null(w)){
      w <- rep(1, length(pik))
      w <- stats::setNames(w,names(pik))
    } else {
      if(any(w < 0)){
        warning("Some weights are non-positive.")
      }
    }
    
    
    #In order to compute variance estimation based on Wolter's formula
    #We need to seperate pseudo strata (union of strata) and strata that remains 
    #the same after collapse.
    if(is.null(names(old_strata)) | is.null(names(new_strata))){
      stop("old_strata and new_strata must be named vectors.")
    }
    
    if(is.null(names(pik))){
      names(pik) <- names(old_strata)
    }
    
    if(!identical(names(old_strata), names(new_strata))){
      stop("old_strata and new_strata must be named vectors with same names.")
    }
    
    #SRSWOR part :
    srs_names <- names(new_strata)[is.na(new_strata)]
    part_srs <- var_srs(y = NULL, pik = pik[srs_names],
                        strata = old_strata[srs_names],
                        id = srs_names)
    diag_srs <- part_srs$diago
    
    #Wolter part :
    #wolter_names is a vector that contains all units that have been collapsed
    wolter_names <- names(new_strata)[!is.na(new_strata)]
    #n_old_strata_tot : number of sampled units in each old strata
    n_old_strata_tot <- table(old_strata)
    n_old_strata_tot <- stats::setNames(as.vector(n_old_strata_tot), names(n_old_strata_tot))
    #n_old_strata_wolter : number of sampled units in each old strata that have been collapsed
    n_old_strata_wolter <- n_old_strata_tot[unique(old_strata[wolter_names])]
    
    #Compute population size from probabilities and sample sizes.
    repr_old_strat <- names(old_strata[!duplicated(old_strata)])
    pik_old_strat <- pik[repr_old_strat]
    pik_old_strat <- stats::setNames(pik_old_strat, old_strata[names(pik_old_strat)]) #TO CHANGE : dep to names(pik)
    
    
    #Number of original strata by collapsed strata
    Hp <- table(unique(cbind(old_strata[wolter_names], new_strata[wolter_names]))[,2])
    Hp <- stats::setNames(as.vector(Hp), names(Hp))
    
    #We assumed that each pseudo-stratum is obtained by collapsing at least two strata.
    if(any(Hp <= 1)){
      stop("Each collapsed stratum must be the union of at least two strata.")
    }
    
    #Computation of diagonal terms of the associated quadratic form.
    diag_wolter <- (pik_old_strat[old_strata[wolter_names]])^(-2)
    diag_wolter <- stats::setNames(diag_wolter, wolter_names)
    pik_wolter <- pik[wolter_names] 
    
    #Concatenate diag 
    diago <- c(diag_srs, diag_wolter)
    diago <- diago[id]
  } else {
    list2env(precalc, envir = environment())
  }
  
  if(is.null(y)){
    return(list("part_srs" = part_srs, "Hp" = Hp, 
                "pik" = pik, "id" = id, "psu_id" = psu_id,
                "old_strata" = old_strata, "new_strata" = new_strata, 
                "diago" = diago))
  } else {
    y <- stats::setNames(as.vector(y), rownames(y))
    
    if(is.null(names(y))){
      warning("y is a unnamed matrix : old_strata names vector is used instead.")
      names(y) <- names(old_strata)
    }
    
    if(!is.null(precalc) && !is.null(id) && !is.null(rownames(y)) && !identical(as.character(id), rownames(y))) stop(
      "The names of the data matrix (y argument) do not match the reference id (id argument)."
    )
    
    #Variance from Wolter
    wolter_names <- names(new_strata)[!is.na(new_strata)]
    if(!is.null(w)){
      y_wolter <- y[wolter_names]*sqrt(w[wolter_names])
    } else {
      y_wolter <- y[wolter_names]
    }
    
    tot_old_strat <- sum_by(y_wolter/pik[wolter_names], old_strata[wolter_names])
    tot_new_strat <- sum_by(y_wolter/pik[wolter_names], new_strata[wolter_names])
    tot_old <- tot_old_strat[old_strata[wolter_names]]
    mean_new <- tot_new_strat[new_strata[wolter_names]]/Hp[new_strata[wolter_names]]
    
    part_var_wolter <- sum((Hp[new_strata[wolter_names]]/(Hp[new_strata[wolter_names]] - 1))*(tot_old - mean_new)^2)
    
    #Variance from SRSWOR
    y_srs <- y[setdiff(names(y), wolter_names)]
    part_var_srs <- var_srs(y_srs, precalc = part_srs, id = names(y_srs), w = w[setdiff(names(y), wolter_names)])
    
    return(sum(part_var_wolter,part_var_srs, na.rm = TRUE))
  }
}


#' Rao variance estimator for a survey design with two stages.
#' 
#' @description \code{var_Rao} computes the Rao
#' variance estimator (Rao, 1975).
#'
#' @param y A (sparse) row-named numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated.
#' @param description_psu A list with two named elements that describes how to take into account the variance from psu :
#' \itemize{\item `var_fn` : a variance function named.
#' \item `precalc` : a precalc object returned by a variance function from `gustave`.}
#' @param description_ssu A named list of list that describes how to take into account the variance from ssu.
#' `description_ssu` can be 
#' \itemize{\item a named list of \eqn{n_{PSU}} lists : each of \eqn{n_{PSU}}$ lists contains two named
#' element - a variance function `var_fn` and a precalc `precalc`.
#' \item }
#' @param precalc A list of pre-calculated results (analogous to the one used by 
#'   \code{\link{varDT}}).
#' @param id A vector of identifiers of the units used in the calculation.
#'
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical vector of size the number of columns of y. \item if \code{y} is
#'   \code{NULL} (pre-calculation step) : a list containing pre-calculated data 
#'   (analogous to the one used by \code{\link{varDT}}).}
#' 
#' @references Caron N. (1998), "Le logiciel Poulpe : aspects méthodologiques", \emph{Actes 
#'   des Journées de méthodologie statistique} \url{http://jms-insee.fr/jms1998s03_1/}
#'   Deville, J.-C. (1993), \emph{Estimation de la variance pour les enquêtes en
#'   deux phases}, Manuscript, INSEE, Paris.
#'   
#'   Rao, J.N.K (1975), "Unbiased variance estimation for multistage designs",
#'   \emph{Sankhya}, C n°37
#'   
#' @export
#'
#' @author Khaled Larbi
#' 
#' @examples
#' #Define PSU and SSU samples
#' n_psu <- 10L
#' n_per_psu <- sample(5:10, 10, replace = TRUE)
#' psu_by_ssu <- rep(paste0("id_psu",1:10), n_per_psu)
#' id_ssu <- paste0("id_ssu", 1: sum(n_per_psu))
#' pi_ssu <- runif(sum(n_per_psu), 0.2, 0.8)
#' 
#' #Define descrip1 that describes PSU's variance estimator
#' #(SRS variance estimator)
#' precalc1 <- var_srs(y = NULL,
#'                     pik = rep(0.1, n_psu),
#'                     id = paste0("id_psu", 1:n_psu))
#' descrip1 <- list("var_fn" = "var_srs",
#'                  "precalc" = precalc1)
#' 
#' #Define descrip1 that describes PSU's variance estimator
#' #(SRS variance estimator)
#' 
#' 
#' precalc2 <- lapply(X = unique(psu_by_ssu), FUN = function(psu_name){
#'   ind <- which(psu_by_ssu == psu_name);
#'   return(list("var_fn" = "var_pois",
#'               "precalc" = var_pois(y = NULL, pik = pi_ssu[ind], id = id_ssu[ind])))
#' })
#' names(precalc2) <- unique(psu_by_ssu)
#' descrip2 <- precalc2
#'
#' res1 <- var_Rao(y = NULL, descrip1, descrip2, NULL)
#' y <- as.matrix(rnorm(sum(n_per_psu)), ncol = 1)
#'
#' var_Rao(y, precalc = res1)

var_Rao <- function(y = NULL, 
                    description_psu, 
                    description_ssu,
                    precalc = NULL, 
                    id = NULL){
  if(is.null(precalc)){
    #Checks : 
    if(any(sort(names(description_psu)) != c("precalc","var_fn"))){
      stop("`description_psu` must be a list with two named elements : var_fn and precalc.")
    }
    
    if(is.null(description_psu$precalc$id)){
      stop("precalc in description_psu must contain a id attribute : please, fullfill it
           while defining the precalc.")
    }
    
    if(length(description_ssu) > 1 & is.null(names(description_ssu))){
      stop("description_ssu must be a named list of list.")
    }
    
    if(length(description_ssu) > 1 & any(sort(unique(names(description_ssu))) !=
                                         sort(unique(description_psu$precalc$id)))){
      stop("Element names of description_ssu must be the same as description_psu$precalc$id.")
    }
    
    desc_not_format <- lapply(X = description_ssu, FUN = function(desc){
      return(any(sort(names(desc)) != c("precalc","var_fn")))
    })
    
    desc_not_format <- stats::setNames(Reduce(f = c, desc_not_format), names(desc_not_format))
    
    if(any(desc_not_format)){
      stop("Some description in description_ssu are not 
           a named list with two elements : var_fn and precalc :", names(desc_not_format)[desc_not_format])
    }
    
    id_missings <- lapply(X = description_ssu, FUN = function(desc){
      return(is.null(desc$precalc$id))
    })
    
    id_missings <- stats::setNames(Reduce(f = c, id_missings), names(id_missings))
    
    if(any(id_missings)){
      stop("Id's are missing in some precalc from description_ssu elements", 
           names(id_missings)[id_missings])
    }
    
    #Check if all elements of description_ssu have a var_fn
    var_ssu_fn <- lapply(X = description_ssu, FUN = function(x){x[["var_fn"]]})
    if(any(is.null(var_ssu_fn))){
      psu_without_fn <- names(description_ssu)[which(is.null(var_ssu_fn))]
      stop("Some var_fn are missing in `description_ssu` : ", psu_without_fn)
    }
    
    precalc_ssu <- lapply(X = description_ssu, FUN = function(x){x[["precalc"]]})
    if((is.null(names(var_ssu_fn)) | is.null(names(description_ssu))) & length(precalc_ssu) > 1){
      stop("var_ssu_fn and precalc_ssu must be named using PSU names.")
    }
    
    #Store ssu id
    ssu_id <- lapply(X = precalc_ssu, FUN = function(x){x[["id"]]})
    
    #This condition corresponds at every case except when var_wolter is used for ssu
    #(var_wolter is defined once for all ssu)
    if(length(precalc_ssu) != 1 | length(unique(description_psu$precalc$id)) == 1){
      psu_id <- rep(names(precalc_ssu), sapply(X = ssu_id, FUN = length))
    } else {
      psu_id <- description_ssu[[1]]$precalc$psu_id #for var_wolter, one can fullfill an argument psu_id 
      #in order to store, for each ssu, the psu it belongs to.
    }
    #links is a matrix that contains id from psu and ssu
    #This matrix is based on the names of each element in the 
    #description_ssu object and the id vecteur from each 
    #precalc_ssu. 
    ssu_id <- Reduce(c, ssu_id)
    links <- cbind(psu_id, ssu_id)
    
    #Store psu and ssu conditionnal inclusion probabilities.
    #psu_pi : a named vector that contains for each psu, the inclusion probability 
    #ssu_pi : a named vector that contains for each ssu, the inclu probability given the psu has been drawn. 
    psu_pi <- stats::setNames(description_psu$precalc$pik,description_psu$precalc$id)
    ssu_pi <- lapply(X = precalc_ssu, FUN = function(prec){stats::setNames(prec$pik, prec$id)})
    ssu_pi <- Reduce(c, ssu_pi)
    psu_pi <- psu_pi[psu_id]
    
    #links_pi is a matrix that contains pi (conditionnal probabilities) from psu and ssu
    links_pi <- cbind(psu_pi, ssu_pi)
    
    #Check
    if(!is.null(precalc_ssu) & (!all(links[,1] %in% names(description_ssu))
                                & length(precalc_ssu) != 1)){
      stop("Some precalculated results are missing in description_ssu")
    }
    
    #Computation of diagonal terms. 
    q_ssu <- lapply(X = precalc_ssu, 
                    FUN = function(x){stats::setNames(x$diago, x$id)})
    q_ssu <- Reduce(f = c, x = q_ssu)
    q_ssu <- q_ssu[links[,2]]
    
    q_psu <- stats::setNames(description_psu$precalc$diago, description_psu$precalc$id)
    q_psu <- q_psu[links[,1]]
    
    diago <- (q_psu/(links_pi[,2]^2)) + ((1/links_pi[,1]^2) - q_psu)*q_ssu
    diago <- stats::setNames(diago, links[,2])
    
    #Store id's (usefull when var_rao is used another time and var_psu is var_rao)
    id <- links[,2]
  } else {
    list2env(precalc, envir = environment())
  }
  
  if(is.null(y)){
    return(list("id" = id, "description_psu" = description_psu, 
                "description_ssu" = description_ssu,
                "q_psu" = q_psu, "q_ssu" = q_ssu,
                "pik" = stats::setNames(links_pi[,1] * links_pi[,2], links[,2]),
                "links_pi" = links_pi, "links" = links,
                "diago" = diago))
  } else {
    #Prepare variance function names for psu and ssu variance estimators.
    var_psu_fn <- description_psu$var_fn
    var_ssu_fn <- lapply(X = description_ssu, FUN = function(x){x[["var_fn"]]})
    var_ssu_fn <- stats::setNames(Reduce(c, var_ssu_fn), names(var_ssu_fn))
    
    #Prepare precalc's for psu and ssu variance estimators.
    precalc_psu <- description_psu$precalc
    precalc_ssu <- lapply(X = description_ssu, FUN = function(x){x[["precalc"]]})
    
    #Check that y is a row-named matrix.
    #If not, then, we assume that y is sorted in the same way as `id` and a warning is handled.
    #If id is a permutation of rownames(y), then y is sorted.
    #If id is not equal to rownames(y), then a error is handled. 
    if(is.null(rownames(y))){
      warning("y must be a row-named matrix : rownames are assumed to be those from the id argument.")
      rownames(y) <- id
    } else {
      if(any(sort(rownames(y)) != sort(id))){
        stop("id arguments and y rownames must contain the same elements.")
      } else {
        if(any(rownames(y) != id)){
          #Reorder row from y.
          warning("y's rows have been sorted to match with the id parameter")
          y <- y[id,,drop = FALSE]
        }
      }
    }
    #Variance computation 
    
    #Compute estimated total in each psu.
    y_psu <- sum_by(y = y, by = links[,1], w = 1/links_pi[,2])
    
    #Apply Psu variance estimator to y_psu
    var_psu_computed <- do.call(what = var_psu_fn,
                                args = list("y" = y_psu[precalc_psu$id,,drop = FALSE],
                                            "precalc" = precalc_psu))
    
    #Duplicate to make y_psu size equal to number of ssu.
    y_psu <- y_psu[links[,1], , drop = FALSE]
    
    
    #Apply SSU variance estimators in each SSU
    #Two cases : one can provide a list that contains n_PSU precalc
    #or a single precalc for all SSU sampling (using when using Wolter's estimator) 
    # unique_psu <- unique(link_psu_ssu[, c("psu", "pi_psu", "ssu")])
    # unique_psu <- unique(link_psu_ssu[, c("psu", "pi_psu", "ssu")])
    pi_psu <- stats::setNames(links_pi[,1], links[,2])
    pond_var_ssu <- (1/(pi_psu^2) - q_psu[links[,1]])
    
    if(var_psu_fn == "var_Rao"){
      #If var_wolter is used for taking into account SSU variance then
      #weights used in Rao formula for each SSU variance are
      #clipped down by 0.
      if(all(precalc_psu$var_ssu_fn == "var_wolter")){
        pond_var_ssu <- pond_var_ssu * as.integer(pond_var_ssu > 0)
      }
    }
    
    #This condition corresponds at every case except when var_wolter is used for ssu
    #(var_wolter is defined once for all ssu)
    if(length(precalc_ssu) != 1 | length(unique(links[,1])) == 1){
      var_ssu_computed <- lapply(X = names(precalc_ssu), FUN = function(psu){
        ssu_names_from_psu <- links[links[,1] == psu,2]
        y_ssu <- y[ssu_names_from_psu,,drop = FALSE]
        precalc_ssu_with_w <- precalc_ssu[[psu]]
        precalc_ssu_with_w$w <- pond_var_ssu[rownames(y_ssu)]
        do.call(what = var_ssu_fn[psu],
                args = list("y" = y_ssu,
                            "precalc" = precalc_ssu_with_w))
      })
      var_ssu_computed <- do.call(what = rbind, args = var_ssu_computed)
      rownames(var_ssu_computed) <- names(precalc_ssu)
    } else {
      y_ssu <- y[precalc_ssu[[1]]$id, ,drop = FALSE]
      precalc_ssu_with_w <- precalc_ssu[[1]]
      precalc_ssu_with_w$w <- pond_var_ssu[rownames(y_ssu)]
      var_ssu_computed <- do.call(what = var_ssu_fn[1], 
                                  args = list("y" = y_ssu,
                                              "precalc" = precalc_ssu_with_w))
    }
    var_tot <- var_psu_computed + colSums(var_ssu_computed)
    return(var_tot)
  }
}





#' Variance estimator for multiple stage sampling
#' 
#' @aliases var_Rao_multiple var_Ro
#' 
#'
#' @param y A (sparse) numerical matrix of the variable(s) whose variance of their total
#'   is to be estimated.
#' @param ... Arguments named \code{descriptionX} where \code{X} is an integer
#' that describe each stage of the sample.
#' For each stage \code{s}, one needs to provide a list that contains two named elements :
#' \itemize{\item \code{var_fn_s} which corresponds to a gustave variance function.
#' \item\code{precalc_s} which is the precalc associated to \code{var_fn_s}. \code{id} 
#' argument must be fulfilled for each precalc in order to be able to follow units across stages.}
#' @param precalc  A list of pre-calculated results provided by \code{var_Rao_multiple}.
#' @param id A vector of identifiers of the units from the last stage used in the calculation.
#'
#'
#' @return \itemize{ \item if \code{y} is not \code{NULL} (calculation step) : a
#'   numerical vector of size the number of columns of y. \item if \code{y} is
#'   \code{NULL} (pre-calculation step) : a list containing pre-calculated data 
#'   (analogous to the one used by \code{\link{varDT}}).  Note that \code{pik}
#'   corresponds to the probability that a unit is inside the final sample (which is obtained
#'   by chaining weights).}
#' @export
#' 
#' @details \code{var_Rao_multiple} computes total variance estimates for multi-stage sampling designs. 
#' It is based on Rao's formula (which allows variance estimation for a two-stage design), used recursively.
# This estimator is based on three assumptions: 
#' 
#' \itemize{\item the existence of an unbiased estimator of the first-degree variance which is a quadratic form, and of the second-degree variance (given the first degree).
#' \item the draws of secondary units within primary units are independent from one primary unit to another.
#' \item the total within a primary unit can be estimated unbiasedly.}
#'
#'  \code{var_Rao} is an intermediate function : even for a two-degree design, use 
#'  \code{var_Rao_multiple}
#'  
#' @author Khaled Larbi
#' 
#'@references Caron N. (1998), "Le logiciel Poulpe : aspects méthodologiques", \emph{Actes 
#'   des Journées de méthodologie statistique} \url{http://jms-insee.fr/jms1998s03_1/}
#'   Deville, J.-C. (1993), \emph{Estimation de la variance pour les enquêtes en
#'   deux phases}, Manuscript, INSEE, Paris.
#'   
#'   Deville, J.-C., Tillé, Y. (2005), "Variance approximation under balanced
#'   sampling", \emph{Journal of Statistical Planning and Inference}, 128, issue
#'   2 569-591
#'   
#'   Rao, J.N.K (1975), "Unbiased variance estimation for multistage designs",
#'   \emph{Sankhya}, C n°37
#'   
#' @examples 
#'### Example from the Labour force survey (LFS) in `define_variance_wrapper`
#' 
#' # The (simulated) Labour force survey (LFS) has the following characteristics:
#' # - first sampling stage: balanced sampling of 4 areas (each corresponding to 
#' #   about 120 dwellings) on first-order probability of inclusion (proportional to 
#' #   the number of dwellings in the area) and total annual income in the area.
#' # - second sampling stage: in each sampled area, simple random sampling of 20 
#' #   dwellings
#' # - neither non-response nor calibration
#' 
#' #Estimator based on `var_Rao_multiple`
#' precalc1 <- varDT(
#'   y = NULL, 
#'   pik = lfs_samp_area$pik_area, 
#'   x = as.matrix(lfs_samp_area[c("pik_area", "income")]),
#'   id = lfs_samp_area$id_area
#' )
#' 
#' desc_first_stage <- list("var_fn" = "varDT",
#'                          "precalc" = precalc1)
#' 
#' desc_second_stage <- lapply(X = split(lfs_samp_dwel, lfs_samp_dwel$id_area),
#'                             FUN = function(x){list("var_fn" = "var_srs",
#'                                                    "precalc" = var_srs(y = NULL, 
#'                                                    pik = x$pik_dwel, id = x$id_dwel))})
#' 
#' precalc_lfs <- var_Rao_multiple(y = NULL, 
#'                                 description1 = desc_first_stage,
#'                                 description2 = desc_second_stage)
#' 
#' 
#' #Estimator computed manually
#' lfs_samp_dwel$q_area <- with(precalc1, setNames(diago, id))[lfs_samp_dwel$id_area]
#' 
#' var_lfs <- function(y, ind, dwel, area){
#'   variance <- list()
#'   
#'   # Variance associated with the sampling of the dwellings
#'   y <- sum_by(y, ind$id_dwel)
#'   variance[["dwel"]] <- var_srs(
#'     y = y, pik = dwel$pik_dwel, strata = dwel$id_area, 
#'     w = (1 / dwel$pik_area^2 - dwel$q_area)
#'   )
#'   
#'   # Variance associated with the sampling of the areas
#'   y <- sum_by(y = y, by = dwel$id_area, w = 1 / dwel$pik_dwel) 
#'   variance[["area"]] <- varDT(y = y, precalc = area)
#'   
#'   Reduce(`+`, variance)
#'   
#' }
#' 
#' #Estimations
#' y <- matrix(as.numeric(lfs_samp_ind$unemp), ncol = 1, dimnames = list(lfs_samp_ind$id_ind))
#' y_dw <- sum_by(y = as.numeric(lfs_samp_ind$unemp), by = lfs_samp_ind$id_dwel)
#' var_lfs1 <- var_Rao_multiple(y = matrix(y_dw, ncol = 1, dimnames = list(names(y_dw), NULL)), 
#'                              precalc = precalc_lfs)
#' var_lfs2 <- var_lfs(y = y,
#'                     ind = lfs_samp_ind,
#'                     dwel = lfs_samp_dwel,
#'                     area = precalc1) 
var_Rao_multiple <- function(y = NULL, ..., precalc = NULL, id = NULL) {
  extra_args <- list(...)
  descriptions_args <- (grepl(pattern = "description\\d{1,}", x = names(extra_args)))
  nb_stages <- sum(descriptions_args)
  #add warnings 
  if (is.null(precalc)) {
    description_psu <- extra_args[["description1"]]
    for (s in 1:(nb_stages - 1)) {
      q_rao <- var_Rao(y = NULL, 
                       description_psu = description_psu,
                       description_ssu = extra_args[[paste0("description",s+1)]])
      description_psu <- list("var_fn" = "var_Rao",
                              "precalc" = q_rao)
    }
  } else {
    list2env(precalc, envir = environment())
  }
  
  if (is.null(y)) {
    return(list(q_rao = q_rao))
  } else {
    var_fin <- var_Rao(y = y, precalc = q_rao)
    return(var_fin)
  }
  
}
