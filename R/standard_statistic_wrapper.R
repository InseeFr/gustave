#' Standard statistic wrappers
#' 
#' @description Functions to be used within variance estimation 
#'   wrappers in order to specify which statistic is to be estimated.
#'   
#' @param y A vector corresponding to the (sole) variable to estimate
#'   variance on. If not numeric (character or factor), it is 
#'   automatically discretized.
#' @param num,num1,num2 Numerical vector(s) corresponding to the numerator(s) 
#'   to be used in the estimation.
#' @param denom,denom1,denom2 Numerical vector(s) corresponding to the denominator(s) 
#'   to be used in the estimation.
#' @param by Factor vector (character vectors are coerced to factors) whose levels are used
#'   to break down the estimation by domains.
#' @param where Logical vector indicating the domain to perform variance estimation on.
#' 
#' @details When the estimator is not the estimator of a total, the application of 
#'   analytical variance estimation formulae developed for the estimator of a total 
#'   is not straightforward (Deville, 1999). An asymptotically unbiased variance 
#'   estimator can nonetheless be obtained if the estimation of variance is performed
#'   on a variable obtained from the original data through a linerization step. 
#'   
#'   The \code{ratio}, \code{mean}, \code{diff_of_ratio} and \code{ratio_of_ratio} 
#'   functions produce the point estimate of the statistic and derive the 
#'   corresponding linearized variable which is passed on to the variance
#'   estimation function. The \code{total} function does not perform any 
#'   linearization (as none is needed for the estimator of a total) and 
#'   solely produces the corresponding point estimator. 
#' 
#' @seealso \code{\link{define_variance_wrapper}}
#' 
#' @references 
#'   Caron N. (1998), "Le logiciel Poulpe : aspects méthodologiques", \emph{Actes 
#'   des Journées de méthodologie statistique} \url{http://jms-insee.fr/jms1998s03_1/}
#'   
#'   Deville J.-C. (1999), "Variance estimation for complex statistics and 
#'   estimators: linearization and residual techniques", \emph{Survey Methodology}, 
#'   25:193–203
#'   
#' @examples # See define_variance_wrapper examples
#' 
#' @author Martin Chevalier
#' 
#' @name standard_statistic_wrapper
#' @aliases total ratio mean diff_or_ratio ratio_of_ratio

NULL

#' @rdname standard_statistic_wrapper
total <- define_statistic_wrapper(
  statistic_function = function(y, weight){
    na <- is.na(y)
    y[na] <- 0
    point <- sum(y * weight)
    list(point = point, lin = y, metadata = list(n = sum(!na)))
  }, 
  arg_type = list(data = "y" , weight = "weight")
)

#' @rdname standard_statistic_wrapper
ratio <- define_statistic_wrapper(
  statistic_function = function(num, denom, weight){
    na <- is.na(num) | is.na(denom)
    num[na] <- 0
    denom[na] <- 0
    est_num <- sum(num * weight)
    est_denom <- sum(denom * weight)
    point <- est_num / est_denom
    lin <- (num - point * denom ) / est_denom
    list(point = point, lin = lin, metadata = list(
      n = sum(!na), est_num = est_num, est_denom = est_denom
    ))
  }, 
  arg_type = list(data = c("num", "denom") , weight = "weight")
)

#' @rdname standard_statistic_wrapper
mean <- define_statistic_wrapper(
  statistic_function = function(y, weight){
    environment(ratio)$statistic_function(num = y, denom = rep(1, length(y)), weight = weight)
  }, 
  arg_type = list(data = "y" , weight = "weight")
)

#' @rdname standard_statistic_wrapper
diff_of_ratio <- define_statistic_wrapper(
  statistic_function = function(num1, denom1, num2, denom2, weight){
    na <- is.na(num1) | is.na(denom1) | is.na(num2) | is.na(denom2)
    num1[na] <- 0
    denom1[na] <- 0
    num2[na] <- 0
    denom2[na] <- 0
    ratio1 <- environment(ratio)$statistic_function(num = num1, denom = denom1, weight = weight)
    ratio2 <- environment(ratio)$statistic_function(num = num2, denom = denom2, weight = weight)
    point <- ratio1$point - ratio2$point
    lin <- ratio1$lin - ratio2$lin
    list(point = point, lin = lin, metadata = list(n = sum(!na)))
  }, 
  arg_type = list(data = c("num1", "denom1", "num2", "denom2") , weight = "weight")
)

#' @rdname standard_statistic_wrapper
ratio_of_ratio <- define_statistic_wrapper(
  statistic_function = function(num1, denom1, num2, denom2, weight){
    na <- is.na(num1) | is.na(denom1) | is.na(num2) | is.na(denom2)
    num1[na] <- 0
    denom1[na] <- 0
    num2[na] <- 0
    denom2[na] <- 0
    est_num1 <- sum(num1 * weight)
    est_denom1 <- sum(denom1 * weight)
    est_num2 <- sum(num2 * weight)
    est_denom2 <- sum(denom2 * weight)
    point <- (est_num1 / est_denom1) / (est_num2 / est_denom2)
    lin <- point * ( 
      (num1 / est_num1) - (num2 / est_num2) - 
      (denom1 / est_denom1) + (denom2 / est_denom2)
    )
    list(point = point, lin = lin, metadata = list(n = sum(!na)))
  }, 
  arg_type = list(data = c("num1", "denom1", "num2", "denom2") , weight = "weight")
)
