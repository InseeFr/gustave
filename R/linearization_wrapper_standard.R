#' Standard linearization wrappers
#' 
#' @description Functions to be used within variance estimation 
#'   wrappers in order to perform on-the-fly linearizations (see Details).
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
#' @param ... Technical parameters passed on to helper functions
#'   within the linearization wrapper.
#' 
#' @details When the estimator is not the estimator of a total, the application of 
#'   analytical variance estimation formulae developed for the estimator of a total 
#'   is not straightforward (Deville, 1999). An asymptotically unbiased variance 
#'   estimator can nonetheless be obtained if the estimation of variance is performed
#'   on a variable obtained from the original data through a linerization step. 
#'   
#'   The \code{ratio}, \code{mean}, \code{diff_of_ratio} and 
#'   \code{ratio_of_ratio} functions implement the standard linearization 
#'   techniques respectively for the ratio, mean, difference of ratios and 
#'   ratio of ratios estimators, as presented for example in (Caron, 1998). 
#'   The \code{total} function does not perform any linearization
#'   (as none is needed for the estimator of a total) and solely adds the technical 
#'   features required to use the linearization wrapper within the \code{\link[=define_variance_wrapper]{variance wrappers}}.
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
#' @name linearization_wrapper_standard
#' @aliases total ratio mean diff_or_ratio ratio_of_ratio

NULL

#' @rdname linearization_wrapper_standard
total <- define_linearization_wrapper(
  linearization_function = function(y, weight){
    na <- is.na(y)
    y[na] <- 0
    total <- sum(y * weight)
    list(
      lin = list(y), 
      metadata = list(est = total, n = sum(!na))
    )
  }, 
  arg_type = list(data = "y" , weight = "weight"),
  allow_factor = TRUE
)

#' @rdname linearization_wrapper_standard
ratio <- define_linearization_wrapper(
  linearization_function = function(num, denom, weight){
    na <- is.na(num) | is.na(denom)
    num[na] <- 0
    denom[na] <- 0
    est_num <- sum(num * weight)
    est_denom <- sum(denom * weight)
    ratio <- est_num / est_denom
    lin <- (num - ratio * denom ) / est_denom
    list(
      lin = list(lin), 
      metadata = list(est = ratio, n = sum(!na), est_num = est_num, est_denom = est_denom)
    )
  }, 
  arg_type = list(data = c("num", "denom") , weight = "weight")
)

#' @rdname linearization_wrapper_standard
mean <- define_linearization_wrapper(
  linearization_function = function(y, weight){
    environment(ratio)$linearization_function(num = y, denom = rep(1, length(y)), weight = weight)
  }, 
  arg_type = list(data = "y" , weight = "weight"),
  allow_factor = TRUE
)

#' @rdname linearization_wrapper_standard
diff_of_ratio <- define_linearization_wrapper(
  linearization_function = function(num1, denom1, num2, denom2, weight){
    na <- is.na(num1) | is.na(denom1) | is.na(num2) | is.na(denom2)
    num1[na] <- 0
    denom1[na] <- 0
    num2[na] <- 0
    denom2[na] <- 0
    ratio1 <- environment(ratio)$linearization_function(num = num1, denom = denom1, weight = weight)
    ratio2 <- environment(ratio)$linearization_function(num = num2, denom = denom2, weight = weight)
    lin <- ratio1$lin[[1]] - ratio2$lin[[1]]
    est <- ratio1$metadata$est - ratio2$metadata$est
    list(
      lin = list(lin), 
      metadata = list(est = est, n = sum(!na))
    )
  }, 
  arg_type = list(data = c("num1", "denom1", "num2", "denom2") , weight = "weight")
)

#' @rdname linearization_wrapper_standard
ratio_of_ratio <- define_linearization_wrapper(
  linearization_function = function(num1, denom1, num2, denom2, weight){
    na <- is.na(num1) | is.na(denom1) | is.na(num2) | is.na(denom2)
    num1[na] <- 0
    denom1[na] <- 0
    num2[na] <- 0
    denom2[na] <- 0
    est_num1 <- sum(num1 * weight)
    est_denom1 <- sum(denom1 * weight)
    est_num2 <- sum(num2 * weight)
    est_denom2 <- sum(denom2 * weight)
    est <- (est_num1 / est_denom1) / (est_num2 / est_denom2)
    lin <- est * ( 
      (num1 / est_num1) - (num2 / est_num2) - 
      (denom1 / est_denom1) + (denom2 / est_denom2)
    )
    list(
      lin = list(lin), 
      metadata = list(est = est, n = sum(!na))
    )
  }, 
  arg_type = list(data = c("num1", "denom1", "num2", "denom2") , weight = "weight")
)
