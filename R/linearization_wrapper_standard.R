
#' Standard linearization wrappers
#' 
#' 
#' 
#' 
#' @name linearization_wrapper_standard
#' @aliases total ratio mean diffratio


#' @rdname linearization_wrapper_standard
total <- define_linearization_wrapper(
  linearization_function = function(y, weight){
    na <- is.na(y)
    y[na] <- 0
    total <- sum(y * weight)
    return(list(lin = list(y), metadata = list(est = total, n = sum(!na))))
  }, 
  arg_type = list(data = "y" , weight = "weight"),
  allow_factor = TRUE
)

#' @rdname linearization_wrapper_standard
ratio <- define_linearization_wrapper(
  linearization_function = function(num, denom, weight){
    na <- is.na(num) | is.na(denom); num[na] <- 0; denom[na] <- 0
    est_num <- sum(num * weight)
    est_denom <- sum(denom * weight)
    ratio <- est_num / est_denom
    lin <- (num - ratio * denom ) / est_denom
    return(list(lin = list(lin), metadata = list(est = ratio, n = sum(!na), est_num = est_num, est_denom = est_denom)))
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
diffratio <- define_linearization_wrapper(
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
    return(list(lin = list(lin), metadata = list(est = est, n = sum(!na))))
  }, 
  arg_type = list(data = c("num1", "denom1", "num2", "denom2") , weight = "weight")
)


