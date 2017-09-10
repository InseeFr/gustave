
#' TODO
total <- define_linearization_wrapper(
  linearization_function = function(y, weight){
  y[is.na(y)] <- 0
  total <- sum(y * weight)
  return(list(lin = list(y), metadata = list(est = total)))
  }
  , allow_factor = TRUE
)

#' TODO
ratio <- define_linearization_wrapper(
  linearization_function = function(num, denom, weight){
    na <- is.na(num) | is.na(denom); num[na] <- 0; denom[na] <- 0
    total_num <- sum(num * weight)
    total_denom <- sum(denom * weight)
    ratio <- total_num / total_denom
    lin <- (num - ratio * denom ) / total_denom
    return(list(lin = list(lin), metadata = list(est = ratio)))
  }
  , arg_type = list(data = c("num", "denom") , weight = "weight")
)

#' TODO
mean <- define_linearization_wrapper(
  linearization_function = function(y, weight){
    environment(ratio)$linearization_function(num = y, denom = rep(1, length(y)), weight = weight)
  }
  , allow_factor = TRUE
)

#' TODO
diffratio <- define_linearization_wrapper(
  linearization_function = function(num1, denom1, num2, denom2, weight){
    na <- is.na(num1) | is.na(denom1) | is.na(num2) | is.na(denom2)
    num1[na] <- 0; denom1[na] <- 0; num2[na] <- 0; denom2[na] <- 0
    ratio1 <- environment(ratio)$linearization_function(num = num1, denom = denom1, weight = weight)
    ratio2 <- environment(ratio)$linearization_function(num = num2, denom = denom2, weight = weight)
    lin <- ratio1$lin[[1]] - ratio2$lin[[1]]
    est <- ratio1$metadata$est - ratio2$metadata$est
    return(list(lin = list(lin), metadata = list(est = est)))
  }
  , arg_type = list(data = c("num1", "denom1", "num2", "denom2") , weight = "weight")
)

#' TODO
arpr <- define_linearization_wrapper(
  linearization_function = function(inc, weight, local = TRUE){
    require(vardpoor)
    r <- linarpr(Y = inc, weight = weight)
    return(list(lin = list(r$lin$lin_arpr), metadata = list(est = r$val$arpr)))
  }
  , arg_type = list(data = "inc" , weight = "weight", param = "local")
)

