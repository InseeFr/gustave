
#' Linearization wrappers for some Laeken indicators based on the vardpoor package
#' 
#' 
#' 
#' 
#' @name linearization_wrapper_laeken
#' @aliases arpr gini

NULL

#' @rdname linearization_wrapper_laeken
arpr <- define_linearization_wrapper(
  linearization_function = function(y, weight, percentage = 60, order_quant = 50L){
    require(vardpoor)
    r <- linarpr(Y = y, weight = weight, percentage = percentage, order_quant = order_quant)
    return(list(lin = list(r$lin$lin_arpr), metadata = list(est = r$val$arpr)))
  }, 
  arg_type = list(data = "y", weight = "weight", param = c("percentage", "order_quant"))
)

#' @rdname linearization_wrapper_laeken
gini <- define_linearization_wrapper(
  linearization_function = function(y, weight){
    require(vardpoor)
    r <- lingini(Y = y, weight = weight)
    return(list(lin = list(r$lin$lin_gini), metadata = list(est = r$value$Gini)))
  }, 
  arg_type = list(data = "y", weight = "weight", param = NULL)
)

