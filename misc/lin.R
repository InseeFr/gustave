
#' Linearization placeholder for standard evaluation
#' 
#' @description The \code{lin} function is a generic 
#' placeholder allowing to easily use the gustave
#' package capabilities with standard evaluation.
#' @param ... Arguments to be passed to the linearization
#' function, as variable names (character vectors of length 1)
#' @param stat,.stat Statistic to be used for linearization,
#' either as a function or as a function name (character vector
#' of length 1)
#' @param by,.by Domains to split accross, either as an R expression
#' or as a variable name (character vector of length 1)
#' @param where,.where Domain restriction, either as an R expression
#' or as a variable name (character vector of length 1)
#' 
#' @output The corresponding unevaluated function call.

lin <- function(..., 
                stat = NULL,
                by = NULL,
                where = NULL
){
  
  # Step 1: Controls
  if(is.null(stat) && is.null(.stat))
    stop("Either stat or .stat must be not NULL.")
  if(!is.null(stat) && !is.null(.stat))
    stop("stat and .stat must not be not NULL simultaneously.")
  if(!is.null(by) && !is.null(.by))
    stop("stat and .stat must not be not NULL simultaneously.")
  if(!is.null(where) && !is.null(.where))
    stop("stat and .stat must not be not NULL simultaneously.")
  if(!is.null(.stat) && (!is.character(.stat) || !length(.stat) == 1))
    stop(".stat must be a character vector of length 1.")
  if(!is.null(.by) && (!is.character(.by) || !length(.by) == 1))
    stop(".by must be a character vector of length 1.")
  if(!is.null(.where) && (!is.character(.where) || !length(.where) == 1))
    stop(".where must be a character vector of length 1.")
  
  # Step 2: Coertion
  if(!is.null(stat)) .stat <- deparse(substitute(stat))
  if(!is.null(.by)) .by <- as.symbol(.by)
  
  
  return(.stat)

}

lin("big_data", stat = "mean")