
#' Linearization placeholder for standard evaluation
#' 
#' @description The \code{lin} function is a generic 
#' placeholder allowing to easily use the gustave
#' package capabilities with standard evaluation.
#' @param ... Arguments to be passed to the linearization
#' function, as variable names (character vectors of length 1)
#' @param stat, Statistic to be used for linearization,
#' either as a function or as a function name (character vector
#' of length 1)
#' @param by, Domains to split accross, either as an R expression
#' or as a variable name (character vector of length 1)
#' @param where, Domain restriction, either as an R expression
#' or as a variable name (character vector of length 1)
#' 
#' @output The corresponding unevaluated function call.

is_variable_name <- function(x) is.character(x) && length(x) == 1

lin <- function(stat,
                ..., 
                by = NULL,
                where = NULL
){
  
  parent_frame <- parent.frame()
  
  stat <- if(is.function(stat)) substitute(stat) else as.symbol(stat)
  
  arg <- eval(substitute(alist(...)), envir = parent_frame)
  spy <<- arg; stop()
  if(any(!sapply(arg, is_variable_name)))
    stop("All arguments passed on to the linearization wrapper (through \"...\") must be variables names (character vectors of length 1)")
  arg <- lapply(arg, as.symbol)
  
  by <- if(exists(deparse(substitute(by))) && is_variable_name(by))
    as.symbol(by) else substitute(by)
  where <- if(exists(deparse(substitute(by))) && is_variable_name(by))
    as.symbol(where) else substitute(where)
  
  as.call(c(list(stat), arg, list(by = by, where = where)))

  
}

# lin("mean", "speed_quanti")

sapply(c("speed_quanti", "speed_quali"), function(var) lin(stat = "mean", var))