
#' Define a statistic wrapper

#' @description \code{define_statistic_wrapper} defines 
#'   statistic \emph{wrappers} to be used together with
#'   \code{\link[=define_variance_wrapper]{variance estimation wrappers}}. 
#'   A statistic wrapper produces both the point estimator and the 
#'   linearized variable associated with a given statistic to estimate 
#'   variance on (Deville, 1999). \code{define_statistic_wrapper} is intended 
#'   for \strong{advanced use only}, standard statistic wrappers are included 
#'   in the gustave package (see \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}})
#'   
#' @param statistic_function An R function specific to the statistic to
#'   calculate. It should produce at least the point estimator and the
#'   linearized variable associated with the statistic (see Details).
#' @param arg_type A named list with three character vectors describing 
#'   the type of each argument of \code{statistic_function} (see Details).
#' @param arg_not_affected_by_domain A character vector indicating the
#'   arguments which should not be affected by domain-splitting. Such parameters
#'   may appear in some complex linearization formula, for instance when the 
#'   At-Risk of Poverty Rate (ARPR) is estimated by region but with a poverty 
#'   line calculated at the national level.
#' @param  display_function An R function which produces, for each variance 
#'   estimation, the data.frame to be displayed by the variance estimation 
#'   wrapper. The default display function (\code{standard_display}) uses 
#'   standard metadata to display usual variance indicator (point estimate, 
#'   variance, standard  deviation, coefficient of variation, confidence interval) 
#'   broken down by statistic wrapper, domain (if any) and level (if the variable 
#'   is a factor).
#'   
#' @details When the statistic to estimate is not a total, the application of 
#'   analytical variance estimation formulae developed for the estimator of a total 
#'   is not straightforward (Deville, 1999). An asymptotically unbiased variance 
#'   estimator can nonetheless be obtained if the estimation of variance is performed
#'   on a variable obtained from the original data through a linearization step. 
#'   
#'   \code{define_statistic_wrapper} is the function used to create, for a 
#'   given statistic, an easy-to-use function which calculates both the point
#'   estimator and the linearized variable associated with the statistic. These
#'   operations are implemented by the \code{statistic_function}, which can have
#'   any needed input (for example \code{num} and \code{denom} for a ratio
#'   estimator) and should output a list with at least two named elements: \itemize{
#'   \item \code{point}: the point estimator of the statistic
#'   \item \code{lin}: the linearized variable to be passed on to the variance
#'   estimation formula. If several variables are to be associated with
#'   the statistics, \code{lin} can be a list itself.
#'   }
#'   All other named elements in the output of \code{define_statistic_wrapper} are 
#'   treated as metadata (that may be used later on by \code{display_function}).
#'   
#'   \code{arg_type} is a named list of three elements that describes the nature 
#'   of the argument of \code{statistic_function}: \itemize{
#'   \item \code{data}: data argument(s), numerical vector(s) to be used 
#'   to calculate the point estimate and the linearized variable associated
#'   with the statistic 
#'   \item \code{weight}: weight argument, numerical vector to be used 
#'   as row weights
#'   \item \code{param}: parameters, non-data arguments to be used to 
#'   control some aspect of the computation}
#'   
#'   Statistic wrappers are quite flexible tools to apply a variance function 
#'   to an estimator requiring a linearization step (e.g. all estimators except 
#'   the estimator of a total) with virtually no  additional complexity for the
#'   end-user. 
#'   
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}} 
#'   are included within the gustave package and automatically added 
#'   to the variance estimation wrappers. New statistic wrappers can be defined
#'   using the \code{define_statistic_wrapper} and then explicitly added to the 
#'   variance estimation wrappers using the \code{objects_to_include} argument.
#'   
#'   Note: To some extent, statistic wrappers can be seen as ggplot2
#'   \code{geom_} and \code{stat_} functions: they help the end-user in writing 
#'   down what he or she wants without having to go too  deep into the details 
#'   of the corresponding layers. 
#'   
#' @return A function to be used within a variance estimation wrapper to estimate
#'   a specific statistic (see examples). Its formals are the ones of 
#'   \code{statistic_function} with the addition of \code{by} and \code{where} 
#'   (for domain estimation, set to \code{NULL} by default). 
#' 
#' @author Martin Chevalier
#'    
#' @seealso \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}}, \code{\link{define_variance_wrapper}}
#'   
#' @references 
#'   Deville J.-C. (1999), "Variance estimation for complex statistics and 
#'   estimators: linearization and residual techniques", \emph{Survey Methodology}, 
#'   25:193–203
#'   
#' @examples ### Example from the Information and communication technologies (ICT) survey
#' 
#' # Let's define a variance wrapper asfor the ICT survey 
#' # as in the examples of the qvar function: 
#' precision_ict <- qvar(
#'   data = ict_sample,
#'   dissemination_dummy = "dissemination",
#'   dissemination_weight = "w_calib",
#'   id = "firm_id",
#'   scope_dummy = "scope",
#'   sampling_weight = "w_sample", 
#'   strata = "strata",
#'   nrc_weight = "w_nrc", 
#'   response_dummy = "resp", 
#'   hrg = "hrg",
#'   calibration_weight = "w_calib",
#'   calibration_var = c(paste0("N_", 58:63), paste0("turnover_", 58:63)),
#'   define = TRUE
#' )
#' precision_ict(ict_survey, mean(speed_quanti))
#' 
#' # Let's now redefine the mean statistic wrapper
#' mean2 <- define_statistic_wrapper(
#'   statistic_function = function(y, weight){
#'     point <- sum(y * weight) / sum(weight)
#'     lin <- (y - point) / sum(weight)
#'     list(point = point, lin = lin, metadata = list(n = length(y)))
#'   },
#'   arg_type = list(data = "y", weight = "weight")
#' )
#' 
#' # mean2 can now be used inside precision_ict (and yields
#' # the same results as the mean statistic wrapper)
#' precision_ict(ict_survey, mean(speed_quanti), mean2(speed_quanti))
#' 
#' @export

define_statistic_wrapper <- function(statistic_function, 
                                     arg_type, 
                                     arg_not_affected_by_domain = NULL, 
                                     display_function = standard_display
){
  
  # Step I: Control arguments consistency
  inconsistent_arg <- list(
    in_arg_type_not_in_statistic_function = setdiff(unlist(arg_type), names(formals(statistic_function))), 
    in_statistic_function_not_in_arg_type = setdiff(names(formals(statistic_function)), unlist(arg_type)), 
    in_arg_not_affected_by_domain_not_in_statistic_function = setdiff(arg_not_affected_by_domain, names(formals(statistic_function)))
  )
  if(length(unlist(inconsistent_arg)) > 0) stop(
    "Some arguments are inconsistent:", 
    if(length(inconsistent_arg[[1]]) > 0) paste("\n  -", paste(inconsistent_arg[[1]], collapse = ", "), "in arg_type but not in statistic_function arguments") else "", 
    if(length(inconsistent_arg[[2]]) > 0) paste("\n  -", paste(inconsistent_arg[[2]], collapse = ", "), "in statistic_function arguments but not in arg_type") else "", 
    if(length(inconsistent_arg[[3]]) > 0) paste("\n  -", paste(inconsistent_arg[[3]], collapse = ", "), "in arg_not_affected_by_domain but not in statistic_function arguments") else ""
  )
  if(is.null(arg_type$weight))
    stop("A weight argument must be provided in order to create a statistic wrapper.")
  arg_domain <- list(
    data = setdiff(arg_type$data, arg_not_affected_by_domain),
    weight = setdiff(arg_type$weight, arg_not_affected_by_domain)
  )
  
  # Step II: Create the statistic wrapper
  statistic_wrapper <- function(by = NULL, where = NULL){

    # Step 1: Rewrite the call to add by and where from the variance wrapper call
    execution_envir <- get_through_parent_frame("execution_envir")
    evaluation_envir <- get("evaluation_envir", execution_envir)
    call_list <- as.list(match.call(expand.dots = TRUE))
    if(!("by" %in% names(call_list))) call_list$by <- substitute(by, execution_envir)
    if(!("where" %in% names(call_list))) call_list$where <- substitute(where, execution_envir)

    # Step 2: Rewrite the call to take standard evaluation into account
    data <- eval(substitute(data), execution_envir)
    if(!is.null(call_list$by)) call_list["by"] <- 
      replace_variable_name_with_symbol(call_list["by"], data = data)
    if(!is.null(call_list$where)) call_list["where"] <- 
      replace_variable_name_with_symbol(call_list["where"], data = data)
    data_arg <- replace_variable_name_with_symbol(
      call_list[arg_type$data], data = data, single = FALSE
    )
    call_list <- lapply(seq_along(data_arg[[1]]), function(c){
      call_list[names(data_arg)] <- lapply(data_arg, `[[`, c)
      call_list
    })

    # Step 3: Initialize the call and metadata slot
    data_as_list <- lapply(call_list, function(d) list(
      call = as.call(d),
      metadata = list(
        call = paste(deparse(as.call(d), width.cutoff = 500L), collapse = ""),
        by = NA, mod = NA
      )
    ))
    
    # Step 4: Evaluate the arguments and create the data, weight and param slots
    data_as_list <- lapply(data_as_list, function(d){
      d$data <- lapply(
        as.list(d$call)[arg_type$data], 
        eval, envir = data, enclos = evaluation_envir
      )
      d$weight <- lapply(
        stats::setNames(arg_type$weight, arg_type$weight),
        function(w) eval(substitute(reference_weight), envir = execution_envir)
      )
      d$param <- lapply(
        as.list(d$call)[arg_type$param], 
        eval, envir = evaluation_envir
      )
      d$metadata$row_number <- seq_along(d$data[[1]])
      d
    })
    if(all(sapply(data_as_list[[1]]$data, is.null))) return(NULL)

    # Step 5: Where 
    data_as_list <- lapply(data_as_list, function(d){
      if(is.null(d$call$where)) return(d)
      where <- as.logical(eval(d$call$where, envir = data, enclos = evaluation_envir))
      if(!any(where)) stop("where argument excludes all observations.")
      d$metadata$row_number <- d$metadata$row_number[where]
      d$data[arg_domain$data] <- lapply(d$data[arg_domain$data], `[`, where)
      d$weight[arg_domain$weight] <- lapply(d$weight[arg_domain$weight], `[`, where)
      d
    })
    
    # Step 6: By
    data_as_list <- unlist(lapply(data_as_list, function(d){
      if(is.null(d$call$by)) return(list(d))
      by <- droplevels(as.factor(eval(d$call$by, envir = data, enclos = evaluation_envir)))
      by_split <- split(seq_along(by), by)
      tmp <- lapply(levels(by), function(by_group){
        d_by <- d
        in_by_group <- d_by$metadata$row_number %in% by_split[[by_group]]
        if(!any(in_by_group)) return(NULL) 
        d_by$metadata$by <- by_group
        d_by$metadata$row_number <- d_by$metadata$row_number[in_by_group]
        d_by$data[arg_domain$data] <- 
          lapply(d_by$data[arg_domain$data], `[`, in_by_group)
        d_by$weight[arg_domain$weight] <- 
          lapply(d_by$weight[arg_domain$weight], `[`, in_by_group)
        d_by
      })
      tmp[!sapply(tmp, is.null)]
    }), recursive = FALSE)
    
    # Step 7: Handle factors and character variables in data
    data_as_list <- unlist(lapply(data_as_list, function(d){
      if(!any(sapply(d$data, function(var) is.character(var) || is.factor(var))))
        return(list(d))
      if(length(arg_type$data) > 1) stop("Character or factor variables aren't allowed.")
      tmp <- discretize_qualitative_var(d$data[[1]])
      lapply(colnames(tmp), function(mod){
        d_mod <- d
        d_mod$metadata$mod <- mod
        d_mod$data[[1]] <- tmp[, mod]
        d_mod
      })
    }), recursive = FALSE)

    # Step 8: Call the statistic function
    data_as_list <- lapply(data_as_list, function(d){
      statistic_function_arg <- 
        unlist(unname(d[c("data", "weight", "param")]), recursive = FALSE)
      tmp <- do.call(statistic_function, statistic_function_arg)
      d$statistic_function <- statistic_function
      d$point <- tmp$point
      d$lin <- if(!is.list(tmp$lin)) list(tmp$lin) else tmp$lin
      d$display_function <- display_function
      d$metadata <- c(d$metadata, tmp[setdiff(names(tmp), c("point", "lin", ""))])
      d
    })
    
    data_as_list
    
  }

  # Step III: Finalize the statistic wrapper
  
  # Step III.1: Modify statistic_wrapper formals
  formals(statistic_wrapper) <- c(
    formals(statistic_function)[setdiff(names(formals(statistic_function)), arg_type$weight)], 
    formals(statistic_wrapper)
  )

  # Step III.2: Include objects in statistic_wrapper enclosing environment
  e <- new.env(parent = globalenv())
  assign_all(objects = c("discretize_qualitative_var", "get_through_parent_frame", "replace_variable_name_with_symbol", "is_error", "is_variable_name"), to = e, from = asNamespace("gustave"))
  assign_all(objects = c("statistic_function", "arg_type", "arg_domain", "display_function"), to = e, from = environment())
  statistic_wrapper <- change_enclosing(statistic_wrapper, envir = e)
  
  structure(statistic_wrapper, class = c("function", "gustave_statistic_wrapper"))
  
}

standard_display <- function(point, var, metadata, alpha){
  # TODO: If installed, use tibble to add more explicit column labels
  metadata <- metadata[c("label", "call", "mod", "by", "n")]
  output_df <- as.data.frame(metadata[!sapply(metadata, is.null)], stringsAsFactors = FALSE)
  if(length(var) != length(point)) stop(
    "The number of estimated variances does not match the number of point estimates. A specific display function could be needed.", 
    "\n", call. = FALSE
  )
  output_df$est <- point
  output_df$variance <- var
  output_df$std <- sqrt(output_df$variance)
  output_df$cv <- output_df$std * 100 / output_df$est
  output_df$lower <- output_df$est - stats::qnorm(1 - alpha / 2) * output_df$std
  output_df$upper <- output_df$est + stats::qnorm(1 - alpha / 2) * output_df$std
  output_df
}
standard_display <- change_enclosing(standard_display, globalenv())




#' Define a function that computes statistics without the need to provide linearized variables.
#' @description Define a function that computes statistics without the need to provide linearized variables.
#' The estimated linearized variables are computed using autodifferentiation based
#' on \code{torch}.
#'
#' @param fn A function describing the total function to be estimated. All arguments from \code{fn} must be in \code{arg_type}. 
#' @param arg_type A list that specifies the arguments for the created function, 
#' including \code{data}, \code{weight}, and optionally \code{param}.
#'
#' @return A function that computes the estimated totals, applies 
#' the \code{fn} function, and returns a list with two elements:
#' \describe{
#'   \item{point}{The point estimate, as a numeric value.}
#'   \item{lin}{The linearized variable, defined as the dot product between the gradient 
#'   and the data.}
#' }
#' 
#' @details \code{fn} describes the function applied to the estimated totals. This function takes weighted totals as input, 
#' so weights do not need to be provided in \code{fn}.
#' @export
#'
#' @examples
#' ratio_autolin <- define_statistic_wrapper(
#'   statistic_function = auto_statistic_function(function(x, y) {return(x / y)}, arg_type),
#'   arg_type = arg_type
#' )

auto_statistic_function <- function(fn, arg_type){
  # The auto_statistic_function creates a function that meets the criteria of the statistic_function
  # argument in gustave::define_statistic_wrapper. It is based on the fn function,
  # which describes the total function the user wishes to estimate.
  # 
  # From this fn function, we create a fn_tensored function that takes the elements specified
  # in arg_type as arguments. This fn_tensored function computes the estimated totals, applies
  # the fn function, and calculates the gradient of the fn function with respect to the data 
  # (variables specified in arg_type$data).
  
  #Check if `torch` is installed.
  if (!requireNamespace("torch", quietly = TRUE)) {
    stop("The 'torch' package is required for this function.
         Please install it using install.packages('torch').")
  }
  #Ajout d'un test sur l'installation de torch.
  
  if(!identical(setdiff(sort(unname(unlist(arg_type))), arg_type$weight),
                sort((methods::formalArgs(fn))))){
    stop("Argument names from `fn` must be in `arg_type`.")
  }
  
  #Define an empty function
  fn_tensored <- function() NULL
  #Change formal arguments
  formals(fn_tensored) <- stats::setNames(vector("list", length(unlist(arg_type))), unlist(arg_type))
  #Change function body
  body(fn_tensored) <- quote({
    data_list <- mget(arg_type$data)
    #Compute total estimation for each variable in arg_type$data
    tot <- lapply(X = data_list, FUN = function(x){sum(x*get(arg_type$weight))})
    
    #Check if some totals are not numerical value (missing, NULL, ...)
    tot_not_numerical <- (sapply(X = tot, FUN = function(x){!is.numeric(x)}))
    
    if(sum(tot_not_numerical) > 1){
      var_not_numerical <- paste(arg_type$data[tot_not_numerical], collapse = " ")
      stop(paste0("Some variables provide a non-numerical weighted total : ", var_not_numerical))
    }
    #Transform it into (torch) tensors with requires_grad at TRUE:
    #That allows us to get the gradient of the point estimation
    #with respect to those variables
    tot_tensored <- lapply(X = tot, FUN = function(x){torch::torch_tensor(data = x,
                                                                          requires_grad = TRUE)})
    #Get arguments for point estimation.
    #In some case, arg_type$param = NULL then mget(arg_type$param) raises an error.
    #In order to avoid error, we add a condition on is.null(arg_type$param)
    args_for_point_estimation <- tot_tensored
    
    if(!is.null(arg_type$param)){
      args_for_point_estimation <- c(args_for_point_estimation,
                                     mget(arg_type$param))
    }
    
    #Compute point estimation.
    point <- do.call(what = fn, 
                     args = args_for_point_estimation)
    
    if("torch_tensor" %in% class(point)){
      if(is.na(point) | is.null(point)){
        stop("Results from `fn` must be a numerical vector of size 1.")
      }
    }
    
    #Compute the gradient of `fn` with respect to `data` inputs
    torch::autograd_backward(point)
    #Get gradients as matrix
    jac <- do.call(what = rbind,
                   args = lapply(X = tot_tensored, FUN = function(x){as.numeric(x$grad)}))
    #Get data as matrix
    data <- do.call(what = rbind,
                    args = data_list)
    #(Estimated) Linearization variable for unit i is defined as <grad, data_i>
    #where <,> denotes for the canonical dot product and data_i is the i-th row
    #corresponding to data from the i-th unit. 
    lin <- t(data) %*% jac
    #A list with two named elements `point` and `lin` is returned 
    #as required for the `statistic_function` argument of `gustave::define_statistic_wrapper`.
    n <- min(sapply(X = data_list, FUN = function(variable){sum(!is.na(variable))}))
    res <- list(point = as.numeric(point), lin = lin, n = n)
    return(res)
  })
  #Return result
  return(fn_tensored)
}

