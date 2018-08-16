
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
#' @param arg_not_affected_by_domain A character vector indicating the (data) 
#'   arguments which should not be affected by domain-splitting. Such parameters
#'   may appear in some complex linearization formula, for instance when the 
#'   At-Risk of Poverty Rate (ARPR) is estimated by region but with a poverty 
#'   line calculated at the national level.
#' @param  display_function An R function which produces, for each variance 
#'   estimation, the data.frame row to be displayed by the variance estimation 
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
#'   estimator) and should output a list with three named elements: \itemize{
#'   \item \code{point}: the point estimator of the statistic
#'   \item \code{lin}: the linearized variable to be passed on to the variance
#'   estimation formula. If several variables are to be associated with
#'   the statistics, \code{lin} can be a list itself.
#'   \item \code{metadata}: optional metadata to be used when displaying
#'   the results of the variance estimation.
#'   }
#'   
#'   \code{arg_type} is a named list of three elements that describe the nature 
#'   of the argument of \code{statistic_function}: \itemize{
#'   \item \code{data}: data argument(s), numerical vector(s) to be used 
#'   to calculate the point estimate and the linearized variable associated
#'   with the statistic 
#'   \item \code{weight}: weight argument, numerical vector to be used 
#'   as row weights in the linearization formula 
#'   \item \code{param}: parameters, non-data arguments to be used to 
#'   control some aspect of the computation of ths statistic}
#'   
#'   Statistic wrappers are quite flexible tools to apply a variance function 
#'   to an estimator requiring a linearization step (e.g. all estimators except 
#'   the estimator of a total) with virtually no  additional complexity for the
#'   end-user. To some extent, statistic wrappers can be seen as ggplot2
#'   \code{geom_} and \code{stat_} functions: they help the end-user in writing 
#'   down what he or she wants without having to go too  deep into the details 
#'   of the corresponding layers. 
#'   
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}} 
#'   are included within the gustave package and automatically added 
#'   to the variance estimation wrappers. New statistic wrappers can be defined
#'   using the \code{define_statistic_wrapper} and then explicitly added to the 
#'   variance estimation wrappers using the \code{objects_to_include} argument.
#'   
#' @return A function to be used within a variance estimation wrapper to estimate
#'   a specific statistic (see examples). Its formals are the ones of 
#'   \code{statistic_function} with the addition of \code{by} and \code{where} 
#'   (for domain estimation, set to \code{NULL} by default). 
#' 
#' @author Martin Chevalier
#'    
#' @seealso \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}} 
#'   \code{\link{define_variance_wrapper}}
#'   
#' @references 
#'   Deville J.-C. (1999), "Variance estimation for complex statistics and 
#'   estimators: linearization and residual techniques", \emph{Survey Methodology}, 
#'   25:193–203
#'   
#' @examples ### Example from the Information and communication technologies (ICT) survey
#' 
#' # The subset of the (simulated) ICT survey has the following features: 
#' # - stratified one-stage sampling design of 650 firms;
#' # - 612 responding firms, non-response correction through reweighting 
#' # in homogeneous response groups based on economic sub-sector and turnover;
#' # - calibration on margins (number of firms and turnover broken down
#' # by economic sub-sector).
#' 
#' # Step 1 : Dummy variance wrapper
#' # Note : see define_variance_wrapper() for a more 
#' # realistic variance function and examples.
#' variance_wrapper <- define_variance_wrapper(
#'   variance_function = function(y) abs(colSums(y)), 
#'   reference_id = ict_survey$firm_id, 
#'   reference_weight = ict_survey$w_calib, 
#'   default_id = "firm_id"
#' )
#' variance_wrapper(ict_survey, total(speed_quanti))
#' 
#' # Step 2 : Redefine the mean statistic wrapper
#' # The mean() statistic wrapper defined in the gustave 
#' # package is bulit on top of the ratio() statistic wrapper.
#' variance_wrapper(ict_survey, mean(speed_quanti))
#' 
#' # Let's redefine it directly from the formula found for instance
#' # in (Caron, Deville, Sautory, 1998) and without handling NA
#' # values
#' mean2 <- define_statistic_wrapper(
#'   statistic_function = function(y, weight){
#'     point <- sum(y * weight) / sum(weight)
#'     lin <- (y - point) / sum(weight)
#'     list(point = point, lin = lin, metadata = list(n = length(y)))
#'   },
#'   arg_type = list(data = "y", weight = "weight")
#' )
#' variance_wrapper(ict_survey, mean(speed_quanti), mean2(speed_quanti))
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
      d$metadata <- c(d$metadata, tmp$metadata)
      d$statistic_function <- statistic_function
      d$point <- tmp$point
      d$lin <- if(!is.list(tmp$lin)) list(tmp$lin) else tmp$lin
      d$display_function <- display_function
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


