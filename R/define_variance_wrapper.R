
#' Define a variance estimation wrapper

#' @description Given a variance estimation \emph{function} (specific to a 
#'   survey), \code{define_variance_wrapper} defines a variance estimation 
#'   \emph{wrapper} easier to use (e.g. automatic domain estimation, 
#'   linearization).
#'   
#' @param variance_function An R function. It is the methodological workhorse of 
#'   the variance estimation: from a set of arguments including the variables 
#'   of interest (see below), it should return a vector of estimated variances.
#'   See Details.
#' @param reference_id A vector containing the ids of all the responding units
#'   of the survey. It can also be an unevaluated expression (enclosed in 
#'   \code{quote()}) to be evaluated within the execution environment of the wrapper.
#'   It is compared with \code{default$id} (see below) to check whether 
#'   some observations are missing in the survey file. The matrix of variables 
#'   of interest passed on to \code{variance_function} has \code{reference_id} 
#'   as rownames and is ordered according to its values.
#' @param reference_weight A vector containing the reference weight of the survey. 
#'   It can also be an unevaluated expression (enclosed in \code{quote()}) to 
#'   be evaluated within the execution environment of the wrapper. 
#' @param default_id A character vector of length 1, the name of the default 
#'   identifying variable in the survey file. It can also be an unevaluated 
#'   expression (enclosed in \code{quote()}) to be evaluated within the survey file.
#' @param technical_data A named list of technical data needed to perform 
#'   the variance estimation (e.g. sampling strata, first- or second-order 
#'   probabilities of inclusion, estimated response probabilities, calibration 
#'   variables). Its names should match the names of the corresponding arguments 
#'   in \code{variance_function}.
#' @param technical_param A named list of technical parameters used to control 
#'   some aspect of the variance estimation process (e.g. alternative methodology).
#'   Its names should match the names of the corresponding arguments in \code{variance_function}.
#' @param objects_to_include (Advanced use) A character vector indicating the name of 
#'   additional R objects to include within the variance wrapper.

#' 
#'   
#' @details Defining variance estimation wrappers is the \strong{key feature} of
#'   the \code{gustave} package. It is the workhorse of the ready-to-use 
#'   \code{\link{qvar}} function and should be used directly to handle more complex
#'   cases (e.g. surveys with several stages or balanced sampling).
#'   
#'   Analytical variance estimation is often difficult to carry out by 
#'   non-specialists owing to the complexity of the underlying sampling 
#'   and estimation methodology. This complexity yields complex \emph{variance 
#'   estimation functions} which are most often only used by the sampling expert 
#'   who actually wrote them. A \emph{variance estimation wrapper} is an 
#'   intermediate function that is "wrapped around" the (complex) variance 
#'   estimation function in order to provide the non-specialist with 
#'   user-friendly features (see examples): \itemize{
#'   \item calculation of complex statistics (see 
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}})
#'   \item domain estimation 
#'   \item handy evaluation and factor discretization
#'   }
#'   
#'   \code{define_variance_wrapper} allows the sampling expert to define a 
#'   variance estimation wrapper around a given variance estimation function and
#'   set its default parameters. The produced variance estimation wrapper is 
#'   standalone in the sense that it contains all technical data necessary
#'   to carry out the estimation (see \code{technical_data}).
#'   
#'   The arguments of the \code{variance_function} fall into three types: \itemize{
#'   \item the data argument (mandatory, only one allowed): the numerical matrix of 
#'   variables of interest to apply the variance estimation formula on
#'   \item technical data arguments (optional, one or more allowed): technical 
#'   and methodological information used by the variance estimation function
#'   (e.g. sampling strata, first- or second-order probabilities of inclusion, 
#'   estimated response probabilities, calibration variables)
#'   \item technical parameters (optional, one or more allowed): non-data arguments 
#'   to be used to control some aspect of the variance estimation (e.g. alternative
#'   methodology)}
#'   
#'   \code{technical_data} and \code{technical_param} are used to determine
#'   which arguments of \code{variance_function} relate to technical information, 
#'   the only remaining argument is considered as the data argument.
#'   
#' @return An R function that makes the estimation of variance based on the
#'   provided variance function easier. Its parameters are: \itemize{ \item
#'   \code{data}: one or more calls to a statistic wrapper (e.g. \code{total()},
#'   \code{mean()}, \code{ratio()}). See examples and
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}}) and
#'   \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}})
#'   \item \code{where}: a logical vector indicating a domain on which the
#'   variance estimation is to be performed \item \code{by}: q qualitative
#'   variable whose levels are used to define domains on which the variance
#'   estimation is performed \item \code{alpha}: a numeric vector of length 1
#'   indicating the threshold for confidence interval derivation (\code{0.05} by
#'   default) \item \code{display}: a logical verctor of length 1 indicating
#'   whether the result of the estimation should be displayed or not \item
#'   \code{id}: a character vector of size 1 containing the name of the
#'   identifying variable in the survey file. Its default value depends on the
#'   value of \code{default_id} in \code{define_variance_wrapper} \item
#'   \code{envir}: an environment containing a binding to \code{data}}
#' 
#' @author Martin Chevalier
#'   
#' @references Rao, J.N.K (1975), "Unbiased variance estimation for multistage designs",
#'   \emph{Sankhya}, C n°37
#'   
#' @seealso \code{\link{qvar}}, \code{\link[=standard_statistic_wrapper]{standard statistic wrappers}}, \code{\link{varDT}}
#' 
#' @examples ### Example from the Labour force survey (LFS)
#' 
#' # The (simulated) Labour force survey (LFS) has the following characteristics:
#' # - first sampling stage: balanced sampling of 4 areas (each corresponding to 
#' #   about 120 dwellings) on first-order probability of inclusion (proportional to 
#' #   the number of dwellings in the area) and total annual income in the area.
#' # - second sampling stage: in each sampled area, simple random sampling of 20 
#' #   dwellings
#' # - neither non-response nor calibration
#' 
#' # As this is a multi-stage sampling design with balanced sampling at the first
#' # stage, the qvar function does not apply. A variance wrapper can nonetheless
#' # be defined using the core define_variance_wrapper function.
#' 
#' # Step 1 : Definition of the variance function and the corresponding technical data
#' 
#' # In this context, the variance estimation function specific to the LFS 
#' # survey can be defined as follows:
#' 
#' var_lfs <- function(y, ind, dwel, area){
#'   
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
#' # where y is the matrix of variables of interest and ind, dwel and area the technical data:
#' 
#' technical_data_lfs <- list()
#' 
#' # Technical data at the area level
#' # The varDT function allows for the pre-calculation of 
#' # most of the methodological quantities needed.
#' technical_data_lfs$area <- varDT(
#'   y = NULL, 
#'   pik = lfs_samp_area$pik_area, 
#'   x = as.matrix(lfs_samp_area[c("pik_area", "income")]),
#'   id = lfs_samp_area$id_area
#' )
#' 
#' # Technical data at the dwelling level
#' # In order to implement Rao (1975) formula for two-stage samples,
#' # we associate each dwelling with the diagonal term corresponding 
#' # to its area in the first-stage variance estimator: 
#' lfs_samp_dwel$q_area <- with(technical_data_lfs$area, setNames(diago, id))[lfs_samp_dwel$id_area]
#' technical_data_lfs$dwel <- lfs_samp_dwel[c("id_dwel", "pik_dwel", "id_area", "pik_area", "q_area")]
#' 
#' # Technical data at the individual level
#' technical_data_lfs$ind <- lfs_samp_ind[c("id_ind", "id_dwel", "sampling_weight")]
#' 
#' # Test of the variance function var_lfs
#' y <- matrix(as.numeric(lfs_samp_ind$unemp), ncol = 1, dimnames = list(lfs_samp_ind$id_ind))
#' with(technical_data_lfs, var_lfs(y = y, ind = ind, dwel = dwel, area = area))
#'
#'
#' # Step 2 : Definition of the variance wrapper
#' 
#' # Call of define_variance_wrapper
#' precision_lfs <- define_variance_wrapper(
#'   variance_function = var_lfs,
#'   technical_data = technical_data_lfs, 
#'   reference_id = technical_data_lfs$ind$id_ind,
#'   reference_weight = technical_data_lfs$ind$sampling_weight,
#'   default_id = "id_ind"
#' )
#' 
#' # Test
#' precision_lfs(lfs_samp_ind, unemp)
#' 
#' # The variance wrapper precision_lfs has the same features
#' # as variance wrappers produced by the qvar function (see
#' # qvar examples for more details).
#' 
#' @import Matrix
#' @export

define_variance_wrapper <- function(variance_function, 
                                    reference_id,
                                    reference_weight,
                                    default_id = NULL,
                                    technical_data = NULL,
                                    technical_param = NULL,
                                    objects_to_include = NULL
){

  # TODO: Add some sort of startup message on first run of the function (check what Matrix does)
  sys_time <- Sys.time()
  session_info <- utils::sessionInfo()

  # Step I: Controls 

  # Missing arguments  
  is_missing <- c(
    variance_function = missing(variance_function),
    reference_id = missing(reference_id),
    reference_weight = missing(reference_weight)
  )
  if(any(is_missing)) stop(
    "The following arguments are missing: ", 
    paste(names(which(is_missing)), collapse = ", "), "."
  )
  
  # Determine argument type
  names_formals_variance_function <- names(formals(variance_function))
  names_technical_data <- names_else_NA(technical_data)
  if(anyNA(names_technical_data)) stop("All elements of technical_data must be named.")
  if(!all(names_technical_data %in% names_formals_variance_function))
    stop("All elements of technical_data must match an argument of variance_function.")
  names_technical_param <- names_else_NA(technical_param)
  if(anyNA(names_technical_param)) stop("All elements of technical_param must be named.")
  if(!all(names_technical_param %in% names_formals_variance_function))
    stop("All elements of technical_param must match an argument of variance_function.")
  names_remaining_args <- setdiff(names_formals_variance_function, c(names_technical_data, names_technical_param))
  if(length(names_remaining_args) == 0) stop(
    "variance_function appears to have only technical arguments (identified with technical_data and technical_param).",
    " It must also have a data argument (see the Details section in help)."
  )
  if(length(names_remaining_args) > 1) stop(
    "variance_function appears to have several data arguments (", paste(names_remaining_args, collapse = ", "), 
    ") where it should only have one. Use technical_data and technical_param to identify the technical arguments (see the Details section in help)."
  )
  arg_type <- list(data = names_remaining_args, tech_data = names_technical_data, tech_param = names_technical_param)

  # Step II: Create the variance wrapper
  variance_wrapper <- function(
    data, ..., by = NULL, where = NULL, id, 
    alpha = 0.05, display = TRUE, envir = parent.frame()
  ){

    if(!("package:Matrix" %in% search())) attachNamespace("Matrix")

    # Step 1: Preliminary operations and controls
    
    # Step 1.1: Environments and missing arguments
    evaluation_envir <- envir
    execution_envir <- environment()
    is_missing <- c(
      data = missing(data),
      id = missing(id) && is.symbol(formals()$id)
    )
    if(any(is_missing)) stop(
      "The following arguments are missing: ", 
      paste(names(which(is_missing)), collapse = ", "), "."
    )
    
    # Step 1.2 Evaluation 
    data <- eval(data, envir = evaluation_envir)
    reference_id <- eval(reference_id)
    id <- tryCatch(
      eval(substitute(id), envir = execution_envir),
      error = function(e) substitute(id, execution_envir)
    )
    id <- if(is.character(id)) data[[id]] else eval(id, data)
    reference_weight <- eval(reference_weight)
    
    # Step 1.3: Controls
    in_reference_id_not_in_id <- setdiff(reference_id, id)
    if(length(in_reference_id_not_in_id) > 0)
      warn("Some observations from the survey appear to be missing. The variance estimation function may produce unexpected results.")
    in_id_not_in_reference_id <- setdiff(id, reference_id)
    if(length(in_id_not_in_reference_id) > 0){
      warn(length(in_id_not_in_reference_id), " observations do not match any responding units of the survey. They are discarded.")
      data <- data[id %in% reference_id, ]
      id <- id[id %in% reference_id]
    }
    if(!identical(match_id <- match(reference_id, id), seq_along(reference_id))){
      warn(
        "The inputted id variable (id argument) appears not to match the reference ",
        "id variable provided when the variance wrapper was defined: it is reordered ",
        "and everything should be fine. Issues may nonetheless arise if part of the call ", 
        "is to be evaluated outside of the inputted data.frame (data argument)."
      )
      data <- data[match_id, ]  
    }

    
    # Step 2: Handling domains, qualitative variables and linearization
    statistic_wrapper_list <- eval(substitute(alist(...)))
    statistic_wrapper_label <- names_else_NA(statistic_wrapper_list)
    data_as_list <- unlist(lapply(seq_along(statistic_wrapper_list), function(i){

      call <- statistic_wrapper_list[[i]]
      
      # Add a statistic wrapper when none is spefified
      if(is.symbol(call) || !is_statistic_wrapper(eval(call[[1]]))) 
        call <- as.call(c(quote(total), call))

      # Evaluate the statistic wrapper
      d <- eval(call)
      if(is.null(d)) return(d)

      # Add labels
      lapply(d, function(slice){
        slice$metadata$label <- statistic_wrapper_label[i]
        slice
      })

    }), recursive = FALSE)
    if(is.null(data_as_list)) stop("No variable to estimate variance on.")
    
    
    # Step 3: Variance estimation
    
    # Step 3.1: Build up the sparse matrix of linearized variables to be used in the estimation
    data_as_Matrix <- list(
      lin = Matrix::sparseMatrix(
        i = unlist(lapply(data_as_list, function(slice) rep(slice$metadata$row_number, length(slice$lin))), use.names = FALSE),
        p = c(0, cumsum(unlist(lapply(data_as_list, function(slice) 
          rep(length(slice$metadata$row_number), length(slice$lin))
        ), use.names = FALSE))),
        x = unlist(lapply(data_as_list, function(slice) do.call(base::c, slice$lin)), use.names = FALSE),
        dims = c(length(reference_id), sum(sapply(lapply(data_as_list, `[[`, "lin"), length))),
        dimnames = list(reference_id, NULL), 
        check = FALSE
      ),
      slice_number = unlist(lapply(seq_along(data_as_list), function(i)
        rep(i, sapply(lapply(data_as_list, `[[`, "lin"), length)[i])
      ), use.names = FALSE)
    )

    # Step 3.2: Call the variance estimation function
    variance_function_args <- c(
      stats::setNames(list(data_as_Matrix$lin), arg_type$data), 
      stats::setNames(lapply(arg_type$tech_param, get, envir = execution_envir), arg_type$tech_param), 
      technical_data
    )
    variance_function_result <- suppressMessages(do.call(variance_function, variance_function_args))
    
    # Step 3.3: Test and reorganize variance_function results
    is_list_variance_function_result <- is.list(variance_function_result)
    if(!is_list_variance_function_result) variance_function_result <- list(var = variance_function_result) 
    if(!any(names(variance_function_result) == "var")) 
      stop("At least one output of variance_function should be named \"var\".")
    if(!is.vector(variance_function_result$var))
      stop("The ", if(is_list_variance_function_result) "\"var\" " else NULL,"output of variance_function should be a vector.")
    data_as_Matrix <- c(data_as_Matrix, variance_function_result)

    # Step 3.4: Reintroduce the "var" output of variance_function within data_as_list
    data_as_list <- lapply(seq_along(data_as_list), function(i){
      slice <- data_as_list[[i]]
      slice$var <- data_as_Matrix$var[data_as_Matrix$slice_number == i]
      slice
    })
    
      
    # Step 4: Display the results (if requested)
    if(!display) return(invisible(list(
      data_as_list = data_as_list, data_as_Matrix = data_as_Matrix
    )))
    list_output_df <- lapply(data_as_list, function(i) with(i, display_function(
      point = point, var = var, metadata = metadata[c("label", "call", "mod", "by", "n")], alpha = alpha
    )))
    rbind_output_df(list_output_df)
  
  }

  # Step III: Finalize the variance wrapper
  
  # Step III.1: Modify variance wrapper arguments depending on the context
  if(!is.null(default_id)) formals(variance_wrapper)$id <- default_id

  # Step III.2: Add variance_function technical parameters to variance_wrapper arguments
  # (just after the ...)
  add_technical_param_after <- match("...", names(formals(variance_wrapper)))
  formals(variance_wrapper) <- c(
    formals(variance_wrapper)[1:add_technical_param_after], 
    technical_param,
    formals(variance_wrapper)[(add_technical_param_after + 1):length(formals(variance_wrapper))]
  )
  
  # Step III.3: Include objects in variance_wrapper enclosing environment
  e1 <- new.env(parent = globalenv())
  assign_all(objects = ls(asNamespace("gustave")), to = e1, from = asNamespace("gustave"))
  e2 <- new.env(parent = e1)
  assign_all(objects = c(
    "sys_time", "session_info",
    "variance_function", "reference_id", "reference_weight", "technical_data", "arg_type"
  ), to = e2, from = environment())
  assign_all(objects = objects_to_include, to = e2, from = parent.frame())
  variance_wrapper <- change_enclosing(variance_wrapper, envir = e2)

  structure(variance_wrapper, class = c("function", "gustave_variance_wrapper"))

}
