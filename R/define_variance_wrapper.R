
#' Define a variance estimation wrapper

#' @description Given a variance estimation \emph{function} (specific to a 
#'   survey), \code{define_variance_wrapper} defines a variance estimation 
#'   \emph{wrapper} easier to use (e.g. automatic domain estimation, 
#'   linearization).
#'   
#' @param variance_function An R function, with input a data matrix and possibly 
#'   other arguments (e.g. parameters affecting the estimation of variance), 
#'   and output a numeric vector of estimated variances (or a list whose first 
#'   element is a numeric vector of estimated variances).
#' @param reference_id A vector containing the ids of all the responding units 
#'   of the survey. It is compared with \code{default$id} to check whether some 
#'   observations are missing in the survey file. Observations are reordered 
#'   according to \code{reference_id}.
#' @param default a named list specifying the default values for: \itemize{
#'   \item \code{id}: the name of the default identifying variable in the survey 
#'   file. It can also be an unevaluated expression (enclosed in \code{substitute()}) to be 
#'   evaluated within the survey file.
#'   \item \code{weight}: the name of the default weight variable in the survey file. 
#'   It can also be an unevaluated expression (enclosed in \code{substitute()}) to be 
#'   evaluated within the survey file.
#'   \item \code{stat}: the name of the default statitic to compute when none is specified. 
#'   It is set to \code{"total"} by default.
#'   \item \code{alpha}: the default threshold for confidence interval derivation. 
#'   It is set to \code{0.05} by default.
#' }
#' @param objects_to_include A character vector indicating the name of 
#'   additional R objects to include within the variance wrapper. These objects 
#'   are to be used to carry out the variance estimation.
#' @param objects_to_include_from The environment to which the additional R 
#'   objects belong.
#'   
#' @details Defining variance estimation wrappers is the \strong{key feature} of
#'   the \code{gustave} package.
#'   
#'   Analytical variance estimation is often difficult to carry out by 
#'   non-specialists owing to the complexity of the underlying sampling 
#'   and estimation methodology. This complexity yields complex \emph{variance estimation 
#'   functions} which are most often only used by the methodologists who 
#'   actually wrote them. A \emph{variance estimation wrapper} is an 
#'   intermediate function that is "wrapped around" the (complex) variance 
#'   estimation function in order to provide the non-specialist with 
#'   user-friendly features: \itemize{ \item checks for consistency between the 
#'   provided dataset and the survey characteristics \item factor discretization
#'   \item domain estimation \item linearization of complex statistics (see 
#'   \code{define_linearization_wrapper})}
#'   
#'   \code{define_variance_wrapper} allows the methodologist to define a 
#'   variance estimation wrapper around a given variance estimation function and
#'   set its default parameters. The produced variance estimation wrapper will 
#'   be stand-alone in the sense that it can contain additional data which would
#'   \code{objects_to_include} and \code{objects_to_include_from} parameters).
#'   
#' @return An R function that makes the estimation of variance based on the provided 
#' variance function easier. Its parameters are:
#'   \itemize{
#'    \item \code{data}: the survey data where the interest variables are stored
#'    \item \code{...}: one or more calls to a linearization wrapper (see examples
#'    and \code{\link{define_linearization_wrapper}})
#'    \item \code{where}: a logical vector indicating a domain on which the variance
#'    estimation is conducted
#'    \item \code{by}: a qualitative variable whose levels are used to define domains
#'    on which the variance estimation is conducted
#'    \item \code{stat}: a character vector of size 1 indicating the linearization
#'    wrapper to use when none is specified. Its default value depends on
#'    the value of \code{default_stat} in \code{define_variance_wrapper}
#'    \item \code{alpha}: a numeric vector of size 1 indicating the threshold
#'    for confidence interval derivation. Its default value depends on
#'    the value of \code{default_alpha} in \code{define_variance_wrapper}
#'    \item \code{id}: a character vector of size 1 containing the name of
#'    the identifying variable in the survey file. It can also be an 
#'    unevaluated expression (using \code{substitute()}) to be evaluated within
#'    the survey file. Its default value depends on the value of 
#'    \code{default_id} in \code{define_variance_wrapper}
#'    \item \code{envir}: an environment containing a binding to \code{data}
#'  }
#' 
#' @author Martin Chevalier (Insee)
#'    
#' @seealso \code{\link{define_linearization_wrapper}} \code{\link{varDT}}
#' 
#' @examples ### Survey setup
#' 
#' # Let's consider a survey drawn with a one-stage unequal 
#' # probability sampling and neither non-response nor calibration.
#' N <- 1000 # Size of the population
#' n <- 20 # Size of the sample
#' 
#' # Randomly generating a sampling frame and first-order 
#' # probabilities of inclusion
#' set.seed(1)
#' frame <- data.frame(id = 1:N, pik = runif(N))
#' frame$pik <- frame$pik * n / sum(frame$pik)
#' sum(frame$pik) # n
#'
#' # Drawing the sample with sampling::UPmaxentropy()
#' library(sampling)
#' sample <- frame[as.logical(UPmaxentropy(frame$pik)), ]
#' 
#' # Randomly generating the data that would have been collected
#' # on the field and storing it in the "survey" data.frame 
#' survey <- data.frame(
#'   id = sample$id
#'   , weight = 1 / sample$pik
#'   , var1 = 10 + rnorm(n)
#'   , var2 = letters[sample.int(3, n, replace = TRUE)]
#'   , var3 = 5 + rnorm(n)*2
#' )
#' 
#' # Sorting the "sample" and "survey" objects by id
#' sample <- sample[order(sample$id), ]
#' survey <- survey[order(survey$id), ]
#' 
#' 
#' ### Definition of the variance wrapper
#' 
#' # Using the varDT function for estimating the variance
#' # (see gustave::varDT)
#' varDT(y = survey$var1, pik = sample$pik)
#' 
#' # Definition of the variance wrapper
#' variance_wrapper <- define_variance_wrapper(
#'   variance_function = function(y) varDT(y = y, pik = sample$pik)
#'   , reference_id = sample$id
#'   , default = list(id = "id", weight = "weight")
#'   , objects_to_include = "sample"
#' )
#' 
#' # The data.frame "sample" is embedded within the function 
#' # variance_wrapper
#' ls(environment(variance_wrapper))
#' str(environment(variance_wrapper)$sample)
#' # Note : variance_wrapper is a closure 
#' # (http://adv-r.had.co.nz/Functional-programming.html#closures)
#' 
#' 
#' ### Features of the variance wrapper
#' 
#' # Better display of results
#' variance_wrapper(survey, var1)
#' 
#' # Discretization of qualitative variables
#' variance_wrapper(survey, var2)
#' # On-the-fly recoding
#' variance_wrapper(survey, var2 == "a") 
#' 
#' # 1-domain estimation
#' variance_wrapper(survey, var1, where = var2 == "a") 
#' # Multiple domains estimation
#' variance_wrapper(survey, var1, by = var2) 
#' 
#' # Mean linearization
#' variance_wrapper(survey, mean(var1)) 
#' # Ratio linearization
#' variance_wrapper(survey, ratio(var2 == "a", var2 %in% c("a", "b"))) 
#' 
#' # Multiple variables at a time
#' variance_wrapper(survey, var1, var3)
#' variance_wrapper(survey, mean(var1), total(var3))
#' # Flexible syntax for where and by arguments
#' # (similar to the aes() function in ggplot2)
#' variance_wrapper(survey, where = var2 == "c"
#'   , mean(var1), total(var3)
#' )
#' variance_wrapper(survey, where = var2 == "c"
#'   , mean(var1), total(var3, where = var2 == "a")
#' )
#' variance_wrapper(survey, where = var2 == "c"
#'   , mean(var1), total(var3, where = NULL)
#' )
#' 
#' @export define_variance_wrapper
#' @import Matrix

define_variance_wrapper <- function(
  variance_function, reference_id
  , default = list(stat = "total", alpha = 0.05)
  , objects_to_include = NULL, objects_to_include_from = parent.frame()
){

  if(!("package:Matrix" %in% search())) attachNamespace("Matrix")
  
  # Step 0 : Work with default argument
  if(is.null(default$stat) && !("stat" %in% names(default))) default$stat <- "total"
  if(is.null(default$alpha) && !("alpha" %in% names(default))) default$alpha <- 0.05
  
  # Step 1 : Creating the variance estimation wrapper
  variance_wrapper <- function(
    data, ..., weight = NULL, by = NULL, where = NULL, stat = NULL, alpha = NULL
    , display = TRUE, id = NULL, w = NULL, envir = parent.frame()
  ){

    # Step 1.0: Retrieve information about the environments,
    # the call and evaluate the data argument
    evaluation_envir <- envir
    execution_envir <- environment()
    call <- match.call(expand.dots = TRUE)
    substitute_data <- substitute(data)
    eval_data <- eval(substitute_data, evaluation_envir)
    
    # Step 1.1 : Control identifiers
    reference_id <- eval(reference_id)
    id <- if(is.character(id)) eval_data[, id] else eval(id, eval_data)      
    in_reference_id_not_in_id <- setdiff(reference_id, id)
    if(length(in_reference_id_not_in_id) > 0)
      warning("Some observations from the survey appear to be missing. The variance estimation function may produce unexpected results.", call. = FALSE)
    in_id_not_in_reference_id <- setdiff(id, reference_id)
    if(length(in_id_not_in_reference_id) > 0)
      stop("Some observations do not belong to the survey.", call. = FALSE)

    # Step 1.2 : Specify default values for stat, weight, by and where arguments
    l <- eval(substitute(alist(...)))
    l <- lapply(l, function(i){
      if(is.symbol(i) || !("gustave_linearization_wrapper" %in% class(eval(i[[1]]))))
        i <- as.call(c(as.symbol(stat), i))
      i <- as.list(i)
      for(j in environment(eval(i[[1]]))$arg_type$weight){
        if(!(j %in% names(i))) i[[j]] <- if(is.character(weight)) as.symbol(weight) else weight
      }
      if(!("by" %in% names(i))) i$by <- substitute(by, execution_envir)
      if(!("where" %in% names(i))) i$where <- substitute(where, execution_envir)
      as.call(i)
    })
    # TODO: keep track of the non-automatically assigned weight arguments
    # in order to display them in the call columns of the output (see
    # define_linearization_wrapper, about row 44).
  
    # Step 1.3 : Call the linearization wrappers
    labels <- if(!is.null(names(l))) names(l) else rep(NA, length(l))
    labels[labels %in% ""] <- NA
    d <- unlist(lapply(seq_along(l), function(i){
      linearization_wrapper_call <- as.call(c(as.list(l[[i]]), list(technical_arg = list(
        data = substitute_data, label = labels[i]
        , evaluation_envir = evaluation_envir, execution_envir = execution_envir
      ))))
      eval(linearization_wrapper_call, envir = execution_envir)
    }), recursive = FALSE)
    d <- list(
      preparation = lapply(d, `[[`, "preparation")
      , estimation = NULL
      , display = lapply(d, `[[`, "display")
    )
    
    # Step 1.4 : Build up the sparse matrix to be used in the estimation
    d$estimation$data <- {
      data <- lapply(d$preparation, function(k){
        t <- do.call(cbind, k$lin)
        Matrix::sparseMatrix(
          i = rep(k$metadata$bypos, NCOL(t))
          , j = rep(1:NCOL(t), each = NROW(t)), giveCsparse = FALSE
          , x = c(t), dims = c(length(id), NCOL(t)), check = FALSE
        )
      })
      data <- methods::as(Matrix::drop0(do.call(cbind, data)), "TsparseMatrix")
      data@i <- as.integer(match(id, reference_id)[data@i + 1] - 1)
      data@Dim <- c(length(reference_id), NCOL(data))
      data@Dimnames <- list(as.character(reference_id), NULL)
      data
    }

    # Step 1.5 : Call the variance estimation function
    d$estimation$variance_function <- variance_function
    variance_function_args <- c(
      list(d$estimation$data)
      , lapply(names(formals(variance_function))[-1], get, envir = execution_envir)
    )
    r <- suppressMessages(do.call(variance_function, variance_function_args))
    if(is.data.frame(r)) r <- as.matrix(r)
    if(!is.list(r)) r <- list(var = r)
    d$estimation$result <- r
    
    # Step 1.6 Reorganize the results of the estimation
    k <- 0;
    d$display <- lapply(seq_along(d$display), function(i) c(d$display[[i]]
      , list(var = lapply(d$preparation[[i]]$lin, function(j){
        t <- d$estimation$result[[1]][(k + 1):(k + NCOL(j))]
        assign("k", (k + NCOL(j)), envir = execution_envir)
        return(t)
      }))
    ))

    # Step 1.7 : Display the results if requested (the default)
    if(display){
      d <- lapply(d$display, function(i) i$fun(i, alpha = alpha))
      names <- unique(do.call(base::c, lapply(d, names)))
      d <- do.call(rbind, lapply(d, function(i){
        i[, setdiff(names, names(i))] <- NA
        i[, names]
      }))
      d <- d[, sapply(d, function(i) !all(is.na(i)))]
      rownames(d) <- NULL
      return(d)
    }else return(invisible(d))

  }

  # Step 2 : Modify variance_wrapper arguments depending on the context
  if(!is.null(default$id)) formals(variance_wrapper)$id <- substitute(default$id)
  if(!is.null(default$weight)) formals(variance_wrapper)$weight <- substitute(default$weight)
  if(!is.null(default$stat)) formals(variance_wrapper)$stat <- default$stat
  if(!is.null(default$alpha)) formals(variance_wrapper)$alpha <- default$alpha
  formals(variance_wrapper) <- c(formals(variance_wrapper), formals(variance_function)[names(formals(variance_function))[-1]])

  # Step 3 : Include objects in variance_wrapper enclosing environment
  e1 <- new.env(parent = globalenv())
  assign_all(objects = ls(asNamespace("gustave")), to = e1, from = asNamespace("gustave"))
  e2 <- new.env(parent = e1)
  assign_all(objects = c("variance_function", "reference_id"), to = e2, from = environment())
  assign_all(objects = objects_to_include, to = e2, from = objects_to_include_from)
  variance_wrapper <- change_enclosing(variance_wrapper, envir = e2)

  variance_wrapper <- structure(variance_wrapper, class = c("function", "gustave_variance_wrapper"))

  return(variance_wrapper)

}

