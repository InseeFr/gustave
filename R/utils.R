

#' Efficient by-group (weighted) summation
#' 
#' @description \code{sum_by} performs an efficient and optionally weighted 
#' by-group summation by using linear algebra and the Matrix package 
#' capabilities. The by-group summation is performed through matrix cross-product
#' of the y parameter (coerced to a matrix if needed) with a (very) sparse
#' matrix built up using the by and the (optional) w parameters. 
#' 
#' Compared to base R, dplyr or data.table alternatives, this implementation 
#' aims at being easier to use in a matrix-oriented context and can yield 
#' efficiency gains when the number of columns becomes high.
#' 
#' @param y A (sparse) vector, a (sparse) matrix or a data.frame. 
#' The object to perform by-group summation on. 
#' @param by The factor variable defining the by-groups. Character variables
#' are coerced to factors.
#' @param w The optional weight to be used in the summation. 
#' @param na_rm Should NA values in y be removed (ie treated as 0 in the summation) ? 
#' Similar to na.rm argument in \code{\link[base]{sum}}, but TRUE by default. 
#' If FALSE, NA values in y produce NA values in the result.
#' @param keep_sparse When y is a sparse vector or a sparse matrix, should the result
#' also be sparse ? FALSE by default. As \code{\link[Matrix]{sparseVector-class}} does
#' not have a name attribute, when y is a sparseVector the result does not have any
#' name (and a warning is cast).
#' 
#' @return A vector, a matrix or a data.frame depending on the type of y. If y is
#' sparse and keep_sparse is TRUE, then the result is also sparse (without names
#' when it is a sparse vector, see keep_sparse argument for details).
#' 
#' @author Martin Chevalier
#' 
#' @examples # Data generation
#' set.seed(1)
#' n <- 100
#' p <- 10
#' H <- 3
#' y <- matrix(rnorm(n*p), ncol = p, dimnames = list(NULL, paste0("var", 1:10)))
#' y[1, 1] <- NA
#' by <- letters[sample.int(H, n, replace = TRUE)]
#' w <- rep(1, n)
#' w[by == "a"] <- 2
#' 
#' # Standard use
#' sum_by(y, by)
#' 
#' # Keeping the NAs
#' sum_by(y, by, na_rm = FALSE)
#' 
#' # With a weight
#' sum_by(y, by, w = w)
#' 
#' @export
#' @import Matrix

sum_by <- function(y, by, w = NULL, na_rm = TRUE, keep_sparse = FALSE){

  # y <- V
  
  # Type of y
  class_y <- class(y)
  is_data.frame_y <- is.data.frame(y)
  if(is_data.frame_y) y <- as.matrix(y)
  is_sparse_y <- inherits(y, c("Matrix", "sparseVector"))
  is_vector_y <- is.null(dim(y))
  is_numeric_y <- is.numeric(if(!is_sparse_y) y else y@x)
  if(!is_numeric_y) stop("y is not numeric (or not entirely).")
  if(!is_sparse_y | is_vector_y) y <- methods::as(y, "sparseMatrix")
  
  # Weight, NA in y
  if(is.null(w)) w <- rep(1, NROW(y))
  if(!is.numeric(w)) stop("w is not numeric")
  if(na_rm) y[is.na(y)] <- 0
  
  # NA in by
  NA_in_by <- is.na(by)
  if(any(NA_in_by)){
    y <- y[!NA_in_by, , drop = FALSE]
    by <- by[!NA_in_by]
    w <- w[!NA_in_by]
  }
  
  # Matrix cross-product
  by <- as.factor(by)
  x <- make_block(w, by)
  colnames(x) <- levels(by)
  r <- crossprod(x, y)

  # Type of r
  if(!is_sparse_y | !keep_sparse){
    r <- if(is_vector_y) stats::setNames(as.vector(r), rownames(r)) else as.matrix(r)
  }else{
    if(is_vector_y) warn("sparseVector can't have names, hence the output won't have names.")
    r <- methods::as(r, class_y)
  }
  if(is_data.frame_y) r <- as.data.frame(r)
  
  r
  
}


#' Expand a matrix or a data.frame with zeros based on rownames matching
#'
#' @description For a given two-dimensional object with rownames and a character
#'   vector, \code{add_zero} produces a corresponding object whose rownames match
#'   the character vector, with zeros on the additional rows.
#'
#'   This function is an easy-to-use and reliable way to reintroduce
#'   non-responding units in the variance estimation process (after the
#'   non-response phase is taken into account).
#'
#' @param y A (sparse) matrix or a data.frame. The object to add zeros to.
#' @param rownames A character vector (other types are coerced to character).
#'   The character vector giving the rows of the produced object.
#' @param remove Should rows of y whose name do not appear in the rownames
#'   argument be removed ? TRUE by default, a warning is shown when rows are
#'   removed.
#'
#' @return A (sparse) matrix or data.frame depending on the type of y.
#'
#' @author Martin Chevalier
#'
#' @examples # Data generation
#' set.seed(1)
#' n <- 10
#' p <- 2
#' y <- matrix(1:(n*p), ncol = p, dimnames = list(sample(letters, n)))
#' y[c(3, 8, 12)] <- NA
#' rownames <- letters
#'
#' # Standard use
#' add_zero(y, rownames)
#'
#' # Use when rownames in y do not match
#' # any element in the rownames argument
#' rownames(y)[1:3] <- toupper(rownames(y)[1:3])
#' add_zero(y, rownames)
#' add_zero(y, rownames, remove = FALSE)
#'
#' @import Matrix
#' @export
#' 
add_zero <- function(y, rownames, remove = TRUE){
  
  # y <- m; rownames <- letters
  
  # Type of y
  class_y <- class(y)
  is_data.frame_y <- is.data.frame(y)
  if(is_data.frame_y) y <- as.matrix(y)
  if(is.null(dim(y)))
    stop("y must be a (sparse) matrix or a data.frame.")
  if(is.null(rownames(y)))
     stop("y must have rownames in order to be used in add_zero().")
  is_sparse_y <- inherits(y, c("Matrix", "sparseVector"))
  is_numeric_y <- is.numeric(if(!is_sparse_y) y else y@x)
  if(!is_numeric_y) stop("y is not numeric (or not entirely).")
  
  # Prepare rownames argument
  rownames <- rownames[!is.na(rownames)]
  rownames <- as.character(rownames)
  
  # Expand y with 0 in order to get an object whose rownames 
  # are the character argument rownames (in the same order)
  compl <- setdiff(rownames, rownames(y))
  if(!is_sparse_y){
    r <- rbind(y, matrix(0, nrow = length(compl), ncol = NCOL(y), dimnames = list(compl)))
    if(is_data.frame_y) r <- as.data.frame(r)
  }else{
    r <- rbind(y, Matrix(0, nrow = length(compl), ncol = NCOL(y), dimnames = list(compl, NULL)))
    r <- methods::as(r, class_y)
  }

  # Remove rows that do not match any element in rownames 
  # if remove is TRUE
  if(remove){
    if(length(setdiff(rownames(y), rownames)))
      warn("The name of some rows in y do not match any element in the rownames argument. These rows are removed from the result (use remove = FALSE to change this behaviour).")
    o <- rownames    
  }else o <- order(rownames(r))
  
  r[o, , drop = FALSE]
  
}


# TODO: Export and document make_block()
make_block <- function(y, by){
  
  # Step 1: Prepare the by argument
  by <- droplevels(as.factor(by))
  H <- length(levels(by))
  if(H == 1) return(y)
  
  # Step 2: Coerce y to a TsparseMatrix and remove NA values
  res <- coerce_to_TsparseMatrix(y)
  if(any(is.na(by))){
    na <- is.na(res@j)
    res@x <- res@x[!na]
    res@i <- res@i[!na]
    res@j <- res@j[!na]
  }
  
  # Step 3: Adjust the y and Dim slots in order to obtain the block matrix
  p <- NCOL(res)
  res@Dimnames[2] <- list(NULL)
  res@j <- as.integer(((as.numeric(by) - 1) * p)[res@i + 1] + res@j)
  res@Dim <- c(res@Dim[1], as.integer(res@Dim[2] * H))
  
  # Step 4: Export the result with relevant attributes
  attr(res, "rowby") <- as.character(by)
  attr(res, "colby") <- as.character(rep(levels(by), each = p))
  res
  
}


# Unexported (and undocumented) functions

# From devtools (https://github.com/r-lib/devtools/blob/master/R/utils.r)
"%||%" <- function(a, b) if (!is.null(a)) a else b

coerce_to_TsparseMatrix <- function(y){
  if(is.null(dim(y))){
    names_y <- names(y)
    res <- Matrix::sparseMatrix(
      x = unname(y), i = seq_along(y), j = rep(1, length(y)), giveCsparse = FALSE
    )
    if(!is.null(names_y)) rownames(res) <- names_y
  }else if(!methods::is(y,"TsparseMatrix")){
    dimnames_y <- dimnames(y)
    res <- methods::as(y, "TsparseMatrix")
    if(!is.null(dimnames_y)) dimnames(res) <- dimnames_y
  }else res <- y
  res
}

detect_block <- function(y, by){
  by <- droplevels(as.factor(by))
  y_bool <- coerce_to_TsparseMatrix(y) != 0
  by_bool <- make_block(rep(TRUE, NROW(y)), by)
  prod <- crossprod(by_bool, y_bool)
  prod_bool <- prod > 0
  if(!all(colSums(prod_bool) <= 1)) return(NULL)
  attr(y, "rowby") <- as.character(by)
  attr(y, "colby") <- rep(levels(by), NCOL(prod_bool))[as.vector(prod_bool)]
  y
}


change_enclosing <- function(FUN, envir = environment(FUN)){
  eval(parse(text = deparse(FUN)), envir = envir)
}

assign_all <- function(objects, to, from = parent.frame(), not_closure = c(list(globalenv()), sys.frames())){
  for(n in objects){
    get_n <- get(n, from)
    if(!is.function(get_n)){
      assign(n, get_n, envir = to)
    }else{
      tmp <- new.env(parent = to)
      env_n <- environment(get_n)
      not_closure <- c(not_closure, from)
      is_closure <- !any(sapply(not_closure, identical, env_n))
      if(is_closure)
        assign_all(ls(env_n, all.names = TRUE), to = tmp, from = env_n, not_closure = not_closure)
      assign(n, change_enclosing(get_n, envir = tmp), envir = to)
    }
  }
}

is_error <- function(expr) 
  inherits(try(expr, silent = TRUE), "try-error")

replace_variable_name_with_symbol <- function(arg_list, envir, single = TRUE){
  tmp <- lapply(arg_list, function(a){
    if(is_error(a_eval <- eval(a, envir = envir))){
      a_out <- list(a)
    }else if(is_variable_name(a_eval, Inf)){
      if(single && !is_variable_name(a_eval, 1)) 
        stop("Only single variable names are allowed for the by argument.")
      a_out <- lapply(a_eval, as.symbol)
    }else a_out <- list(a)
    a_out
  })
  if(!single){
    tmp_length <- sapply(tmp, length)
    if(!all(tmp_length %in% c(1, max(tmp_length))))
      stop("Some arguments have longer variable vectors than others.")
    tmp[tmp_length == 1] <- 
      lapply(tmp[tmp_length == 1], `[`, rep(1, max(tmp_length)))
  }else if(length(tmp) == 1) tmp[1] <- tmp[[1]]
  tmp
}

warn <- function(...) warning(..., "\n", call. = FALSE, immediate. = TRUE)
note <- function(...) message("Note: ", ..., "\n")

is_statistic_wrapper <- function(x) inherits(x, "gustave_statistic_wrapper")

names_else_NA <- function(x){
  if(is.null(names(x))) rep(NA, length(x)) else{
    tmp <- names(x)
    tmp[tmp %in% ""] <- NA
    tmp
  }
}

discretize_qualitative_var <- function(var, logical  = FALSE){
  var <- droplevels(as.factor(var))
  result <- Matrix(nrow = length(var), ncol = length(levels(var)))
  result[!is.na(var), ] <- Matrix::sparse.model.matrix(~ var - 1)
  result[is.na(var), ] <- NA
  if(!logical) result <- result * 1
  rownames(result) <- names(var)
  colnames(result) <- levels(var)
  result
}

get_through_parent_frame <- function(x){
  n <- 0
  found <- NULL
  while(is.null(found) || identical(baseenv(), parent.frame(n))){
    n <- n + 1
    found <- get0("execution_envir", parent.frame(n))
  }
  found
}

is_variable_name <- function(param, max_length = 1)
  is.character(param) && length(param) > 0 && length(param) <= max_length

variable_not_in_data <- function(var, data){
  if(is.null(var)) return(NULL)
  tmp <- var[!(var %in% names(data))]
  if(length(tmp) == 0) return(NULL)
  tmp
} 

display_only_n_first <- function(x, 
                                 n = 10, 
                                 collapse = ", ", 
                                 final_text = paste0(" and ", length(x) - n, " more")
){
  if(length(x) <= n){
    paste(x, collapse = collapse)
  }else{
    paste0(paste(x[1:n], collapse = collapse), final_text)
  }
}

rbind_output_df <- function(list_output_df){
  names <- unique(do.call(base::c, lapply(list_output_df, names)))
  output_df <- do.call(rbind, lapply(list_output_df, function(i){
    i[, setdiff(names, names(i))] <- NA
    i[, names]
  }))
  output_df <- output_df[, sapply(output_df, function(i) !all(is.na(i)))]
  rownames(output_df) <- NULL
  output_df
}
