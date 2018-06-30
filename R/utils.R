

#' Efficient by-group (weighted) summation
#' 
#' @description \code{sumby()} performs an efficient and optionally weighted 
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
#' sumby(y, by)
#' 
#' # Keeping the NAs
#' sumby(y, by, na_rm = FALSE)
#' 
#' # With a weight
#' sumby(y, by, w = w)
#' 
#' @export
#' @import Matrix

sumby <- function(y, by, w = NULL, na_rm = TRUE, keep_sparse = FALSE){

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
  x <- block_matrix(w, by)$y
  colnames(x) <- levels(by)
  r <- crossprod(x, y)

  # Type of r
  if(!is_sparse_y | !keep_sparse){
    r <- if(is_vector_y) stats::setNames(as.vector(r), rownames(r)) else as.matrix(r)
  }else{
    if(is_vector_y) warning("sparseVector can't have names, hence the output won't have names.")
    r <- methods::as(r, class_y)
  }
  if(is_data.frame_y) r <- as.data.frame(r)
  
  r
  
}


#' Expand a matrix or a data.frame with zeros based on rownames matching
#'
#' @description For a given two-dimensional object with rownames and a character
#'   vector, \code{add0()} produces a corresponding object whose rownames match
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
#' add0(y, rownames)
#'
#' # Use when rownames in y do not match
#' # any element in the rownames argument
#' rownames(y)[1:3] <- toupper(rownames(y)[1:3])
#' add0(y, rownames)
#' add0(y, rownames, remove = FALSE)
#'
#' @import Matrix
#' @export
#' 
add0 <- function(y, rownames, remove = TRUE){
  
  # y <- m; rownames <- letters
  
  # Type of y
  class_y <- class(y)
  is_data.frame_y <- is.data.frame(y)
  if(is_data.frame_y) y <- as.matrix(y)
  if(is.null(dim(y)))
    stop("y must be a (sparse) matrix or a data.frame.")
  if(is.null(rownames(y)))
     stop("y must have rownames in order to be used in add0().")
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
      warning("The name of some rows in y do not match any element in the rownames argument. These rows are removed from the result (use remove = FALSE to change this behaviour).")
    o <- rownames    
  }else o <- order(rownames(r))
  
  r[o, , drop = FALSE]
  
}




# Unexported (and undocumented) functions

block_matrix <- function(y, by){
  # TODO: remanufacture block_matrix (do not change y when length(levels(by)) == 1) and change output
  # y <- as(Matrix(TRUE, ncol = 10, nrow = length(rowby)), "TsparseMatrix"); by <- rowby; p <- 2
  # y <- x; by <- strata
  byrow <- by
  by <- as.factor(by)
  H <- length(levels(by))
  if(H == 1) return(list(y = y, byrow = rep(levels(by), NROW(y)), bycol = rep(levels(by), NCOL(y))))
  p <- NCOL(y)
  if(!methods::is(y,"TsparseMatrix")) y <- methods::as(if(p == 1) as.matrix(y) else y, "TsparseMatrix")
  y@j <- as.integer(((as.numeric(by) - 1) * p)[y@i + 1] + y@j)
  y@Dim <- c(y@Dim[1], as.integer(y@Dim[2] * H))
  if(any(is.na(by))){na <- is.na(y@j); y@x <- y@x[!na]; y@i <- y@i[!na]; y@j <- y@j[!na]}
  bycol <- rep(levels(by), each = p)
  bycol <- if(is.factor(byrow)) as.factor(bycol) else methods::as(bycol, class(byrow))
  list(y = y, byrow = byrow, bycol = bycol)
}
# TODO: export a matrix with rowby/colby attributes instead of a list, 
# add an option for row/colnames creation (with a given separator)

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


coerce_to_Matrix <- function(y){
  if(is.null(dim(y))){
    names_y <- names(y)
    Matrix(y, ncol = 1, dimnames = list(names_y, NULL))
  }else if(!inherits(y, "Matrix")){
    methods::as(y, "sparseMatrix")
  }else y
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


# 
# add_names_to_list <- function(l){
#   if(!is.language(l)) stop("l must be an unevaluated evaluation (you may want to use substitute() in the function call.)")
#   l <- as.list(l)
#   names_l <- if(!is.null(names(l)[-1])) names(l) else rep(NA, length(l))
#   symbol_l <- sapply(unname(l), function(x) if(is.symbol(x)) deparse(x) else "")
#   names_l[names_l == ""] <- symbol_l[names_l == ""]
#   as.call(stats::setNames(l, names_l))
# }
# identical(
#   add_names_to_list(substitute(list(reference_id = fsndf, bsfsdf))),
#   substitute(list(reference_id = fsndf, bsfsdf = bsfsdf))
# )