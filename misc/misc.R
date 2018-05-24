
#' Convert a vector to a one-column matrix keeping
#' the sparse Matrix structure (if any) and transforming
#' names into rownames().
as_matrix <- function(y, deparse_name = TRUE){
  # y <- setNames(1:10, letters[1:10])
  if(!is.null(dim(y))) return(y)
  is_sparse_y <- isTRUE(attr(class(y), "package") == "Matrix")
  r <- if(is_sparse_y) methods::as(y, "sparseMatrix") else as.matrix(y)
  rownames(r) <- names(y)
  if(deparse_name) colnames(r) <- deparse(substitute(y))
  r
}
test <- as(setNames(1:10, letters[1:10]), "sparseVector")
as_matrix(test)
