

#' Efficient sum by group using the Matrix package
#' 
#' @description 
#' 
#' @export

sumby <- function(y, by, w = NULL){

  isVector <- is.null(dim(y))
  n <- NROW(y)
  if(!is.null(w)) y <- y * w
  if(anyNA(by)){
    byNA <- is.na(by)
    y <- y[!byNA, , drop = FALSE]
    by <- by[!byNA]
    n <- NROW(y)
  }
  by <- as.factor(by)
  x <- block_matrix(rep(TRUE, n), by)$y
  r <- Matrix::t(x) %*% y

  if(isVector){
    r <- as.vector(r)
    names(r) <- levels(by)
  }else{
    if(is.matrix(y)) r <- as.matrix(r)
    rownames(r) <- levels(by)
  }

  return(r)

}


#' Expand a matrix with zeros based on rownames matching
#' @export

add0 <- function(y, rownames){
  compl <- setdiff(rownames, rownames(y))
  compl <- matrix(
    0, nrow = length(compl), ncol = NCOL(y), dimnames = list(compl)
  )
  rbind(y, compl)[rownames, , drop = FALSE]
}




# Unexported (and undocumented) functions

block_matrix <- function(y, by){
  # y <- as(Matrix(TRUE, ncol = 10, nrow = length(rowby)), "TsparseMatrix"); by <- rowby; p <- 2
  # y <- x; by <- strata
  byrow <- by
  by <- as.factor(by)
  H <- length(levels(by))
  p <- NCOL(y)
  if(!methods::is(y,"TsparseMatrix")) y <- methods::as(if(p == 1) as.matrix(y) else y, "TsparseMatrix")
  y@j <- as.integer(((as.numeric(by) - 1) * p)[y@i + 1] + y@j)
  y@Dim <- c(y@Dim[1], as.integer(y@Dim[2] * H))
  if(any(is.na(by))){na <- is.na(y@j); y@x <- y@x[!na]; y@i <- y@i[!na]; y@j <- y@j[!na]}
  bycol <- rep(levels(by), each = p)
  bycol <- if(is.factor(byrow)) as.factor(bycol) else methods::as(bycol, class(byrow))
  return(list(y = y, byrow = byrow, bycol = bycol))
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