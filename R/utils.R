#' TODO
#' @export
Diagby <- function(y = NULL, by, p = 1){
  # y <- as(Matrix(TRUE, ncol = 10, nrow = length(rowby)), "TsparseMatrix"); by <- rowby; p <- 2
  # y <- x; by <- strata
  byrow <- by
  by <- as.factor(by)
  H <- length(levels(by))
  if(is.null(y)){
    t <- lapply(1:H, function(i) which(as.integer(by) == i))
    y <- Matrix::sparseMatrix(
      i = do.call(base::c, lapply(t, rep, p))
      , j = do.call(base::c, lapply(1:(p * H), function(k) rep(k, length(t[[ceiling(k/p)]]))))
      , x = TRUE, giveCsparse = FALSE, check = FALSE
    )
    y@Dim[1] <- length(by)
  }else{
    p <- NCOL(y)
    if(!methods::is(y,"TsparseMatrix")) y <- methods::as(if(p == 1) as.matrix(y) else y, "TsparseMatrix")
    y@j <- as.integer(((as.numeric(by) - 1) * p)[y@i + 1] + y@j)
    y@Dim <- c(y@Dim[1], as.integer(y@Dim[2] * H))
    if(any(is.na(by))){na <- is.na(y@j); y@x <- y@x[!na]; y@i <- y@i[!na]; y@j <- y@j[!na]}
  }
  bycol <- rep(levels(by), each = p)
  bycol <- if(is.factor(byrow)) as.factor(bycol) else methods::as(bycol, class(byrow))
  return(list(y = y, byrow = byrow, bycol = bycol))
}
# rowby <- as.factor(c(rep(1, 5), NA, rep(2, 8), rep(NA, 4)))
# colby <- c(rep(1, 2), rep(2, 2))
# all.equal(Diagby(by = rowby, p = 10), Diagby(y = Matrix(TRUE, ncol = 10, nrow = length(rowby)), by = rowby))


#' Efficient sum by group using Matrix
#' @export

sumby <- function(y, by = NULL, w = NULL, dense = FALSE){

  n <- NROW(y)
  if(!is.null(w)) y <- y * w
  nullBy <- is.null(by)
  if(nullBy) by <- rep(1,n)
  if(anyNA(by)){
    byNA <- is.na(by)
    y <- y[!byNA, , drop = FALSE]
    by <- by[!byNA]
    n <- NROW(y)
  }

  by <- as.factor(by)
  x <- Diagby(rep(TRUE, n), by)$y
  r <- t(x) %*% y

  if(!grepl(pattern = "matrix", x = class(y), ignore.case = TRUE)){
    r <- as.vector(r)
    if(!nullBy) names(r) <- levels(by)
  }else{
    if(is.matrix(y) || dense) r <- as.matrix(r)
    if(!nullBy) rownames(r) <- levels(by)
  }

  return(r)

}

# set.seed(1); n <- 20000; p <- 10; H <- 600; y <- as(Matrix(rnorm(p*n),ncol=p),"TsparseMatrix"); by <- rep(1:H,n %/% H + 1)[1:n][sample.int(n)]; by <- as.factor(by); w <- rnorm(n)
# library(microbenchmark)
# microbenchmark(r <- sumby(y,by), times = 10)
#
#   n <- 4500000; y <- as.matrix(rnorm(n)); by <- sample(1:20000,n,replace = TRUE)
#   library(microbenchmark)
#   microbenchmark(
#     sumby(y,by)
#     , data.table(as.matrix(y))[,j=list(blabla=sum(V1)),by=by]
#     , aggregate(as.matrix(y),list(by),sum)
#     , times = 10L
#   )


#' Expand a matrix with zeros based on rownames matching
#' @export

add0 <- function(y, rownames){
  compl <- setdiff(rownames, rownames(y))
  compl <- matrix(
    0, nrow = length(compl), ncol = NCOL(y), dimnames = list(compl)
  )
  rbind(y, compl)[rownames, , drop = FALSE]
}


#' change_enclosing
#' @export

change_enclosing <- function(fun, envir = environment(fun)){
  eval(parse(text = deparse(fun)), envir = envir)
}


#' assign_all
#' @export

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

# assign_all <- function(objects, to, from = parent.frame(), not_closure = c(list(globalenv()), sys.frames())){
#   for(n in objects){
#     env_n <- environment(get_n <- get(n, where_n <- pryr::where(n, from)))
#     if(!is.function(get_n)){
#       assign(n, get_n, envir = to)
#     }else{
#       tmp <- new.env(parent = to)
#       not_closure <- c(not_closure, where_n)
#       is_closure <- !any(sapply(not_closure, identical, env_n))
#       if(is_closure)
#         assign_all(ls(env_n, all.names = TRUE), to = tmp, from = env_n, not_closure = not_closure)
#       assign(n, change_enclosing(get_n, envir = tmp), envir = to)
#     }
#   }
# }
