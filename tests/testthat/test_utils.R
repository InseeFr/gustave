

context("utils")


# sumby()

set.seed(1)
n <- 10
v <- as.numeric(1:n)
v[6] <- NA
V <- sparseVector(v, seq_along(v), n)
m <- matrix(c(v, v), ncol = 2)
colnames(m) <- c("variable1", "variable2")
M <- Matrix(m)
df <- as.data.frame(m)
by <- sample(letters[1:3], 10, replace = TRUE)
w <- rep(2, n)
by_NA <- by
by_NA[c(2, 8)] <- NA

test_that("sumby() works", {
  
  # Standard behaviour
  r <- sapply(split(v, by), sum, na.rm = TRUE)
  expect_equal(sumby(v, by), r)
  expect_equal(sumby(V, by), r)
  expect_equal(sumby(m, by), matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(M, by), matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(M, by, keep_sparse = TRUE), Matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(df, by), data.frame(variable1 = r, variable2 = r, row.names = names(r)))
  expect_equal(sumby(v, by, w), r * 2)
  
  # Standard behaviour without removing the NA values
  r <- sapply(split(v, by), sum)
  expect_equal(sumby(v, by, na_rm = FALSE), r)
  expect_equal(sumby(V, by, na_rm = FALSE), r)
  expect_equal(sumby(m, by, na_rm = FALSE), matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(M, by, na_rm = FALSE), matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(M, by, na_rm = FALSE, keep_sparse = TRUE), Matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(df, by, na_rm = FALSE), data.frame(variable1 = r, variable2 = r, row.names = names(r)))
  expect_equal(sumby(v, by, w, na_rm = FALSE), r * 2)
  
  # Standard behaviour with NA value in the by variable
  r <- sapply(split(v, by_NA), sum, na.rm = TRUE)
  expect_equal(sumby(v, by_NA), r)
  expect_equal(sumby(V, by_NA), r)
  expect_equal(sumby(m, by_NA), matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(M, by_NA), matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(M, by_NA, keep_sparse = TRUE), Matrix(c(r, r), ncol = 2, dimnames = list(names(r), colnames(m))))
  expect_equal(sumby(df, by_NA), data.frame(variable1 = r, variable2 = r, row.names = names(r)))
  expect_equal(sumby(v, by_NA, w), r * 2)
  
  # Error messages
  expect_error(sumby(letters[seq_along(by)], by), regexp = "y is not")

})


# add0()

set.seed(1)
n <- 10
p <- 2
m <- matrix(1:(n*p), ncol = p, dimnames = list(sample(letters, n)))
m[c(3, 8, 12)] <- NA
M <- as(m, "TsparseMatrix")
rownames(M) <- rownames(m)
df <- as.data.frame(m)

test_that("add0() works", {
  
  # Standard behaviour
  expect_error(add0(m, letters), regexp = NA)
  expect_error(add0(M, letters), regexp = NA)
  expect_error(add0(df, letters), regexp = NA)
  expect_error(add0(m, as.factor(letters)), regexp = NA)
  
  # Error and warning messages
  expect_error(add0(m[, 1], letters), regexp = "y must be")
  expect_error(add0(unname(m), letters), regexp = "y must have")
  expect_error(add0(matrix(letters[m], ncol = 2, dimnames = list(rownames(m))), letters), regexp = "y is not numeric")
  tmp <- m
  rownames(tmp)[1:3] <- toupper(rownames(tmp)[1:3])
  expect_warning(add0(tmp, letters), regexp = "The name of some rows")

})


# assign_all()

a <- 1
fun <- function(){}
clos <- (function(){
  b <- 2
  function(){}
})()
e <- new.env(parent = globalenv())
assign_all(c("a", "fun", "clos"), to = e)

test_that("assign_all() works", {
  expect_true(
    all(sapply(e, function(x) if(is.function(x)) identical(parent.env(environment(x)), e) else TRUE))
  )
  expect_equal(ls(environment(e$clos)), "b")
})
