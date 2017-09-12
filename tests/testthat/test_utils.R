

context("assign_all")

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


#
# context("Sanitize - Function defined in globalenv()")
#
# varwrap_factory_test <- function(objects_to_include, envir = parent.frame()){
#   varfun <- function(){}
#   varwrap <- function(){}
#
#   varwrap <- include_objects(varwrap, objects = c("varfun", objects_to_include), from = envir)
#   return(varwrap)
# }
#
# a <- 1
# fun <- function(){}
# clos <- function(){
#   b <- 2
#   function(){}
# }


#
# test_that("varwrap_test can be defined", {
#   expect_error(
#     varwrap_test <<- varwrap_factory_test(objects_to_include = c("a", "fun", "clos"))
#   , regexp = NA)
# })
#
# test_that("varwrap_test has globalenv() as parent.env()", {
#   expect_equal(parent.env(environment(varwrap_test)), globalenv())
# })
#
# test_that("varwrap_test contains what it should", {
#   expect_true(all(c("a", "fun", "clos","varfun") %in%ls(environment(varwrap_test))))
# })
#
#
# context("Sanitize - Function defined in another function")
#
# rm(list = c("a", "fun", "clos"))
# prepare_test <- function(){
#   a <- 1
#   fun <- function(){}
#   clos <- function(){
#     b <- 2
#     function(){}
#   }
#   varwrap_factory_test(objects = c("a", "fun", "clos"))
# }
#
# test_that("varwrap_test can be defined", {
#   expect_error(
#     varwrap_test <<- prepare_test()
#   , regexp = NA)
# })
#
# test_that("varwrap_test has globalenv() as parent.env()", {
#   expect_equal(parent.env(environment(varwrap_test)), globalenv())
# })
#
# test_that("varwrap_test contains what it should", {
#   expect_true(all(c("a", "fun", "clos","varfun") %in%ls(environment(varwrap_test))))
# })




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
