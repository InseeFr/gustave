

context("utils")

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
