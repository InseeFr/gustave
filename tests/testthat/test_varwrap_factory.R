

context("varwrap_factory - Function and data defined in globalenv()")

source("data.R")

varfun_test <- function(y, eurostat = FALSE) list(var = if(!eurostat) abs(colSums(y)) else 0)

test_that("varwrap_test can be defined", {
  expect_error(
    varwrap_test <<- varwrap_factory(
      varfun = varfun_test, id_default = substitute(id3)
      , idref = ref$idref, wref = ref$wref
    )
    , regexp = NA)
})

# ls(environment(varwrap_test))
# varwrap_test(data = survey, total(quali))


test_that("varwrap_test works", {
  expect_error(
    varwrap_test(survey, quanti)
    , regexp = NA)
})

context("varwrap_factory - Function and data defined in another function")

prepare_test <- function(){

  source("data.R")
  a <- 1

  varfun_test <- function(y, eurostat = FALSE) list(var = abs(colSums(y)))

  varwrap_factory(
    varfun = varfun_test, id_default = substitute(id3)
    , idref = substitute(ref$idref), wref = ref$wref
    , objects_to_include = c("a", "ref")
  )
}

test_that("varwrap_test can be defined", {
  expect_error(
    varwrap_test <<- prepare_test()
    , regexp = NA)
})

test_that("varwrap_test works", {
  expect_error(
    varwrap_test(survey, quanti)
    , regexp = NA)
})


