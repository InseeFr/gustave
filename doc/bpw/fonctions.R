.init <- function(){
  rm(list = ls())
  set.seed(1)
  library(microbenchmark)
  options(repos = "https://cran.rstudio.com/")
  options(width = 55)
  knitr::opts_chunk$set(comment="  ##", collapse=TRUE, tidy = FALSE)
  .initOK <<- TRUE 
}
.init()