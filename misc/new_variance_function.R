
#' @references Wolter, 2008, p. 53

varCollapse <- function(y, group, strata = NULL){
  
  if(is.null(strata)) Ygh <- y else{
    Ygh <- sumby(y, by = strata)
    m <- match(row.names(Ygh), strata)
    group <- group[m]; strata <- strata[m]
  }
  
  n <- NROW(Ygh)
  t <- sumby(cbind(matrix(1,nrow = n),Ygh),by=group)
  m <- match(group,row.names(t))
  Lg <- t[m,1]
  Yg <- t[m,-1,drop = FALSE]
  
  r <- colSums((Lg/(Lg-1))*(Ygh-Yg/Lg)^2)
  
  return(r)
  
}


# Tests
# set.seed(1)
# n <- 20000
# p <- 10
# y <- matrix(rnorm(p*n),ncol=p)
# pik <- rep(1,n)
# strata <- sample(apply(expand.grid(letters,letters,letters),1,paste0,collapse = ""),n,replace = TRUE)
# # strata <- sample.int(26^2,n,replace = TRUE)
# w <- rbinom(n,500,0.5)
# valid <- !duplicated(strata)
# source("X:/HAB-Bases-UMS/MC/projets/#commun/R/collapse.R")
# group <- collapse(strata,w,valid)
#
# microbenchmark(
#   varC(y/w,group,strata = strata)
#   , varCollapse(y/w,group,strata = strata)
#   , times = 10
# )

#' @references Wolter, 2007, pp. 298-353, v12

varsys <- function(y, pik){
  n <- nrow(as.matrix(y))
  return(
    ( (n - sum(pik)) / ( 2 * (n - 1)) ) *
      colSums(
        as.matrix(as.matrix(y)[-n,]/pik[-n] -
                    as.matrix(y)[-1,]/pik[-1])^2
        ,na.rm = T)
  )
}

varsysst <- function(y, pik, strata=rep(1,nrow(as.matrix(y)))){
  
  o <- order(strata);
  pik <- pik[o]; strata <- strata[o]
  p <- ncol(as.matrix(y))
  y <- matrix(matrix(y,ncol=p)[o,],ncol=p)
  
  id <- cumsum(!duplicated(strata))
  H <- max(id)
  f <- !duplicated(strata)
  l <- rev(!duplicated(rev(strata)))
  
  n <- c(which(f)[-1],length(strata) + 1) - which(f)
  sumpik <- (cumsum(pik) - c(0,cumsum(pik)[which(l)][-H])[id])[l]
  
  return(
    colSums(
      (n[id[-which(f)]] - sumpik[id[-which(f)]])/(2 * (n[id[-which(f)]] - 1)) *
        ( matrix(matrix(matrix(y,ncol=p)[-which(l),],ncol=p)/pik[-which(l)],ncol=p) -
            matrix(matrix(matrix(y,ncol=p)[-which(f),],ncol=p)/pik[-which(f)],ncol=p))^2
    )
  )
}


# y <- as.matrix(Y[,paste0("y",1:10),with = F]);strata <- Y$idzae;pik <- Y$piLog
# strata <- rep(1,nrow(Y))
#