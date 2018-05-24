#' Variance function for one-stage stratified simple random sampling
#' with non-response and calibration
#' 
#' @export
#' 
var_strata <- function(y = NULL, 
                       strata,
                       w_sample,
                       w_nr = NULL,
                       x_calib = NULL,
                       w_calib = NULL,
                       precalc = NULL
){
  
  # y <- setNames(ict_survey$speed_quanti, ict_survey$firm_id); x_calib <- as.matrix(ict_survey[, c(paste0("N_", 58:63), paste0("turnover_", 58:63))]); rownames(x_calib) <- ict_survey$firm_id; w_calib <- setNames(ict_survey$w_calib, ict_survey$firm_id)
  
  if(is.null(precalc)){
    
    # Consistency tests
    y <- as_matrix(y)
    w_ref <- if(!is.null(w_calib)) w_calib else if(!is.null(w_nr)) w_nr else w_sample
    if(!setequal(rownames(y), names(w_ref))) stop()
    
    precalc_calib <- rescal(y = NULL, x = x_calib, w = w_calib)
    
    
  }else list2env(precalc)
  
  if(is.null(y)){
    return(list())
  }else{
    
    # Calibration    
    y <- rescal(y, precalc = precalc_calib)

  }

}














#' TODO
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

#' TODO
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


#' TODO

varB <- function(y, pik){
  return(colSums((1 - pik) * (as.matrix(y)/pik)^2))
}
