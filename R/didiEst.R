#' Function to perform kernel estimation using normally distributed kernels.
#'
#' This function to produce the 'Didi' estimator.  A double weighted kernel
#' estimator of the survival function in the presence of a continuous covariate.
#'
#' @param st a vector of survival times
#' @param cen a vector of censoring indicators
#' @param x a vector contianing a continuous covariate
#' @param bw a bandwidth parameter for time
#' @param bw.x a bandwidth parameter for the covariate
#' @param size a parameter setting the number of points at which the kernals
#' should be evaluated.  Default to 100.
#' @details This performs the procedure which estimates the kernel density of a
#' set of data points, x at a set of evaluation points y.  The function creates
#' a grid with of equal points of length 'size' between 0 and the maximum
#' observed event time.  The kernel estimator is then evaluated at each unique
#' covariate value
#' @returns A object of class "didi" containing
#' * time: a vector specifying at which times the estimator was evaluated
#' * cov: a vector specifying at which covariate values the estimator was evaluated
#' * xMatrix: a matrix supplying the double weighted kernel estimators
#' @import reshape2
#' @export
didiEst <- function(st,cen,x,bw=NULL,bw.x=NULL,type="survival",size=100){

  ## Ensuring x is a continuous covariate
  x <- as.numeric(as.character(x))

  ## Creating requiredquantities
  nn <- length(st);nn

  ## Ordering
  ord <- order(st)
  st <- st[ord]
  cen <- cen[ord]
  x <- x[ord]
  cen.id <- which(cen==0)

  ## Getting censoring weights


  ### Setting estimation parameters
  ST <- st[-cen.id]
  CE <- NULL
  xdum <- seq(0,max(ST),length= size)

  mat <- kernEst(st,xdum,bw=bw)
  wcen <- cenWeight(cen.id,nn)
  mat <- mat[,-cen.id]


  ### Setting bw parameters
  if(is.null(bw)) bw <- sd(st,na.rm=T)/4
  if(is.null(bw.x)) bw.x <- sd(x,na.rm=T)/4


  ### Adding weight for censored ob
  scale <- (max(x)-min(x))^2

  #wmat <- t(matrix(CE,length(ST),1000))
  #nn <- length(st[-cen.id])

  xMatrix <- matrix(NA,size,nn)

  for(i in 1:length(x)){

    x0 <- x[i];x0
    x.dis <- 1-((x-x0)^2)*scale;x.dis

    xdis <- dnorm(x,x0,bw.x);
    xdis <- xdis*(nn/sum(xdis))

    wcen <- cenWeight(cen.id,nn,dist=xdis);wcen

    CE <- wcen[-cen.id]
    wmat <- t(matrix(CE,length(ST),size))
    scl <- sum(CE)/nn

    ### Getting Kernal Estimates
    fhat <- rowSums(mat*wmat)

    ### Hazard estimation
    Fhat <- cumsum(fhat)/sum(fhat,na.rm=T)*scl
    Shat <- 1 - Fhat
    hhat <- fhat/Shat

    xMatrix[,i] <- hhat
    if(type=="survival") {
      xMatrix[,i] <- Shat
      }


  }

  xMatrix <- xMatrix[,order(x)]

  minX <- min(xMatrix)
  if(minX<0) xMatrix[which(xMatrix<0,arr.ind=T)] <- 0

  ret<- list("time"=xdum,"cov"=x[order(x)],"xMatrix"=xMatrix)
  class(ret) <- "didi"
  return(ret)

}
