didiEst <- function(st,cen,x,bw=NULL,bw.x=NULL,type="survival",size=100){

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
    #xdis <- xdis*(1/xdis[i])

    wcen <- cenWeight(cen.id,nn,dist=xdis);wcen

    CE <- wcen[-cen.id]
    wmat <- t(matrix(CE,length(ST),size))
    scl <- sum(CE)/nn

    ### Getting Kernal Estimates
    fhat <- rowSums(mat*wmat)

    ### Hazard estimation
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
