plot.didi <- function(x, y=NULL, ...){

  #### Plotting
  zmat <-(x$xMatrix)
  n <- length(x$cov)
  time <- x$time
  cov <- x$cov+1e-6
  filled.contour(time,cov,zmat)

}
