didiPlot <- function(st,cen,x){

  ### estimating function
  s_Est <- didiEst(st,cen,x,type="survival")

  #### Plotting
  zmat <-(s_Est$xMatrix)
  n <- length(s_Est$cov)
  time <- s_Est$time
  cov <- s_Est$cov+1e-6
  filled.contour(time,cov,zmat)

  }


