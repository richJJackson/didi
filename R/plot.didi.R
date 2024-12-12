plot.didi <- function(x,ylab="cov",xlab="time",xlim=NULL, ...){

  ###
  zmat <-(x$xMatrix)
  z3d <- melt(zmat)
  table(z3d$Var1)
  z3d$time <- x$time[z3d$Var1]
  z3d$cov <- x$cov[z3d$Var2]

  if(is.null(xlim)) xlim=c(0,max(z3d$time,na.rm=T))

  v <- ggplot(z3d, aes(time, cov, z=value)) +
    stat_contour_filled(geom="contour_filled")+
    ylab(ylab)+
    xlab(xlab)+
    xlim(xlim)
  v

}
