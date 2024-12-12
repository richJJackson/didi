plot.didi <- function(x, ...){

  ###
  zmat <-(x$xMatrix)
  z3d <- melt(zmat)
  table(z3d$Var1)
  z3d$time <- x$time[z3d$Var1]
  z3d$cov <- x$cov[z3d$Var2]

  ggplot(z3d, aes(time, cov, z=value)) +
    v <- stat_contour_filled(geom="contour_filled")
  v

}
