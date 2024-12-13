#' Function to perform kernel estimation using normally distributed kernels.
#'
#' This function to produce the 'Didi' estimator.  A double weighted kernel
#' estimator of the survival function in the presence of a continuous covariate.
#"
#' @param x an object of class "didi"
#' @param ylab a text input fo the y axis
#' @param xlab a text input fo the x axis
#' @param ... not used
#' @details This takes the inputs of the "didi" object and plots them using a
#' filled contour plot.  This makes use of the stat_contour_filled option within
#' ggplot 2
#' @returns A ggplot object giving the filled contour plot
#' @import ggplot2
#' @export
plot.didi <- function(x,ylab="cov",xlab="time", ...){

  # binding global variables
  time <- cov <- value <- NULL

  ### reshaping matrix
  zmat <-(x$xMatrix)
  z3d <- melt(zmat)
  table(z3d$Var1)
  z3d$time <- x$time[z3d$Var1]
  z3d$cov <- x$cov[z3d$Var2]

  v <- ggplot(z3d, aes(time, cov, z=value)) +
    stat_contour_filled(geom="contour_filled")+
    ylab(ylab)+
    xlab(xlab)
  v

}
