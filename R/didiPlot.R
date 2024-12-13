#' Function to plot the Didi estimator.
#'
#' This function to plot the 'Didi' estimator.  This isuses the 'filled.contour'
#' opint in base graphics and is now deprecited hving been replaced by
#' plot.didi() which makes use of ggplot2.
#"
#' @param x an object of class "didi"
#' @details This takes the inputs of the "didi" object and plots them using a
#' filled contour plot.
#' @returns A fileld contour plot for the didi estimator
#' @import graphics
#' @export
didiPlot <- function(x){

  #### Plotting
  zmat <-(x$xMatrix)
  n <- length(x$cov)
  time <- x$time
  cov <- x$cov+1e-6
  filled.contour(time,cov,zmat)

  }


