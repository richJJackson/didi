#' Function to perform kernel estimation using normally distributed kernels.
#'
#' This function performs the kernel estimation procedure for a set of values x,
#' at specified points y with a bandwidth bw.  For use with the didiEst function.
#'
#' @param x vector of covariate values
#' @param y a optional vector specifying where the kernels should be evaluated
#' @param bw a bandwidth parameter specify the standard deviation of the kernels
#' @details This performs the procedure which estimates the kernel density of a
#' set of data points, x at a set of evaluation points y.
#' @returns A matrix of dimension x by y
kernEst <- function(x,y,bw){
  mat <- lapply(x,function(x) dnorm(y,x,bw))
  mat <- unlist(mat)
  mat <- matrix(mat,length(y),length(x))
  mat
}
