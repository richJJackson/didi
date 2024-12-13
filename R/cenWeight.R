#' Function to estimate Efforn weights
#'
#' This function will calculate a set of weights to be used in the kernel
#' density estimation of a hazard function
#'
#' @param x an identifier specifying which observations are censorted
#' @param n an interger specifying the total number of observations
#' @param dist an optional covairate of lenth n specifiyin and additional
#' distance, defualts to 'NULL' and all observations are given a nominal
#' distance of 1
#' @details This performs the procedure which redistributes the weights of
#' censored observations to all observations remaining at risk.  Where dist is
#' not spefied this will provide a set of weights which will produce an
#' esitmator close to the Kaplan Meier estimator.  Where a distance is applied a
#' set of weights is produced which can adjust the density estiamte.
#' @returns A set of weights for use in the kernel hazard estimation
cenWeight <- function(x,n,dist=NULL){

  addW <- rep(1,n)
  if(!is.null(dist)) addW <- dist

  for(i in 1:length(x)){
    aw <- addW[x[i]]/(n-x[i]-1)
    addW[((x[i]+1):n)] <- addW[((x[i]+1):n)] + aw
  }

  addW <- addW[1:n]

  if(is.na(addW[length(addW)])|addW[length(addW)]=="Inf") {
    addW[length(addW)] <- max(addW[-length(addW)])
  }
  addW
}
