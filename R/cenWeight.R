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
