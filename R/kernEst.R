kernEst <- function(x,y,bw){

  mat <- lapply(x,function(x) dnorm(y,x,bw))
  mat <- unlist(mat)
  mat <- matrix(mat,length(y),length(x))
  mat
}
