Unionpartition <- function(inputpart, clumppart, clumpindex) {
  n <- length(inputpart)
  outputpart <- rep(NA, n)
  outputpart <- inputpart
  
  dataindex <- min(which(clumppart == clumpindex))
  if(inputpart[dataindex]>0){
    outputpart[dataindex:n] <- inputpart[dataindex:n] + 1
  }
  else {
    partend <- min(which(inputpart == 0))
    newend <- max(which(clumppart == clumpindex))
    outputpart[partend:newend] <- max(inputpart) + 1
  }
  
  return(outputpart)
}


