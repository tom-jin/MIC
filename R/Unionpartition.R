Unionpartition <- function(inputpart, index) {
  n <- length(inputpart)
  outputpart <- rep(NA, n)

  outputpart <- inputpart
  outputpart[(index+1):n] <- inputpart[(index+1):n] + 1
  
  return(outputpart)
}