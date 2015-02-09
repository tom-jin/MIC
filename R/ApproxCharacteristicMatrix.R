ApproxCharacteristicMatrix <- function(data, boundary) {
  reversedata <- data[, 2:1]
  M <- matrix(NA, boundary, boundary)
  I <- matrix(NA, boundary, boundary)
  J <- matrix(NA, boundary, boundary)
  
  for(y in 2:floor(boundary/2)) {
    x <- floor(boundary/y)
    I[, y] <- ApproxMaxMI(data, x, y)
    J[, y] <- ApproxMaxMI(reversedata, x, y)
  }
  
  for(x in 1:boundary) {
    for(y in 1:boundary) {
      if(x*y > boundary)
        break
      
      I[x, y] <- max(I[x, y], J[y, x], na.rm = TRUE)
      M[x, y] <- I[x, y] / min(log(x), log(y), na.rm = TRUE)
    }
  }
  
  return(M)
}