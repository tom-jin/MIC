GetClumpsPartition <- function(data, ypart) {
  n <- nrow(data)
  i <- 1
  c <- -1
 
  xpart <- rep(0, n)
  
  while(i < n) {
    s <- 0
    flag <- FALSE
    for(j in (i+1):n)
      if (data[i, 1] == data[j, 1]) {
        s <- s + 1
        if(ypart[i] != ypart[j])
          flag <- TRUE
      }
    
    if (s > 0 && flag)
      for(j in 0:s) {
        ypart[i+j] <- c
        c <- c - 1
      }
    
    i <- i + s + 1
  }
  
  i <- 1
  xpart[1] <- i
  
  for(j in 2:n) {
    if(ypart[j] != ypart[j-1])
      i <- i + 1
    xpart[j] <- i
  }
  
  return(xpart)
}
