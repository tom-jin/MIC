ShannonEntropy <- function(ypart, xpart = NA) {
  total <- 0
  
  if(is.na(xpart)[1]) {
    for(i in 1:max(ypart)) {
      prob <- sum(i == ypart)/length(ypart)
      if(prob > 0) 
        total <- total - prob * log(prob, 2)
    }
  } else {
    if(length(ypart) != length(xpart))
      stop("xpart must be the same length as ypart or NA")
    for(j in 1:max(xpart)) {
      for(i in 1:max(ypart)) {
        prob <- sum(i == ypart[j == xpart])/length(ypart)
        if(prob > 0) 
          total <- total - prob * log(prob, 2)
      }
    }
  }

  return(total)
}