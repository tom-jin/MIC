OptimiseXAxis <- function(data, ypart, x) {
  xpart <- GetClumpsPartition(data = data, ypart = ypart)
  k <- max(xpart)
  P <- array(NA, c(k, x, length(xpart)))
  I <- matrix(NA, k, x)
  
  for(t in 2:k) {
    maxs <- rep(0, length(xpart))
    maxH <- 0
    for(s in 1:t) {
      part <- Repartition(xpart, 0, s, t)
      H <- ShannonEntropy(part) - ShannonEntropy(part, ypart)
      if(H > maxH) {
        maxs <- part
        maxH <- H
      }
    }
    P[t, 1, ] <- maxs
    I[t, 1] <- ShannonEntropy(ypart) + ShannonEntropy(maxs) - ShannonEntropy(maxs, ypart)
  }
  
  for(l in 3:x) {
    for(t in l:k) {
      maxs <- 0
      maxf <- -Inf
      for(s in (l-1):t) {
        f <- (data[s, 1]/data[t, 1])*(I[s, l-1] - ShannonEntropy(ypart)) - 
          ((data[t, 1] - data[s, 1])/data[s, 1])*ShannonEntropy(Repartition(xpart, s, t), ypart)
        if(is.na(f)) browser()
        if(f > maxf) {
          maxs <- s
          maxf <- f
        }
      }
      P[t, l, ] <- Unionpartition(P[maxs, l-1, ], t)
      I[t, l] <- ShannonEntropy(ypart) + ShannonEntropy(P[t, l, ]) - ShannonEntropy(P[t, l, ], ypart)
    }
  }
  
  if(k < x)
    for(l in (k+1):x) {
      P[k, l-1, ] <- P[k, k-1, ]
      I[k, l-1] <- I[k, k]
    }
  
  return(I[k, ])
}