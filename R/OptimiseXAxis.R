OptimiseXAxis <- function(data, ypart, x) {
  xpart <- GetClumpsPartition(data = data, ypart = ypart)
  k <- max(xpart)
  P <- array(NA, c(k, x, length(xpart)))
  I <- matrix(NA, k, x)
  
  for(t in 2:k) {
    maxs <- rep(0, length(xpart))
    maxH <- -Inf
    for(s in 1:t) {
      part <- Repartition(xpart, 0, s, t)
      H <- ShannonEntropy(part) - ShannonEntropy(part, ypart)
      if(H > maxH) {
        maxs <- part
        maxH <- H
      }
    }
    P[t, 2, ] <- maxs
    I[t, 2] <- ShannonEntropy(ypart) + ShannonEntropy(maxs) - ShannonEntropy(maxs, ypart)
  }
  
  if(x > 2)
    for(l in 3:x) {
      if(l <= k)
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
        P[t, l, ] <- Unionpartition(P[maxs, l-1, ], xpart, t)
        I[t, l] <- ShannonEntropy(ypart) + ShannonEntropy(P[t, l, ]) - ShannonEntropy(P[t, l, ], ypart)
      }
    }
  
  if(k < x)
    for(l in (k+1):x) {
      P[k, l, ] <- P[k, k, ]
      I[k, l] <- I[k, k]
    }
  
  return(I[k, ])
}