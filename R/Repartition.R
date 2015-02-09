Repartition <- function(part, a, b, c = NA) {
 repart <- rep(0, length(part))
 
 repart[a < part & part <= b] <- 1
 if(!is.na(c))
   repart[b < part & part <= c] <- 2
 
 return(repart)
}