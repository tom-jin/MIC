EquipartitionYAxis <- function(data, y) {
  n <- nrow(data)
  dataByX <- cbind(data, 1:n)
  perm <- order(data[,2])
  dataByY <- dataByX[perm, ]

  i <- 1
  row <- 1
  rowSize <- n/y
  
  part <- rep(0, n)
  
  while(i <= n) {
    same <- dataByY[, 2] == dataByY[i, 2]
    count <- sum(part == row, na.rm = TRUE)
    
    if(count != 0 && abs(count + sum(same, na.rm = TRUE) - rowSize) >= abs(count - rowSize)) {
      row <- row + 1
      rowSize <- (n - i + 1)/(y - row + 1)
    }
    
    part[dataByY[same, 3]] <- row
    i  <- i + sum(same)
  }
  
  if(row < y) {
    warning("Partition only has ", row, " groups.")
  }
  
  return(part)
}
