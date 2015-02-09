ApproxMaxMI <- function(data, x, y) {
  sorteddata <- data[order(data[, 1]), ]
  ypart <- EquipartitionYAxis(sorteddata, y)
  return(OptimiseXAxis(sorteddata, ypart, x))
}
