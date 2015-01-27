# Methods for R6 Classes

# Methods for objects of class ESR.Spectrum
differential <- function(data) {
  if (!is.data.table(data) || !is.data.frame(data)) 
    stop(paste("Invalid data type: ", typeof(data)))
  
  d <- data.table(cbind(data$x[1:c(length(data$x)-1)], diff(data$y)))
  setnames(d, c("x", "y"))
  return(d)
}