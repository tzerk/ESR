# Plot methods for R6 Classes
plot.ESR.Spectrum <- function(x, ...) {
  args <- list(type = "l", main = "ESR Spectrum", xlab = "Magnetic field (G)", ylab = "Intensity (a.u.)", ...)
  if ("data.frame" %in% class(x)) graphics::pl(x, args)
}

# Methods for R6 Classes

# Methods for objects of class ESR.Spectrum
differential <- function(x) {
  d <- data.table(cbind(x$x[1:c(length(x$x)-1)], diff(x$y)))
  setnames(d, c("x", "y"))
  class(d) <- c("ESR.Spectrum", class(d))
  return(d)
}

integrate <- function(x) {
  t <- x$y
  for (i in seq_along(t)) 
    t[i] <- sum(x$y[1:i])
  x$y <- t
  class(x) <- c("ESR.Spectrum", class(x))
  return(x)
}

s.spline <- function(x, ...) {
  s <- smooth.spline(x, ...)
  class(s) <- c("ESR.Spectrum", class(s))
  return(s)
}