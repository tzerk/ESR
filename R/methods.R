# Plot methods for R6 Classes
plot.ESR.Spectrum <- function(x, ...) {
  if ("R6" %in% class(x)) x <- x$data
  else class(x) <- class(x)[-1]
  plot(x, type = "l", main = "ESR Spectrum", xlab = "Magnetic field (G)", ylab = "Intensity (a.u.)", ...)
  class(x) <- c("ESR.Spectrum", class(x))
}

# Methods for R6 Classes

# Methods for objects of class ESR.Spectrum
differential <- function(x, order, ...) {
  d <- x$y
  for (i in seq_len(order)) {
    d <- diff(d, ...)
  }
  dt <- data.table(x$x[1:c(length(x$x)-order)], d)
  class(dt) <- c("ESR.Spectrum", class(dt))
  return(dt)
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

gval <- function(v, H) {
  # calculate g-value for all x-values in x$data
  planck_const <- 6.62606957e-27
  bohr_magneton <- 9.27400968e-24
  g <- ((planck_const * v) / (bohr_magneton * H)) * 10^6
  return(g)
}