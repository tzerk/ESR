# Methods for R6 Classes

# Methods for objects of class ESR.Spectrum
differential <- function(x, order, ...) {
  d <- x$y
  for (i in seq_len(order)) {
    d <- diff(d, ...)
  }
  until <- nrow(x) - order
  c <- attributes(x)$spectrum
  x <- x[1:until] # TODO: This causes x to loose its attributes
  if (!is.null(c)) attr(x, "spectrum") <- c
  x$y <- d
  class(x) <- c("ESR.Spectrum", class(x))
  attr(x, "spectrum") <- c(attr(x, "spectrum"), rep("diff", order))
  return(x)
}

integrate <- function(x) {
  t <- x$y
  for (i in seq_along(t)) 
    t[i] <- sum(x$y[1:i])
  x$y <- t
  class(x) <- c("ESR.Spectrum", class(x))
  attr(x, "spectrum") <- c(attr(x, "spectrum"), "integral")
  return(x)
}

s.spline <- function(x, ...) {
  s <- smooth.spline(x, ...)
  x$x <- s$x
  x$y <- s$y
  class(x) <- c("ESR.Spectrum", class(x))
  attr(x, "spectrum") <- c(attr(x, "spectrum"), "spline")
  return(x)
}

gval <- function(v, H, x) {
  if (v >= 10) v <- as.numeric(paste0(strtrim(v, 1), ".", substr(v, 2, nchar(v))))
  
  planck_const <- 6.62606957e-34 # SI: J/s 
  bohr_magneton <- 9.27400968e-24 # SI: J/T
  g <- (planck_const * v * 10^9) / (bohr_magneton * 10^-4 * H)
  if (length(x$x) != length(g)) {
    t <- abs(length(x$x) - length(g))
    ifelse(length(x$x) > length(g), x <- x[1:c(nrow(x)-t)], g <- g[1:c(length(g)-1)]) # TODO: this causes x to loose its attributes
  }
  x$x <- g
  class(x) <- c("ESR.Spectrum", class(x))
  return(x)
}

peaks <- function(x, interval, th = 10) {
  p <- find_Peaks(x, interval, th)
  return(p)
}