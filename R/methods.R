# Plot methods for R6 Classes
plot.ESR.Spectrum <- function(x, ...) {
  if ("R6" %in% class(x)) {
    main <- x$originator
    x <- x$data
  } else {
    class(x) <- class(x)[which(class(x) != "ESR.Spectrum")]
    main <- "ESR Spectrum"
  } 
  xlim <- range(pretty(x$x))
  if (all(x$x < 3)) {
    xlab <- "g-factor"
    xlim <- rev(xlim)
  } else {
    if (all(x$x < 2049)) {
      xlab <- "Datapoint"
    } else {
      xlab <- "Magnetic Field (G)"
    }
  }
  plot(x, type = "l", main = main, xlab = xlab, ylab = "Intensity (a.u.)", xlim = xlim, ...)
  mtext(describe_spectrum(rev(attributes(x)$spectrum)), cex = 0.8, line = 0.25)
  class(x) <- c("ESR.Spectrum", class(x))
}

describe_spectrum <- function(x) {
  str <- vector("character", length(x))
  for (i in seq_along(x)) {
    if (x[i] == "diff") {
      if (exists("cnt")) {
        if (cnt > 0) {
          cnt <- cnt - 1
          next
        }
      }
      cnt <- 0
      j <- 1
      while(x[i+j] == "diff" && !is.na(x[i+j])) {
        j <- j+1
        cnt <- cnt+1
      }
      str[i] <- paste0(j, ". derivative")
    }
    else if (x[i] == "spline") {
      if (exists("cnt_spline")) {
        if (cnt_spline > 0) {
          cnt_spline <- cnt_spline - 1
          next
        }
      }
      cnt_spline <- 0
      j <- 1
      while(x[i+j] == "spline" && !is.na(x[i+j])) {
        j <- j+1
        cnt_spline <- cnt_spline + 1
      }
      str[i] <- "smoothed"
    }
    else if (x[i] == "integral") {
      if (exists("cnt_int")) {
        if (cnt_int > 0) {
          cnt_int <- cnt_int - 1
          next
        }
      }
      cnt_int <- 0
      j <- 1
      while(x[i+j] == "integral" && !is.na(x[i+j])) {
        j <- j+1
        cnt_int <- cnt_int+1
      }
      str[i] <- paste0(j, ". integral")
    }
    else if (x[i] == "spectrum" && length(x) == 1) str[i] <- "spectrum"
  }#EndOf::loop
  str <- paste(str[str != ""], collapse = " ")
  return(str)
}



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