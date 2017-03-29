## -------------
## A function to describe the ESR spectra based on the attributes of the $data
## slot of the ESR.Spectrum object. All ESR.Spectrum methods that modify the
## spectrum (e.g., get_diff and get_spline) add a new attribute to the data.
## If the data has attributes "spline" and "diff" this function returns the 
## string "smoothed 1. derivative". It is currently only used when calling 
## the generic plot() on ESR.Spectrum objects for argument 'main'.
## -------------
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

## -------------
## Alignment of spectra using cross-correlation
## Source: http://stackoverflow.com/questions/10369109/finding-lag-at-which-cross-correlation-is-maximum-ccf
## -------------
abs_max_ccf <- function(a, b, lag.max = length(a) / 2) {
  d <- ccf(a, b, plot = FALSE, lag.max = lag.max)
  cor <- d$acf[ , ,1]
  abscor <- abs(d$acf[ , ,1])
  lag <- d$lag[ , ,1]
  abs.cor.max <- abscor[which.max(abscor)]
  abs.cor.max.lag <- lag[which.max(abscor)]
  return(abs.cor.max.lag)
}
