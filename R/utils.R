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