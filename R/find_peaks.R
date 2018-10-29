#' A simple peak finding algorithm for ESR spectra
#' 
#' This is a simple algorithm to determine local minima and maxima of ESR spectra, but
#' can also be used for various other kind of data.
#' 
#' The algorithm characterises a peak by comparing the y-value at each x-value to its neighbouring y-values.
#' The threshold value specified by \code{th} determines to how many neighbours the value 
#' is compared to, so that lower values of \code{th} yield more peaks than higher
#' threshold values.
#' 
#' @param x \code{\link{data.frame}} (required): a two column \link{data.frame}
#' @param interval \code{\link{numeric}}: a vector of length two specifying
#' the range of x-values where peaks are searched
#' @param th \code{\link{numeric}}: an integer specifying the number of neighbouring values
#' to compare each x-value to
#' @return A \link{data.frame} containing the x- and y-values of each peak is returned.
#' @export
#' @author Christoph Burow, University of Cologne (Germany)
#' @examples
#' 
#' # Import Bruker ELEXSYS500 spectrum (ASCII)
#' file <- system.file("extdata", "dpph.ASC", package = "ESR")
#' spec <- read_Spectrum(file)
#' 
#' # Use the function
#' peaks1 <- find_Peaks(spec)
#' 
#' # Since spec is an object of class ESR.Spectrum, you can also use
#' # its get-method
#' peaks2 <- spec$get_peaks()
#' 
#' identical(peaks1, peaks2)
#' 
#' @export find_Peaks
find_Peaks <- function(x, interval, th = 10) {
  if (!inherits(x, "data.frame") && !inherits(x, "data.frame") && !inherits(x, "R6"))
    stop("x must be a data.frame", call. = FALSE)
  
  if (inherits(x, "R6") && inherits(x, "ESR.Spectrum"))
    x <- as.data.frame(x)
  if (inherits(x, "ESR.Spectrum"))
    class(x) <- class(x)[which(class(x) != "ESR.Spectrum")]
  if (ncol(x) != 2)
    stop("x must have two columns.", call. = FALSE)
  if (missing(interval))
    interval <- range(x[, 1])
  
  ## Preparation
  scan.width <- seq(from = 1, to = length(x[, 1]), by = 1)
  peak.max.storage <- peak.min.storage <- matrix(nrow = length(scan.width), ncol = 2)
  
  ## FIND PEAKS
  for (i in 1:length(x[, 1])) {
    # find max peaks
    if (any(abs(x[, 2][i:c(i + if (i + th > length(x[, 1])) {
      length(x[, 1]) - i
    } else {
      th
    })]) > abs(x[, 2][i])) == FALSE) {
      if (any(abs(x[, 2][c(i - if (i < th) {
        i - 1
      } else {
        th
      }):i]) > abs(x[, 2][i])) == TRUE) {
      } else {
        if (x[, 1][i] > interval[1] && x[, 1][i] < interval[2]) {
          peak.max.storage[i, ] <- as.matrix(c(x[, 1][i], 
                                               x[, 2][i]))
        }
      }
    }
    # find min peaks
    if (any(abs(x[, 2][i:c(i + if (i + th > length(x[, 1])) {
      length(x[, 1]) - i
    } else {
      th
    })]) < abs(x[, 2][i])) == FALSE) {
      if (any(abs(x[, 2][c(i - if (i < th) {
        i - 1
      } else {
        th
      }):i]) < abs(x[, 2][i])) == TRUE) {
      } else {
        if (x[, 1][i] > interval[1] && x[, 1][i] < interval[2]) {
          peak.min.storage[i, ] <- as.matrix(c(x[, 1][i], 
                                               x[, 2][i]))
        }
      }
    }
  }
  all.peaks <- as.data.frame(rbind(na.omit(peak.max.storage), na.omit(peak.min.storage)))
  setorder(all.peaks)
  setnames(all.peaks, c("magnetic.field", "ESR.intensity"))
  
  invisible(all.peaks)
}