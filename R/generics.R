#' @include utils.R

#' @export
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
  ylim <- range(pretty(x$y))
  
  plot(x, type = "l", main = main, xlab = xlab, ylab = "Intensity (a.u.)", xlim = xlim, ylim = ylim, ...)
  mtext(describe_spectrum(rev(attributes(x)$spectrum)), cex = 0.8, line = 0.25)
  class(x) <- c("ESR.Spectrum", class(x))
}

#' @export
summary.ESR.Spectrum <- function(object, ...) {
  return(summary(object$data, ...))
}

#' @export
str.ESR.Spectrum <- function(object, length = 3, ...) {
  names <- ls(object)
  struct <- lapply(names, function(x) {
    f <- get(x, envir = object)
    if (is.function(f))
      f <- paste0("function(", paste(names(formals(f)), collapse = ", "), ")")
    f
  })
  names(struct) <- names
  print(struct, quote = FALSE)
  return(NULL)
}

#' Coercion from and to ESR.Spectrum
#' 
#' A generic function \code{as.ESR.Spectrum} for coercing objects
#' to class "\code{ESR.Spectrum}".
#' 
#' \code{as.ESR.Spectrum} currently includes methods for \link{data.frame}, \link{data.table} objects. \cr\cr
#' Methods for coercing objects of class "\code{ESR.Spectrum}" to other classes currently include: 
#' \link{as.data.frame}, \link{as.data.frame}, \link{as.list}, \link{as.matrix}. Coercion from \code{ESR.Spectrum}
#' to any of these classes converts the raw measurement data (\code{$data} attribute) to respective class.
#' 
#' @param x an object to coerce to an \code{ESR.Spectrum}
#' @return \code{as.ESR.Spectrum} returns an \code{ESR.Spectrum} object.
#' @seealso \link{R6}, \link{R6Class}
#' @author Christoph Burow, University of Cologne (Germany)
#' @export as.ESR.Spectrum
#' @examples
#' 
#' ## coercion to ESR.Spectrum:
#' ## default method
#' x <- as.ESR.Spectrum(data.frame(seq(3350, 3450, length.out = 1024), 
#'                                 runif(1024, -1000, 1000)))
#' 
#' ## coercion from ESR.Spectrum:
#' as.matrix(x)
#' as.data.frame(x)
#' data.table::as.data.table(x)
#' as.list(x)
#' 
as.ESR.Spectrum <- function(x) {
  ## Input validation ----
  if (class(x)[1] != "data.frame" && class(x)[1] != "data.table")
    stop("Please provide an object of class data.frame or data.table", 
         call. = FALSE)
  if (ncol(x) != 2L)
    stop(paste0("Unexpected number of columns: ", ncol(x),". \n Please provide a two column data.frame or data.table."),
         call. = FALSE)
  
  # User argument as origin
  origin <- deparse(substitute(x))
  
  ## Coerce to data.table ----
  if (!is.data.table(x)) 
    x <- as.data.table(x)
  
  ## Create R6 Object ----
  obj <- ESR.Spectrum$new()
  obj$set_data(x)
  obj$set_origin(origin)
  return(obj)
}


#' @export
#' @rdname as.ESR.Spectrum
is.ESR.Spectrum <- function(x) {
  inherits(x, "ESR.Spectrum")
}

#' @export
# as.data.frame method for objects of class ESR.Spectrum
as.data.frame.ESR.Spectrum <- function(x, ...) {
  invisible(as.data.frame(x$data, ...))
}

#' @export
# as.data.table method for objects of class ESR.Spectrum
as.data.table.ESR.Spectrum <- function(x, ...) {
  invisible(as.data.table(x$data, ...))
}

#' @export
# as.list method for objects of class ESR.Spectrum
as.list.ESR.Spectrum <- function(x, ...) {
  invisible(as.list(x$data, ...))
}

#' @export
# as.matrix method for objects of class ESR.Spectrum
as.matrix.ESR.Spectrum <- function(x, ...) {
  invisible(as.matrix(x$data, ...))
}