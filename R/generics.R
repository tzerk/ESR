#' @include utils.R

#' @export
plot.ESR.Spectrum <- function(x, ...) {
  
  settings <- list(x = x,
                   main = "",
                   cex = 1.0,
                   xlim = c(0,0),
                   ylim = c(0,0),
                   xlab = "",
                   ylab = "Intensity (a.u.)",
                   type = "l")
  
  if ("R6" %in% class(settings$x)) {
    settings$main <- settings$x$originator
    settings$x <- settings$x$data
  } else {
    class(settings$x) <- class(x)[which(class(settings$x) != "ESR.Spectrum")]
    settings$main <- "ESR Spectrum"
  } 
  settings$xlim <- range(pretty(settings$x$x))
  if (all(settings$x$x < 3)) {
    settings$xlab <- "g-factor"
    settings$xlim <- rev(settings$xlim)
  } else {
    if (all(settings$x$x < 2049)) {
      settings$xlab <- "Datapoint"
    } else {
      settings$xlab <- "Magnetic Field (G)"
    }
  }
  settings$ylim <- range(pretty(settings$x$y))
  
  do.call(plot, settings)
  
  if ("mtext" %in% names(list(...)))
    mtext <- list(...)$mtext
  else
    mtext <- describe_spectrum(rev(attributes(settings$x)$spectrum))
  mtext(mtext, cex = 0.8, line = 0.25)
  
  return(x)
}

#' @export
plot.ESR.Spectrum.2D <- function(x, y, ...) {
  
  if (!requireNamespace("plotly", quietly = TRUE))
    stop("Plotting a 2D ESR spectrum requires the 'plotly' package. To install",
         " this package run 'install.packages('plotly')' in your R console.",
         call. = FALSE)
  
  # Settings
  settings <- list(dim = c("3D", "2D")[1],
                   reverse.labels = FALSE,
                   xlab = "NA",
                   ylab = "Magnetic Field (G)",
                   zlab = "ESR Intensity (a.u.)")
  settings <- modifyList(settings, list(...))
  
  # 3D Plot
  if (settings$dim == "3D") {
    
    p <- plotly::plot_ly(
      x = unique(unlist(x$data[ ,3])),
      y = unique(unlist(x$data[ ,1])),
      z = matrix(unlist(x$data[ ,2]), ncol = nrow(unique(x$data[,3])))
    )
    
    p <- plotly::add_surface(p, 
                             showscale = FALSE, 
                             contours = list(x = list(show = FALSE,
                                                      color = "#444",
                                                      highlight = TRUE),
                                             y = list(show = FALSE,
                                                      highlight = FALSE),
                                             z = list(show = FALSE,
                                                      highlight = FALSE)),
                             opacity = 0.66)
    
    p <- plotly::layout(p, 
                        scene = list(
                          xaxis = list(title = settings$xlab),
                          yaxis = list(title = settings$ylab),
                          zaxis = list(title = settings$zlab)))
    
  } else if (settings$dim == "2D") {
    
    intensities <- lapply(x$split(), function(y) {
      as.data.frame(y)[ ,2]
    })
    field <- unlist(x$split()[[1]]$data[ ,1])
    labels <- sapply(x$split(), function(y) {
      y$originator
    })
    
    if (settings$reverse.labels)
      labels <- rev(labels)
    
    if (all(grepl("=", labels))) {
      labels <- gsub("(.*)\\=", "", labels)
      labels <- gsub("[^0-9.]", "", labels)
    }
    
    p <- plotly::plot_ly(x = field)
    
    for (i in 1:length(intensities))
      p <- plotly::add_trace(p, y = intensities[[i]], 
                             name = labels[[i]],
                             mode = "lines", type = "scatter")
    
    p <- plotly::layout(p, legend = list(orientation = "v"))
    
  }
  
  print(p)
  
  return(p)
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
#' \code{as.ESR.Spectrum} currently includes methods for \link{data.frame} objects. \cr\cr
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
#' as.list(x)
#' 
as.ESR.Spectrum <- function(x) {
  ## Input validation ----
  if (class(x)[1] != "data.frame")
    stop("Please provide an object of class data.frame", 
         call. = FALSE)
  if (ncol(x) != 2L)
    stop(paste0("Unexpected number of columns: ", ncol(x),". \n Please provide a two column data.frame."),
         call. = FALSE)
  
  # User argument as origin
  origin <- deparse(substitute(x))
  
  ## Create R6 Object ----
  obj <- ESR.Spectrum$new()
  obj$set_data(x)
  obj$set_origin(origin)
  return(obj)
}

#' @export
# as.data.frame method for objects of class ESR.Spectrum
as.data.frame.ESR.Spectrum <- function(x, ...) {
  return(x$data)
}


#' @export
# as.list method for objects of class ESR.Spectrum
as.list.ESR.Spectrum <- function(x, ...) {
  return(as.list(x$data, ...))
}

#' @export
# as.matrix method for objects of class ESR.Spectrum
as.matrix.ESR.Spectrum <- function(x, ...) {
  return(as.matrix(x$data, ...))
}