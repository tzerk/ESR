#' Plot ESR spectra and peak finding
#' 
#' Function to plot an ESR spectrum and finding peaks using an automated
#' routine. Peaks can also be picked manually to calculate the amplitude in
#' intensity.
#' 
#' \bold{Status} \cr\cr In progress
#' 
#' @param input.data \code{\link{data.frame}} (\bold{required}): data frame
#' with two columns for x=magnetic.field or g.value, y=ESR.intensity.
#' @param difference \code{\link{logical}} (with default): plot first
#' derivative of the spectrum
#' @param integrate \code{\link{logical}} (with default): plot integrand of the
#' spectrum
#' @param smooth.spline \code{\link{logical}} (with default): fit a cubic
#' smoothing spline to supplied spectrum.
#' @param smooth.spline.df \code{\link{integer}}: desired number of degrees of
#' freedom
#' @param smooth.spline.diff.df \code{\link{integer}}: desired number of
#' degrees of freedom for splines of the first derivative
#' @param overlay \code{\link{logical}} (with default): overlay actual data and
#' smoothing spline curve in one plot.
#' @param auto.shift \code{\link{logical}} (with default): automatically shift
#' multiple spectra by their maximum peak. This uses smoothing splines for
#' better results.
#' @param find.peaks \code{\link{logical}} (with default): find and plot peaks
#' (\code{TRUE/FALSE}).
#' @param peak.range \code{\link{integer}} (with default): range of magnetic
#' field intensities or g-values in which peaks are picked from \code{c(from,
#' to)}. If no values are provide the whole spectrum is analysed.
#' @param peak.threshold \code{\link{integer}} (with default): threshold value
#' specifying the resolution of the peak finding routine (see details).
#' @param peak.information \code{\link{logical}} (with default): plot peak
#' intensity values for peaks found by the automated routine
#' (\code{TRUE/FALSE}). Applies only when \code{find.peaks = TRUE}.
#' @param manual.peaks \code{\link{logical}} (with default): manually identify
#' xy-coordinates to be used for amplitude calculation (\code{TRUE/FALSE}).
#' @param manual.peaks.roi \code{\link{logical}} (with default): define area
#' for the plot to be zoomed in to allow for a more precise manual peak picking
#' (\code{TRUE/FALSE}). Applies only when \code{manual.peaks = TRUE}.
#' @param info \code{\link{character}}: add information on experimental details
#' as subtitle
#' @param output.console \code{\link{logical}} (with default): print output
#' (\code{TRUE/FALSE}).
#' @param output.plot \code{\link{logical}} (with default): show plot
#' (\code{TRUE/FALSE}).
#' @param add \code{\link{logical}} (with default): whether derivatives and/or
#' integrands are added to the spectrum or are shown separately
#' (\code{TRUE/FALSE}).
#' @param \dots Further plot arguments to pass.
#' @return Returns terminal output and a plot. In addition, a list is returned
#' containing the following elements:
#' 
#' \item{data}{list containing the (modified) input data} \item{splines}{list
#' containing the spline objects} \item{auto.peaks}{data frame containing the
#' peak information (magnetic field and ESR intensity) found by the peak find
#' routine.} \item{manual.peaks}{vector containing the peak information
#' (magnetic field, ESR intensity and amplitude) from the manually identified
#' peaks.}
#' @export
#' @note In progress
#' @author Christoph Burow, University of Cologne (Germany) Who wrote it
#' @seealso \code{\link{plot}}
#' @references In progress
#' @examples
#' 
#' 
#' ##load example data
#' data(ExampleData.ESRspectra, envir = environment())
#' 
#' ##plot dpph and use the automatic peak finding routine
#' plot_ESRspectrum(ExampleData.ESRspectra$dpph, find.peaks = TRUE,
#'                  peak.range = c(3340,3355),
#'                  peak.threshold = 10, peak.information = TRUE,
#'                  output.console = TRUE)
#' 
#' ##plot the mollusc (sample Ba01) natural ESR spectrum with a smoothing spline
#' plot_ESRspectrum(ExampleData.ESRspectra$Ba01_00,
#'                  smooth.spline = TRUE,
#'                  smooth.spline.df = 40,
#'                  overlay = TRUE)
#' 
#' ##plot all ESR spectra of sample Ba01
#' plot_ESRspectrum(ExampleData.ESRspectra$Ba01)
#' 
#' ##plot all ESR spectra of sample Ba01 and align curves by the max peak
#' plot_ESRspectrum(ExampleData.ESRspectra$Ba01,
#'                  auto.shift = TRUE)
#' 
#' ##plot all ESR spectra of sample Ba01, use smoothing splines and
#' ##align curves by the max peak
#' plot_ESRspectrum(ExampleData.ESRspectra$Ba01,
#'                  smooth.spline = TRUE,
#'                  smooth.spline.df = 40,
#'                  auto.shift = TRUE,
#'                  overlay = FALSE)
#' 
#' 
#' 
#' @export plot_ESRspectrum
plot_ESRspectrum <- function(input.data, difference = FALSE, integrate = FALSE, 
                             smooth.spline = FALSE, smooth.spline.df, smooth.spline.diff.df, overlay = TRUE, 
                             auto.shift = FALSE, find.peaks = FALSE, peak.range, peak.threshold = 10, 
                             peak.information = FALSE, manual.peaks = FALSE, manual.peaks.roi = FALSE, 
                             info = NULL, output.console = TRUE, output.plot = TRUE, add = FALSE, 
                             ...) {
  
  
  ## ==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ## ==========================================================================##
  
  ## check if provided data fulfill the requirements
  
  # 1. check if input.data is a data.frame or RLum.Data.Curve object
  if (is.data.frame(input.data) == FALSE) {
    if (is(input.data, "RLum.Data.Curve") == FALSE) {
      stop("\n [plot_ESRspectrum] >> input.data has to be of type data.fame or RLum.Data.Curve!")
      
    } else {
      input.data <- get_RLum.Data.Curve(input.data)
    }
  } else {
    
    # 2. verify if data frame has two or three columns
    if (is.data.frame(input.data) == TRUE) {
      if (length(input.data) != 2) {
        cat(paste("Please provide a data frame with two columns", 
                  "(x=magnetic.field, y=ESR.intensity)"), fill = FALSE)
        stop(domain = NA)
      }
    }
    
    input.data <- list(input.data)
  }
  
  
  
  ## ==========================================================================##
  ## PREPARE INPUT/OUTPUT DATA
  ## ==========================================================================##
  
  # save column names for legend
  colnames <- colnames(input.data)
  
  # difference
  if (difference == TRUE) {
    temp <- lapply(input.data, function(x) {
      diff(x[, 2])
    })
    
    deriv_one <- list()
    
    for (i in 1:length(input.data)) {
      deriv_one[[i]] <- as.data.frame(cbind(input.data[[i]][1:length(input.data[[i]][, 
                                                                                     1]) - 1, 1], temp[[i]]))
    }
    deriv_one <- lapply(deriv_one, function(x) {
      colnames(x) <- c("x", "y")
      x
    })
  }
  
  # label input.data data frame for easier addressing
  input.data <- lapply(input.data, function(x) {
    colnames(x) <- c("x", "y")
    x
  })
  
  
  ## ==========================================================================##
  ## INTEGRAL
  ## ==========================================================================##
  
  if (integrate == TRUE || auto.shift == TRUE) {
    
    temp <- lapply(input.data, function(x) {
      
      t1 <- as.numeric(x[, 2])
      t2 <- x[, 2]
      for (i in 1:length(t1)) {
        
        t1[i] <- sum(t2[1:i])
      }
      return(t1)
    })
    
    integrand <- list()
    
    for (i in 1:length(input.data)) {
      integrand[[i]] <- as.data.frame(cbind(input.data[[i]][1:length(input.data[[i]][, 
                                                                                     1]), 1], temp[[i]]))
    }
    
    integrand <- lapply(integrand, function(x) {
      colnames(x) <- c("x", "y")
      x
    })
  }
  
  ## ==========================================================================##
  ## SPLINE DIFFERENCE
  ## ==========================================================================##
  
  if (smooth.spline == TRUE && difference == TRUE) {
    
    if (missing(smooth.spline.diff.df) == TRUE) {
      smooth.spline.diff.df <- smooth.spline(deriv_one[[1]])$df
    } else {
      smooth.spline.diff.df <- smooth.spline.diff.df
    }
    
    deriv_one.spline <- lapply(deriv_one, function(x) {
      smooth.spline(x, df = smooth.spline.diff.df)
    })
    
  }
  
  ## ==========================================================================##
  ## CHECK ... ARGUMENTS
  ## ==========================================================================##
  
  extraArgs <- list(...)
  
  if ("ylim" %in% names(extraArgs)) {
    ylim <- extraArgs$ylim
  } else {
    
    if (difference == TRUE && add == FALSE) {
      if (smooth.spline == FALSE || overlay == TRUE) {
        ylim.data <- deriv_one
      } else {
        ylim.data <- lapply(deriv_one.spline, function(x) {
          data.frame(x = x[1], y = x[2])
        })
      }
    } else {
      ylim.data <- input.data
    }
    
    ymin <- min(unlist((lapply(ylim.data, function(x) {
      min(x[2])
    }))))
    ymax <- max(unlist((lapply(ylim.data, function(x) {
      max(x[2])
    }))))
    
    
    ymax <- ymax * 1.2
    if (ymin < 0) {
      ymin <- ymin * 1.2
    } else {
      ymin <- ymin * 0.8
    }
    ylim <- c(ymin, ymax)
  }
  
  if ("xlim" %in% names(extraArgs)) {
    xlim <- extraArgs$xlim
  } else {
    # xlim<- c(min(input.data[[1]][1])*0.9998,
    # max(input.data[[1]][1])*1.0002)
    xlim <- range(pretty(c(min(input.data[[1]][1]), max(input.data[[1]][1]))))
  }
  
  if ("main" %in% names(extraArgs)) {
    main <- extraArgs$main
  } else {
    main = "ESR Spectrum"
  }
  
  if ("xlab" %in% names(extraArgs)) {
    xlab <- extraArgs$xlab
  } else {
    xlab <- expression("Magnetic field [G]")
  }
  
  if ("ylab" %in% names(extraArgs)) {
    ylab <- extraArgs$ylab
  } else {
    ylab <- c(paste("ESR intensity [a.u.]", sep = ""))
  }
  
  if ("cex" %in% names(extraArgs)) {
    cex <- extraArgs$cex
  } else {
    cex <- 1
  }
  
  if ("legend" %in% names(extraArgs)) {
    legend <- extraArgs$legend
  } else {
    legend <- TRUE
  }
  
  if ("legend.pos" %in% names(extraArgs)) {
    legend.pos <- extraArgs$legend.pos
  } else {
    legend.pos <- "topright"
  }
  
  if ("type" %in% names(extraArgs)) {
    type <- extraArgs$type
  } else {
    type <- "l"
  }
  
  if ("pch" %in% names(extraArgs)) {
    pch <- extraArgs$pch
  } else {
    pch <- 1
  }
  
  if ("col" %in% names(extraArgs)) {
    col <- extraArgs$col
  } else {
    col <- "black"
  }
  
  if ("lty" %in% names(extraArgs)) {
    lty <- extraArgs$lty
  } else {
    lty <- 1
  }
  
  if ("lwd" %in% names(extraArgs)) {
    lwd <- extraArgs$lwd
  } else {
    lwd <- 1
  }
  
  if ("id" %in% names(extraArgs)) {
    id <- extraArgs$id
  } else {
    id <- FALSE
  }
  
  if ("amplitudes" %in% names(extraArgs)) {
    amplitudes <- extraArgs$amplitudes
  } else {
    amplitudes <- NULL
  }
  
  ## ==========================================================================##
  ## CUBIC SMOOTHING SPLINE
  ## ==========================================================================##
  
  if (smooth.spline == TRUE || auto.shift == TRUE) {
    if (missing(smooth.spline.df) == TRUE) {
      smooth.spline.df <- smooth.spline(input.data[[1]])$df
    }
    
    spline <- lapply(input.data, function(x) {
      smooth.spline(x, df = smooth.spline.df)
    })
  }
  
  ## ==========================================================================##
  ## SHIFT SPECTRA
  ## ==========================================================================##
  
  if (auto.shift == TRUE && (length(input.data) > 1) == TRUE) {
    
    # DEPRECATED - shift spectra by maximum peak in smoothing splines
    # pos.peak.max<- unlist( lapply(spline, function(x) {
    # x[[1]][which.max(x[[2]])] }) )
    
    # shift peaks by maximum peak of integrand
    pos.peak.max <- unlist(lapply(integrand, function(x) {
      x[[1]][which.max(x[[2]])]
    }))
    
    diff.peak.max <- pos.peak.max[1] - pos.peak.max
    
    for (i in 1:length(input.data)) {
      
      # shift real data
      input.data[[i]][, 1] <- input.data[[i]][, 1] + diff.peak.max[i]
      
      if (smooth.spline == TRUE) {
        # shift splines
        spline[[i]][[1]] <- spline[[i]][[1]] + diff.peak.max[i]
      }
      
      # shift integrand
      integrand[[i]][[1]] <- integrand[[i]][[1]] + diff.peak.max[i]
      
      if (difference == TRUE) {
        # shift spline of derivative
        deriv_one.spline[[i]][[1]] <- deriv_one.spline[[i]][[1]] + 
          diff.peak.max[i]
      }
      
    }
  }
  
  ## ==========================================================================##
  ## FIND PEAKS
  ## ==========================================================================##
  
  if (find.peaks == TRUE && length(input.data) == 1) {
    
    
    if (missing(peak.range) == TRUE) {
      peak.range <- c(min(input.data[[1]][, 1]), max(input.data[[1]][, 
                                                                     1]))
    }
    
    
    input.temp <- input.data
    
    if (smooth.spline == TRUE) {
      if (difference == TRUE) {
        input.data[[1]][1:1023, 2] <- deriv_one.spline[[1]]$y
      } else {
        input.data[[1]][, 2] <- spline[[1]]$y
      }
    } else {
      if (difference == TRUE) {
        input.data[[1]] <- deriv_one[[1]]
      }
    }
    
    ## Preparation
    
    scan.width <- seq(from = 1, to = length(input.data[[1]]$x), by = 1)
    
    peak.max.storage <- matrix(data = NA, nrow = length(scan.width), 
                               ncol = 2)
    
    peak.min.storage <- matrix(data = NA, nrow = length(scan.width), 
                               ncol = 2)
    
    ## FIND PEAKS
    
    for (i in 1:length(input.data[[1]]$x)) {
      
      # find max peaks
      if (any(abs(input.data[[1]]$y[i:c(i + if (i + peak.threshold > 
                                                  length(input.data[[1]]$x)) {
        length(input.data[[1]]$x) - i
      } else {
        peak.threshold
      })]) > abs(input.data[[1]]$y[i])) == FALSE) {
        
        if (any(abs(input.data[[1]]$y[c(i - if (i < peak.threshold) {
          i - 1
        } else {
          peak.threshold
        }):i]) > abs(input.data[[1]]$y[i])) == TRUE) {
        } else {
          if (input.data[[1]]$x[i] > peak.range[1] && input.data[[1]]$x[i] < 
                peak.range[2]) {
            peak.max.storage[i, ] <- as.matrix(c(input.data[[1]]$x[i], 
                                                 input.data[[1]]$y[i]))
          }
          
        }
      }
      # find min peaks
      if (any(abs(input.data[[1]]$y[i:c(i + if (i + peak.threshold > 
                                                  length(input.data[[1]]$x)) {
        length(input.data[[1]]$x) - i
      } else {
        peak.threshold
      })]) < abs(input.data[[1]]$y[i])) == FALSE) {
        
        if (any(abs(input.data[[1]]$y[c(i - if (i < peak.threshold) {
          i - 1
        } else {
          peak.threshold
        }):i]) < abs(input.data[[1]]$y[i])) == TRUE) {
        } else {
          if (input.data[[1]]$x[i] > peak.range[1] && input.data[[1]]$x[i] < 
                peak.range[2]) {
            peak.min.storage[i, ] <- as.matrix(c(input.data[[1]]$x[i], 
                                                 input.data[[1]]$y[i]))
          }
        }
      }
    }
    
    
    
    all.peaks <- as.data.frame(rbind(na.omit(peak.max.storage), na.omit(peak.min.storage)))
    all.peaks <- all.peaks[order(all.peaks[, 1]), ]
    colnames(all.peaks) <- c("magnetic.field", "ESR.intensity")
    
    input.data <- input.temp
  }
  
  ## ==========================================================================##
  ## TERMINAL OUTPUT
  ## ==========================================================================##
  
  if (output.console == TRUE && find.peaks == TRUE && length(input.data) == 
        1) {
    print(all.peaks)
  }
  
  ## ==========================================================================##
  ## PLOTTING
  ## ==========================================================================##
  
  if (output.plot == TRUE) {
    
    colororder = c(rgb(0, 0, 0), rgb(0, 0, 1), rgb(0, 0.5, 0), rgb(1, 
                                                                   0, 0), rgb(0, 0.75, 0.75), rgb(0.75, 0, 0.75), rgb(0.75, 0.75, 
                                                                                                                      0), rgb(0.25, 0.25, 0.25), rgb(0.75, 0.25, 0.25), rgb(0.95, 
                                                                                                                                                                            0.95, 0), rgb(0.25, 0.25, 0.75), rgb(0.75, 0.75, 0.75), rgb(0, 
                                                                                                                                                                                                                                        1, 0), rgb(0.76, 0.57, 0.17), rgb(0.54, 0.63, 0.22), rgb(0.34, 
                                                                                                                                                                                                                                                                                                 0.57, 0.92), rgb(1, 0.1, 0.6), rgb(0.88, 0.75, 0.73), rgb(0.1, 
                                                                                                                                                                                                                                                                                                                                                           0.49, 0.47), rgb(0.66, 0.34, 0.65), rgb(0.99, 0.41, 0.23))
    
    if (is.null(info) == TRUE) {
      # general plot parameters
      par(cex = cex, xaxs = "i", yaxs = "i", mar = c(4, 4, 2, 2) + 
            0.2)
    } else {
      # general plot parameters
      par(cex = cex, xaxs = "i", yaxs = "i", mar = c(4, 4, 7, 2) + 
            0.2)
    }
    
    # create empty plot
    plot(NA, NA, ylim = ylim, xlim = xlim, bty = "n", xpd = FALSE, 
         xlab = xlab, ylab = ylab)
    
    # add plot title
    title(main, line = if (is.null(info) == TRUE) {
      1
    } else {
      6
    }, cex = 0.8)
    
    ## add subtitle experimental settings
    if (is.null(info) == FALSE) {
      
      # 1. add input fields
      inf <- list(inf.rec = c("Receiver Gain", "Phase", "Harmonic", 
                              "Mod. Freq.", "Mod Amplitude"), inf.sig = c("Conversion", 
                                                                          "Time Const", "Sweep Time", "Number of Scans"), inf.field = c("Center Field", 
                                                                                                                                        "Sweep Width", "Resolution"), inf.mw = c("Frequency", "Power"))
      
      at.adj <- c(0, 0.275, 0.55, 0.825)
      
      for (i in 1:length(inf)) {
        k <- 5.5
        for (j in 1:length(inf[[i]])) {
          k <- k - 1
          mtext(text = inf[[i]][j], side = 3, line = k, cex = 0.7, 
                adj = 0, at = par("usr")[1] + at.adj[i] * diff(par("usr")[1:2]))
        }
      }
      # 2. add values
      at.adj <- c(0.11, 0.405, 0.65, 0.91)
      
      for (i in 1:length(info)) {
        k <- 5.5
        for (j in 1:length(info[[i]])) {
          k <- k - 1
          mtext(text = paste(": ", info[[i]][j]), side = 3, line = k, 
                cex = 0.7, adj = 0, at = par("usr")[1] + at.adj[i] * 
                  diff(par("usr")[1:2]))
        }
      }
    }
    
    # add amplitude lines
    if (is.null(amplitudes) == FALSE) {
      x1 <- amplitudes[1]
      x2 <- amplitudes[2]
      
      if (smooth.spline == TRUE) {
        temp.data <- cbind(spline[[1]]$x, spline[[1]]$y)
      } else {
        temp.data <- input.data[[1]]
      }
      
      
      y1 <- temp.data[which.min(abs(temp.data[, 1] - x1)), 2]
      y2 <- temp.data[which.min(abs(temp.data[, 1] - x2)), 2]
      
      lines(x = c(x1, x1), y = c(y1, y2), lty = 2, col = "grey")
      
      lines(x = c(x1, x2), y = c(y2, y2), lty = 2, col = "grey")
      
      amp <- as.expression(bquote(italic(hat(u)) ~ "=" ~ .(abs(y1 - 
                                                                 y2))))
      
      if (amplitudes[1] < amplitudes[2]) {
        text(x = min(amplitudes), y = min(c(y1, y2)), labels = amp, 
             pos = 1, cex = 0.8)
      } else {
        text(x = max(amplitudes), y = max(c(y1, y2)), labels = amp, 
             pos = 3, , cex = 0.8)
      }
    }
    
    # background of the plot region
    if (length(input.data) > 1) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
           col = "grey90")
      
      grid(col = "white", lwd = 1, lty = 1)
    }
    
    
    plot_line <- function(data, color, id) {
      
      if (overlay == TRUE && smooth.spline == TRUE) {
        if (id == "input.data" || id == "deriv") {
          color <- adjustcolor(color, alpha.f = 0.33)
        }
      }
      
      if (add == TRUE) {
        
      }
      
      lines(data, col = color, lwd = lwd, lty = lty, type = type, 
            pch = pch)
    }  ##EndOf::plot_line()
    
    if (length(input.data) > 1) {
      col <- colororder
    }
    
    # add sprectum lines (either measured data or splines)
    for (i in 1:length(input.data)) {
      
      # INPUT DATA
      if (add == TRUE || c(smooth.spline == FALSE && difference == 
                             FALSE)) {
        plot_line(cbind(input.data[[i]]$x, input.data[[i]]$y), 
                  col[i], "input.data")
      }
      
      # SPLINE
      if (c(smooth.spline == TRUE && difference == FALSE) || c(smooth.spline == 
                                                                 TRUE && difference == TRUE && add == TRUE && overlay == 
                                                                 TRUE)) {
        plot_line(spline[[i]], col[i], "spline")
      }
      
      # DERIVATIVE
      if (difference == TRUE && c(smooth.spline == FALSE || c(smooth.spline == 
                                                                TRUE && overlay == TRUE))) {
        plot_line(deriv_one[[i]], col[i], "deriv")
      }
      
      # DERIVATIVE SPLINE
      if (difference == TRUE && smooth.spline == TRUE) {
        plot_line(deriv_one.spline[[i]], col[i], "deriv.spline")
      }
      
    }  ##EndOf::Loop
    
    # add legend
    if (length(input.data) > 1 && legend == TRUE) {
      legend(legend.pos, legend = colnames, lty = 1, lwd = 3, col = col, 
             cex = 0.8, ncol = 2)
    }
    
    temp.plot <- recordPlot()
    
    
    ## ==========================================================================##
    ## AUTOMATIC PEAK FINDING
    ## ==========================================================================##
    
    
    if (find.peaks == TRUE && length(input.data) == 1) {
      
      # plot min/max peaks
      points(all.peaks, col = "red", pch = 19)
      
      # label min/max peaks
      if (peak.information == TRUE && length(all.peaks$magnetic.field) != 
            0) {
        
        if (id == TRUE) {
          text(all.peaks$magnetic.field, all.peaks$ESR.intensity, 
               labels = 1:length(all.peaks$ESR.intensity), pos = 2, 
               cex = 0.8, xpd = TRUE)
        } else {
          text(all.peaks$magnetic.field, all.peaks$ESR.intensity, 
               labels = round(all.peaks$ESR.intensity), pos = 2, cex = 0.8, 
               xpd = TRUE)
        }
      }
      temp.plot <- recordPlot()
    }
    
    ## ==========================================================================##
    ## ADD INTEGRAND TO PLOT
    ## ==========================================================================##
    
    if (integrate == TRUE) {
      
      y <- max(unlist(lapply(integrand, function(x) {
        max(x[2])
      })))
      
      for (i in 1:length(integrand)) {
        par(new = TRUE)
        plot(NA, bty = "n", xaxt = "n", yaxt = "n", xlab = "", 
             ylab = "", ylim = c(-y, y), xlim = xlim)
        lines(integrand[[i]], col = col[i], lwd = lwd, lty = lty, 
              type = type, pch = pch)
        if (i == 1) {
          axis(4)
        }
      }
    }
    
    ## ==========================================================================##
    ## MANUAL PEAK IDENTIFICATION
    ## ==========================================================================##
    
    if (manual.peaks == TRUE && length(input.data) == 1) {
      if (manual.peaks.roi == FALSE) {
        mtext("Choose two peaks", side = 3, line = 0, cex = 0.8)
      }
      
      identifyPch <- function(x, y = NULL, n = 2, pch = 19, ...) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        sel <- rep(FALSE, length(x))
        res <- integer(0)
        
        while (sum(sel) < n) {
          ans <- identify(x[!sel], y[!sel], n = 1, plot = TRUE, 
                          labels = round(y[!sel]), xpd = TRUE, ...)
          
          if (!length(ans)) 
            break
          ans <- which(!sel)[ans]
          points(x[ans], y[ans], pch = pch, cex = 1.4, col = "green4")
          sel[ans] <- TRUE
          res <- c(res, ans)
        }
        res
      }
      
      
      if (manual.peaks.roi == TRUE) {
        
        coords <- vector(mode = "integer")
        zoom.data <- if (smooth.spline == TRUE) {
          spline
        } else {
          input.data
        }
        
        for (i in 1:2) {
          mtext(paste("Define zoom area for peak", i), side = 3, 
                line = 0, cex = 0.8)
          
          zoom.area <- identifyPch(zoom.data[[1]]$x, zoom.data[[1]]$y, 
          )
          
          plot(zoom.data[[1]]$x, zoom.data[[1]]$y, main = main, 
               ylim = c(min(zoom.data[[1]]$y[zoom.area[1]:zoom.area[2]]), 
                        max(zoom.data[[1]]$y[zoom.area[1]:zoom.area[2]])), 
               xlim = zoom.data[[1]]$x[zoom.area], type = "l", bty = "l", 
               xpd = FALSE, xlab = expression("Magnetic field [G]"), 
               ylab = ylab)
          
          mtext(paste("Choose peak", i), side = 3, line = 0, cex = 0.8)
          
          coords[i] <- identifyPch(zoom.data[[1]]$x, zoom.data[[1]]$y, 
                                   n = 1)
          replayPlot(temp.plot)
          
        }
        
        points(zoom.data[[1]]$x[coords], zoom.data[[1]]$y[coords], 
               pch = 19, col = "green4", cex = 1.4)
        
        text(zoom.data[[1]]$x[coords], zoom.data[[1]]$y[coords], 
             labels = round(zoom.data[[1]]$y[coords]), pos = c(3, 
                                                               1), xpd = TRUE)
        
      } else {
        
        coords <- identifyPch(zoom.data[[1]]$x, zoom.data[[1]]$y)
        
      }
      
      
      amplitude <- abs(zoom.data[[1]]$y[coords[1]] - zoom.data[[1]]$y[coords[2]])
      
      
      
      cat(paste("\n ------- Selected Peaks -------"))
      cat(paste("\n Peak 1: ", round(zoom.data[[1]]$x[coords[1]]), 
                "[G] ", round(zoom.data[[1]]$y[coords[1]], 2), "[a.u.]"))
      cat(paste("\n Peak 2: ", round(zoom.data[[1]]$x[coords[2]]), 
                "[G] ", round(zoom.data[[1]]$y[coords[2]], 2), "[a.u.]"))
      cat(paste("\n Amplitude: ", round(amplitude), "[a.u.]"))
      
      
      
    }
  }
  ## ==========================================================================##
  ## RETURN VALUES
  ## ==========================================================================##
  
  # create data frame for output
  if (find.peaks == FALSE || length(input.data) > 1) {
    all.peaks <- NULL
  }
  if (find.peaks == TRUE && length(input.data) == 1) {
    man.peaks <- NULL
  }
  if (smooth.spline == FALSE) {
    spline <- NULL
    deriv_one.spline <- NULL
  }
  if (integrate == FALSE && auto.shift == FALSE) {
    integrand <- NULL
  }
  if (difference == FALSE) {
    deriv_one <- NULL
    deriv_one.spline <- NULL
  }
  
  if (manual.peaks == TRUE && length(input.data) == 1) {
    man.peaks <- data.frame(peak1_field = input.data[[1]]$x[1], peak1_intensity = input.data[[1]]$y[1], 
                            peak2_field = input.data[[1]]$x[2], peak2_intensity = input.data[[1]]$y[2], 
                            amplitude = amplitude)
  }
  
  # plot calculus data
  plot.par <- list(ylim = ylim, xlim = xlim, ylim.integrand = if (integrate == 
                                                                    TRUE) {
    c(-y, y)
  } else {
    NULL
  })
  
  # return output data.frame and nls.object fit
  invisible(list(data = input.data, derivative = deriv_one, integrand = integrand, 
                 splines = spline, diff.splines = deriv_one.spline, auto.peaks = all.peaks, 
                 manual.peaks = manual.peaks, plot.par = plot.par))
}