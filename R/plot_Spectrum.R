#' Plot ESR spectra and peak finding
#' 
#' Function to plot an ESR spectrum and finding peaks using an automated
#' routine.
#' 
#' \bold{Status} \cr\cr In progress
#' 
#' @param data \code{\link{data.frame}} (\bold{required}): data frame
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
#' @param info \code{\link{character}}: add information on experimental details
#' as subtitle
#' @param plot \code{\link{logical}} (with default): show plot
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
#' routine.}
#' @export
#' @note In progress
#' @author Christoph Burow, University of Cologne (Germany)
#' @seealso \code{\link{plot}}
#' @examples
#' 
#' 
#' ##load example data
#' data(ExampleData.ESRspectra, envir = environment())
#' 
#' ##plot dpph and use the automatic peak finding routine
#' plot_Spectrum(ExampleData.ESRspectra$dpph, find.peaks = TRUE,
#'                  peak.range = c(3340,3355),
#'                  peak.threshold = 10, peak.information = TRUE,
#'                  verbose = TRUE)
#' 
#' ##plot the mollusc (sample Ba01) natural ESR spectrum with a smoothing spline
#' plot_Spectrum(ExampleData.ESRspectra$Ba01_00,
#'                  smooth.spline = TRUE,
#'                  smooth.spline.df = 40,
#'                  overlay = TRUE)
#' 
#' ##plot all ESR spectra of sample Ba01
#' plot_Spectrum(ExampleData.ESRspectra$Ba01)
#' 
#' ##plot all ESR spectra of sample Ba01 and align curves by the max peak
#' plot_Spectrum(ExampleData.ESRspectra$Ba01,
#'                  auto.shift = TRUE)
#' 
#' ##plot all ESR spectra of sample Ba01, use smoothing splines and
#' ##align curves by the max peak
#' plot_Spectrum(ExampleData.ESRspectra$Ba01,
#'                  smooth.spline = TRUE,
#'                  smooth.spline.df = 40,
#'                  auto.shift = TRUE,
#'                  overlay = FALSE)
#' 
#' 
#' @export plot_Spectrum
plot_Spectrum <- function(data, difference = FALSE, integrate = FALSE, 
                          smooth.spline = FALSE, smooth.spline.df, smooth.spline.diff.df, overlay = TRUE, 
                          auto.shift = FALSE, find.peaks = FALSE, peak.range, peak.threshold = 10, 
                          peak.information = FALSE, info = NULL, plot = TRUE, add = FALSE, 
                          ...) {
  
  
  ## ==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ## ==========================================================================##
  
  ## check if provided data fulfill the requirements
  if (is.data.frame(data)) {
    if (length(data) != 2)
      stop("\n Please provide a data frame with two columns (x=magnetic.field, y=ESR.intensity)")
    if (is.data.table(data))
      data <- as.data.frame(data)
    
    data <- list(data)
  } 
  else if (is.list(data)) {
    # TODO: check if all list items are dataframes
  }
  else if ("ESR.Spectrum" %in% class(data)) {
    data <- list(as.data.frame(data$data))
  }
  else stop("\n [plot_Spectrum] >> data has to be of type data.fame, data.table list or ESR.Spectrum!")
  
  ## ==========================================================================##
  ## PREPARE INPUT/OUTPUT DATA
  ## ==========================================================================##
  
  # save column names for legend
  colnames <- colnames(data)
  
  # difference
  if (difference == TRUE) {
    temp <- lapply(data, function(x) {
      diff(x[, 2])
    })
    
    deriv_one <- list()
    
    for (i in 1:length(data)) {
      deriv_one[[i]] <- as.data.frame(cbind(data[[i]][1:length(data[[i]][ , 1]) - 1, 1], temp[[i]]))
    }
    deriv_one <- lapply(deriv_one, function(x) {
      colnames(x) <- c("x", "y")
      x
    })
  }
  
  # label data data frame for easier addressing
  data <- lapply(data, function(x) {
    colnames(x) <- c("x", "y")
    x
  })
  
  
  ## ==========================================================================##
  ## INTEGRAL
  ## ==========================================================================##
  
  if (integrate == TRUE || auto.shift == TRUE) {
    temp <- lapply(data, function(x) {
      t1 <- as.numeric(x[, 2])
      t2 <- x[, 2]
      for (i in 1:length(t1)) {
        t1[i] <- sum(t2[1:i])
      }
      return(t1)
    })
    integrand <- list()
    for (i in 1:length(data)) {
      integrand[[i]] <- as.data.frame(cbind(data[[i]][1:length(data[[i]][,1]), 1], temp[[i]]))
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
  if ("verbose" %in% names(extraArgs)) {
    verbose <- extraArgs$verbose
  } else {
    verbose <- TRUE
  }
  
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
      ylim.data <- data
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
    # xlim<- c(min(data[[1]][1])*0.9998,
    # max(data[[1]][1])*1.0002)
    xlim <- range(pretty(c(min(data[[1]][1]), max(data[[1]][1]))))
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
      smooth.spline.df <- smooth.spline(data[[1]])$df
    }
    spline <- lapply(data, function(x) {
      smooth.spline(x, df = smooth.spline.df)
    })
  }
  
  ## ==========================================================================##
  ## SHIFT SPECTRA
  ## ==========================================================================##
  
  if (auto.shift == TRUE && (length(data) > 1) == TRUE) {
    # DEPRECATED - shift spectra by maximum peak in smoothing splines
    # pos.peak.max<- unlist( lapply(spline, function(x) {
    # x[[1]][which.max(x[[2]])] }) )
    
    # shift peaks by maximum peak of integrand
    pos.peak.max <- unlist(lapply(integrand, function(x) {
      x[[1]][which.max(x[[2]])]
    }))
    
    diff.peak.max <- pos.peak.max[1] - pos.peak.max
    
    for (i in 1:length(data)) {
      # shift real data
      data[[i]][, 1] <- data[[i]][, 1] + diff.peak.max[i]
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
  
  if (find.peaks == TRUE && length(data) == 1) {
    if (missing(peak.range) == TRUE) {
      peak.range <- c(min(data[[1]][, 1]), max(data[[1]][,1]))
    }
    input.temp <- data
    if (smooth.spline == TRUE) {
      if (difference == TRUE) {
        data[[1]][1:1023, 2] <- deriv_one.spline[[1]]$y
      } else {
        data[[1]][, 2] <- spline[[1]]$y
      }
    } else {
      if (difference == TRUE) {
        data[[1]] <- deriv_one[[1]]
      }
    }
    
    ## Preparation
    scan.width <- seq(from = 1, to = length(data[[1]]$x), by = 1)
    
    peak.max.storage <- matrix(data = NA, 
                               nrow = length(scan.width), 
                               ncol = 2)
    peak.min.storage <- matrix(data = NA, 
                               nrow = length(scan.width), 
                               ncol = 2)
    
    ## FIND PEAKS
    for (i in 1:length(data[[1]]$x)) {
      # find max peaks
      if (any(abs(data[[1]]$y[i:c(i + if (i + peak.threshold > 
                                                  length(data[[1]]$x)) {
        length(data[[1]]$x) - i
      } else {
        peak.threshold
      })]) > abs(data[[1]]$y[i])) == FALSE) {
        
        if (any(abs(data[[1]]$y[c(i - if (i < peak.threshold) {
          i - 1
        } else {
          peak.threshold
        }):i]) > abs(data[[1]]$y[i])) == TRUE) {
        } else {
          if (data[[1]]$x[i] > peak.range[1] && data[[1]]$x[i] < 
                peak.range[2]) {
            peak.max.storage[i, ] <- as.matrix(c(data[[1]]$x[i], 
                                                 data[[1]]$y[i]))
          }
          
        }
      }
      # find min peaks
      if (any(abs(data[[1]]$y[i:c(i + if (i + peak.threshold > 
                                                  length(data[[1]]$x)) {
        length(data[[1]]$x) - i
      } else {
        peak.threshold
      })]) < abs(data[[1]]$y[i])) == FALSE) {
        
        if (any(abs(data[[1]]$y[c(i - if (i < peak.threshold) {
          i - 1
        } else {
          peak.threshold
        }):i]) < abs(data[[1]]$y[i])) == TRUE) {
        } else {
          if (data[[1]]$x[i] > peak.range[1] && data[[1]]$x[i] < 
                peak.range[2]) {
            peak.min.storage[i, ] <- as.matrix(c(data[[1]]$x[i], 
                                                 data[[1]]$y[i]))
          }
        }
      }
    }
    all.peaks <- as.data.frame(rbind(na.omit(peak.max.storage), na.omit(peak.min.storage)))
    all.peaks <- all.peaks[order(all.peaks[, 1]), ]
    colnames(all.peaks) <- c("magnetic.field", "ESR.intensity")
    data <- input.temp
  }
  
  ## ==========================================================================##
  ## TERMINAL OUTPUT
  ## ==========================================================================##
  if (verbose == TRUE && find.peaks == TRUE && length(data) == 1) {
    print(all.peaks)
  }
  
  ## ==========================================================================##
  ## PLOTTING
  ## ==========================================================================##
  
  if (plot == TRUE) {
    
    colororder = c(rgb(0, 0, 0), rgb(0, 0, 1), rgb(0, 0.5, 0), 
                   rgb(1, 0, 0), rgb(0, 0.75, 0.75), rgb(0.75, 0, 0.75), 
                   rgb(0.75, 0.75, 0), rgb(0.25, 0.25, 0.25), rgb(0.75, 0.25, 0.25), 
                   rgb(0.95,  0.95, 0), rgb(0.25, 0.25, 0.75), rgb(0.75, 0.75, 0.75), 
                   rgb(0, 1, 0), rgb(0.76, 0.57, 0.17), rgb(0.54, 0.63, 0.22), 
                   rgb(0.34,  0.57, 0.92), rgb(1, 0.1, 0.6), rgb(0.88, 0.75, 0.73),
                   rgb(0.1,  0.49, 0.47), rgb(0.66, 0.34, 0.65), rgb(0.99, 0.41, 0.23))
    
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
        temp.data <- data[[1]]
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
    if (length(data) > 1) {
      rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
           col = "grey90")
      
      grid(col = "white", lwd = 1, lty = 1)
    }
    
    
    plot_line <- function(data, color, id) {
      
      if (overlay == TRUE && smooth.spline == TRUE) {
        if (id == "data" || id == "deriv") {
          color <- adjustcolor(color, alpha.f = 0.33)
        }
      }
      
      if (add == TRUE) {
        
      }
      
      lines(data, col = color, lwd = lwd, lty = lty, type = type, 
            pch = pch)
    }  ##EndOf::plot_line()
    
    if (length(data) > 1) {
      col <- colororder
    }
    
    # add sprectum lines (either measured data or splines)
    for (i in 1:length(data)) {
      
      # INPUT DATA
      if (add == TRUE || c(smooth.spline == FALSE && difference == 
                             FALSE)) {
        plot_line(cbind(data[[i]]$x, data[[i]]$y), 
                  col[i], "data")
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
    if (length(data) > 1 && legend == TRUE) {
      legend(legend.pos, legend = colnames, lty = 1, lwd = 3, col = col, 
             cex = 0.8, ncol = 2)
    }
    
    ## ==========================================================================##
    ## AUTOMATIC PEAK FINDING
    ## ==========================================================================##
    if (find.peaks == TRUE && length(data) == 1) {
      
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
  }
  ## ==========================================================================##
  ## RETURN VALUES
  ## ==========================================================================##
  
  # create data frame for output
  if (find.peaks == FALSE || length(data) > 1) {
    all.peaks <- NULL
  }
  if (find.peaks == TRUE && length(data) == 1) {
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
  
  # plot calculus data
  plot.par <- list(ylim = ylim, xlim = xlim, ylim.integrand = if (integrate == 
                                                                    TRUE) {
    c(-y, y)
  } else {
    NULL
  })
  
  # return output data.frame and nls.object fit
  invisible(list(data = data, derivative = deriv_one, integrand = integrand, 
                 splines = spline, diff.splines = deriv_one.spline, auto.peaks = all.peaks, 
                 plot.par = plot.par))
}