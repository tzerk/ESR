#' Fit a dose response curve to ESR data and create a DE-DEmax Plot after
#' Schellmann & Radtke (2001)
#' 
#' This function repeatedly fits a single saturating exponential to ESR data
#' and calculates the De while removing the highest datapoint datapoint after
#' each iteration. It then creates a DE-DEmax-Plot after Schellmann & Radtke
#' (2001) where the DE is plotted against the number of datapoints used for
#' fitting.
#' 
#' \bold{Fitting methods} \cr\cr For fitting of the dose response curve the
#' \code{nls} function with the \code{port} algorithm is used. A single
#' saturating exponential in the form of \deqn{y = a*(1-exp(-(x+c)/b))} is
#' fitted to the data. Parameters b and c are approximated by a linear fit
#' using \code{lm}.\cr\cr \bold{Fit weighting} \cr\cr If \code{'equal'} all
#' datapoints are weighted equally. For \code{'prop'} the datapoints are
#' weighted proportionally by their respective ESR intensity: \deqn{fit.weights
#' = 1/intensity/(sum(1/intensity))} If individual errors on ESR intensity are
#' available, choosing \code{'error'} enables weighting in the form of:
#' \deqn{fit.weights = 1/error/(sum(1/error))}
#' 
#' @param input.data \code{\link{data.frame}} (\bold{required}): data frame
#' with two columns for x=Dose, y=ESR.intensity. Optional: a third column
#' containing individual ESR intensity errors can be provided.
#' 
#' @param min.DosePoints \code{\link{integer}} (with default): minimum number
#' of datapoints used for fitting the single saturating exponential.
#' 
#' @param fit.weights \code{\link{logical}} (with default): option whether the
#' fitting is done with equal weights (\code{'equal'}) or weights proportional
#' to intensity (\code{'prop'}). If individual ESR intensity errors are
#' provided, these can be used as weights by using \code{'error'}.
#' 
#' @param model \code{\link{character}} (with default): Currently implemented
#' models: single-saturating exponential (\code{"EXP"}), linear (\code{"LIN"}).
#' 
#' @param mean.natural \code{\link{logical}} (with default): If there are repeated
#' measurements of the natural signal should the mean amplitude be used for
#' fitting?
#' 
#' @param show.grid \code{\link{logical}} (with default): show horizontal grid
#' lines in plots (\code{TRUE/FALSE})
#' 
#' @param output.console \code{\link{logical}} (with default): plot console
#' output (\code{TRUE/FALSE})
#' 
#' @param ... further arguments passed to \code{\link{plot}} and \code{\link{par}}.
#' 
#' @return Returns terminal output and a plot. In addition, a list is returned
#' containing the following elements:
#' 
#' \item{output}{data frame containing the De (datapoints n, De, De.Error,
#' max.Dose).}
#' @export
#' @note Fitting of the dose response curve using \code{\link{fit_DRC}} is
#' largely derived from the \code{plot_GrowthCurve} function of the
#' 'Luminescence' package by Kreutzer et al. (2012).\cr\cr \bold{Fitting
#' methods} \cr Currently, only fitting of a single saturating exponential is
#' supported. Fitting of two exponentials or an exponential with a linear term
#' may be implemented in a future release.
#' 
#' @author Christoph Burow, University of Cologne (Germany) Who wrote it
#' 
#' @seealso \code{\link{plot}}, \code{\link{nls}}, \code{\link{lm}},
#' \code{\link{fit_DRC}}
#' 
#' @references Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of
#' equivalent dose and error calculation and display in OSL dating: An overview
#' and some recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr\cr
#' Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M.,
#' 2012. Introducing an R package for luminescence dating analysis. Ancient TL,
#' 30 (1), pp 1-8.
#' 
#' @examples
#' 
#' \dontrun{
#' ##load example data
#' data(ExampleData.De, envir = environment())
#' 
#' #calculate and plot De-Dmax Plateau
#' calc_DePlateau(input.data = ExampleData.De,
#'                min.DosePoints = 5, 
#'                fit.weights = 'prop', 
#'                show.grid = TRUE, 
#'                output.console = FALSE)
#' }
#' 
#' @export calc_DePlateau
calc_DePlateau <- function(input.data, min.DosePoints = 5, fit.weights = "equal", model = "EXP",
                           mean.natural = FALSE, show.grid = TRUE, output.console = TRUE, ...) {
  
  ## ==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ## ==========================================================================##
  
  ## check if provided data fulfill the requirements
  
  # 1. check if input.data is a data.frame
  if (is.data.frame(input.data) == FALSE) {
    stop("\n [calc_DePlateau] >> input.data has to be of type data.fame!")
  }
  
  # 2. verify if data frame has two or three columns
  if (length(input.data) < 2 | length(input.data) > 3) {
    cat(paste("Please provide a data frame with two or three columns", 
              "(x=Dose, y=ESR.intensity, z=ESR.intensity.Error)."), fill = FALSE)
    stop(domain = NA)
  }
  
  #### satisfy R CMD check Notes
  x <- NULL
  
  ## ==========================================================================##
  ## PREPARE INPUT/OUTPUT DATA
  ## ==========================================================================##
  
  # label input.data data frame for easier addressing
  if (length(input.data) == 3) {
    colnames(input.data) <- c("x", "y", "y.Err")
  } else {
    colnames(input.data) <- c("x", "y")
  }
  
  # create empty matrix as container for fitting results
  De.storage <- matrix(data = NA, nrow = length(input.data[, 1]) - min.DosePoints + 
                         1, ncol = 2)
  # create matrix with one column containing the number of used
  # datapoints for fitting
  IDs <- matrix(data = length(input.data[, 1]):min.DosePoints, nrow = length(input.data[, 
                                                                                        1]) - min.DosePoints + 1, ncol = 1)
  
  # retrieve dose information from input.data only for those datapoints
  # used for for fitting
  dose <- input.data$x[length(input.data$x):min.DosePoints]
  
  
  ## ==========================================================================##
  ## DE CALCULATION (FITTING LOOP)
  ## ==========================================================================##
  
  # set progressbar
  if (output.console) {
    pb <- txtProgressBar(min = 0, max = length(input.data[, 1]) - min.DosePoints, 
                         char = "=", style = 3)
  }
  
  # repeatedly call fit_DRC() while removing highest datapoint after each
  # run
  for (i in 1:c(length(input.data[, 1]) - min.DosePoints)) {
    
    # first run with all datapoints
    if (i == 1) {
      
      # temporary container for input.data
      input.temp <- input.data[1:length(input.data[, 1]), ]

      # call fit_DRC() and save De and De.Error to De container
      drc <- tryCatch(fit_DRC(input.data = input.temp, 
                               fit.weights = fit.weights,
                               mean.natural = mean.natural,
                               model = model,
                               plot = FALSE, bootstrap = FALSE, 
                               verbose = FALSE),
                      error = function(e) { NULL }
      )
      
      if (is.null(drc))
        De.storage[i, ] <- matrix(c(NA, NA), ncol = 2)
      else
        De.storage[i, ] <- as.matrix(drc$output[1, 1:2])
      
      # call fit_DRC() with all datapoints and retrieve nls fit object for
      # the dose response curve plot
      if (!is.null(drc))
        fit <- drc$fit
        
      # update progressbar
      if (output.console)
        setTxtProgressBar(pb, i)
    }
    
    # see above: Fit loop for input.data until min.DosePoints is reached
    input.temp <- input.data[1:c(length(input.data[, 1]) - i), ]
    
    drc <- tryCatch(fit_DRC(input.data = input.temp, 
                            fit.weights = fit.weights,
                            mean.natural = mean.natural,
                            model = model,
                            plot = FALSE, bootstrap = FALSE, 
                            verbose = FALSE),
                    error = function(e) { NULL }
    )
    
    if (is.null(drc))
      De.storage[i + 1, ] <- matrix(c(NA, NA), ncol = 2)
    else
      De.storage[i + 1, ] <- as.matrix(drc$output[1, 1:2])

    # update progressbar
    if (output.console)
      setTxtProgressBar(pb, i)
  }  #::EndOf.fit_loop
  
  # close progressbar
  if (output.console)
    close(pb)
  
  # append dose and number of datapoints to De results
  De.storage <- as.data.frame(cbind(IDs, De.storage, dose))
  colnames(De.storage) <- c("n", "De", "De.Error", "max.Dose")
  
  # if no De error could be calculated, also set the De as NA
  De.storage[which(is.na(De.storage$De.Error) == TRUE), 2] <- NA
  
  
  ## ==========================================================================##
  ## PLOTTING
  ## ==========================================================================##
  
  # save previous plot parameter and set new ones
  .pardefault <- par(no.readonly = TRUE)
  
  # create device layout
  layout(matrix(c(1, 2), 2, 1, byrow = TRUE), respect = TRUE)
  
  # general plot parameters
  settings <- list(cex = 0.8, xaxs = "i", yaxs = "i", mfrow = c(2, 1), line = 0,
                  mar = c(5, 4, 4, 2) + 0.1)
  settings <- modifyList(settings, list(...))
  
  par(cex = settings$cex, 
      xaxs = settings$xaxs, 
      yaxs = settings$yaxs, 
      mfrow = settings$mfrow,
      mar = settings$mar)
  
  
  ## ----------------------------------------------------------------------------
  ## PLOT 1: DOSE RESPONSE CURVE
  
  # prepare y-axis label
  y.label <- c(paste("ESR intensity [a.u.]", sep = ""))
  
  plot(input.data$x, input.data$y, main = expression("Dose response curve"), 
       xlim = c(0, (max(input.data$x) + max(input.data$x) * 0.1)), pch = 19, 
       bty = "l", xpd = TRUE, xlab = expression("additive dose (Gy)"), 
       ylab = y.label)
  
  # add subtitle with De, De error, number of datapoints and fit method
  mtext(substitute(D[e] == De, list(De = paste(De.storage[1, 2], "+/-", 
                                               De.storage[1, 3], "Gy", " | n =", length(input.data$x), " | fit: SSE"))), 
        side = 3, line = settings$line, cex = settings$cex)
  
  # add fitted single saturating exponential
  fit.functionEXP <- function(a, b, c, x) {
    a * (1 - exp(-(x + c)/b))
  }
  a <- coef(fit)["a"]  #obtain parameters from nls fit object from first run
  b <- coef(fit)["b"]
  c <- coef(fit)["c"]
  curve(fit.functionEXP(a, b, c, x), lwd = 1.5, col = "red", add = TRUE)
  
  # add gridlines if desired
  if (show.grid == TRUE) {
    grid(nx = NA, ny = NULL)
    # workaround to avoid overplotting of a dashed grid line on the x-axis
    abline(h = 0)
  }
  
  
  ## ----------------------------------------------------------------------------
  ## PLOT 2: De-Demax Plot
  
  plot(De.storage$max.Dose, De.storage$De, main = expression(paste("D"[e],"-D"[max], "-Plot")), 
       pch = 19, bty = "l", xpd = TRUE, 
       xlim = c(0,max(input.data$x) + max(input.data$x) * 0.1), 
       ylim = c(0, max(na.exclude(De.storage$De + 
                                                                                                                                                                                                              De.storage$De.Error)) + max(na.exclude(De.storage$De + De.storage$De.Error)) * 
                                                                                                                                                                                            0.25), xlab = expression(paste("maximum additive dose (Gy)")), 
       ylab = expression(paste("D"[e], " (Gy)")))
  
  # add line between De values
  lines(De.storage$max.Dose, De.storage$De, lty = "solid", col = "red")
  
  # plot error bars
  segments(x0 = De.storage$max.Dose, y0 = De.storage$De - De.storage$De.Error, 
           y1 = De.storage$De + De.storage$De.Error)
  
  # plot error bars; workaround for vertical lines at the end of the
  # error bars
  epsilon <- max(na.exclude(De.storage$max.Dose)) * 0.02
  segments(De.storage$max.Dose - epsilon, De.storage$De - De.storage$De.Error, 
           De.storage$max.Dose + epsilon, De.storage$De - De.storage$De.Error)
  segments(De.storage$max.Dose - epsilon, De.storage$De + De.storage$De.Error, 
           De.storage$max.Dose + epsilon, De.storage$De + De.storage$De.Error)
  
  # add gridlines if desired
  if (show.grid == TRUE) {
    grid(nx = NA, ny = NULL)
    # workaround to avoid overplotting of a dashed grid line on the x-axis
    abline(h = 0)
  }
  
  # add number of maximum aliquots used for De calculation above error
  # bars
  text(De.storage$max.Dose, De.storage$De + De.storage$De.Error, labels = De.storage$n, 
       pos = 3, cex = settings$cex, xpd = TRUE)
  
  # add subtitle
  legend.label <- paste("De calculated from datapoint 1 to ", min.DosePoints, 
                        "..", max(De.storage$n), sep = "")
  mtext(legend.label, side = 3, line = settings$line, cex = settings$cex)
  
  ## restore previous plot parameters
  par(.pardefault)
  
  
  ## ==========================================================================##
  ## CONSOLE OUTPUT
  ## ==========================================================================##
  
  if (output.console) {
    
    # save weighting method in a new variable for nicer output
    if (fit.weights == "equal") {
      weight.method <- "equal weights"
    }
    if (fit.weights == "prop") {
      weight.method <- "proportional to intensity"
    }
    if (fit.weights == "error") {
      weight.method <- "individual ESR intensity error"
    }
    
    # determine number of successful and failed fits
    fit.suc <- sum(!is.na(De.storage$De))
    fit.fail <- sum(is.na(De.storage$De))
    
    # final console output
    cat("\n [calc_DePlateau]")
    cat(paste("\n\n ---------------------------------------------------------"))
    cat(paste("\n maximum number of datapoints:", length(input.data$x)))
    cat(paste("\n maximum additive dose (Gy)  :", max(input.data$x)))
    cat(paste("\n Error weighting             :", weight.method))
    cat(paste("\n Number of fits successful   :", fit.suc))
    cat(paste("\n Number of fits failed       :", fit.fail))
    cat(paste("\n ---------------------------------------------------------\n"))
  }
  
  
  ## ==========================================================================##
  ## RETURN VALUES
  ## ==========================================================================##
  
  invisible(list(output = De.storage))
}