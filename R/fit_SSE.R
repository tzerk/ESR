#' Fit and plot a dose response curve for ESR data (ESR intensity against dose)
#' 
#' A dose response curve is produced for Electron Spin Resonance measurements
#' using an additive dose protocol.
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
#' \deqn{fit.weights = 1/error/(sum(1/error))} \cr\cr \bold{Bootstrap} \cr\cr
#' If \code{bootstrap = TRUE} the function generates
#' \code{bootstrap.replicates} replicates of the input data for nonparametric
#' ordinary bootstrapping (resampling with replacement). For each bootstrap
#' sample a dose response curve is constructed by fitting the chosen function
#' and the equivalent dose is calculated. The distribution of bootstrapping
#' results is shown in a histogram, while a \code{\link{qqnorm}} plot is
#' generated to give indications for (non-)normal distribution of the data.
#' 
#' @param input.data \code{\link{data.frame}} (\bold{required}): data frame
#' with two columns for x=Dose, y=ESR.intensity. Optional: a third column
#' containing individual ESR intensity errors can be provided.
#' @param fit.weights \code{\link{logical}} (with default): option whether the
#' fitting is done with equal weights (\code{'equal'}) or weights proportional
#' to intensity (\code{'prop'}). If individual ESR intensity errors are
#' provided, these can be used as weights by using \code{'error'}.
#' @param bootstrap \code{\link{logical}} (with default): generate replicates
#' of the input data for a nonparametric bootstrap.
#' @param bootstrap.replicates \code{\link{numeric}} (with default): amount of
#' bootstrap replicates.
#' @param output.console \code{\link{logical}} (with default): plot console
#' output (\code{TRUE/FALSE}).
#' @param output.plot \code{\link{logical}} (with default): plot output
#' (\code{TRUE/FALSE}).
#' @param \dots further arguments
#' @return Returns terminal output and a plot. In addition, a list is returned
#' containing the following elements:
#' 
#' \item{output}{data frame containing the De (De, De Error, D01 value).}
#' \item{fit}{\code{nls} object containing the fit parameters}
#' 
#' @export
#' @note This function is largely derived from the \code{plot_GrowthCurve}
#' function of the 'Luminescence' package by Kreutzer et al. (2012).\cr\cr
#' \bold{Fitting methods} \cr Currently, only fitting of a single saturating
#' exponential is supported. Fitting of two exponentials or an exponential with
#' a linear term may be implemented in a future release. \cr\cr
#' \bold{Bootstrap} \cr\cr While a higher number of replicates (bootstrap
#' samples) is desirable, it is also increasingly computationally intensive.
#' @author Christoph Burow, University of Cologne (Germany) Who wrote it
#' @seealso \code{\link{plot}}, \code{\link{nls}}, \code{\link{lm}},
#' \code{link{boot}}
#' @references Efron, B. & Tibshirani, R., 1993. An Introduction to the
#' Bootstrap.  Chapman & Hall. \cr\cr Davison, A.C. & Hinkley, D.V., 1997.
#' Bootstrap Methods and Their Application. Cambridge University Press. \cr\cr
#' Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent
#' dose and error calculation and display in OSL dating: An overview and some
#' recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr\cr Kreutzer,
#' S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M., 2012.
#' Introducing an R package for luminescence dating analysis. Ancient TL, 30
#' (1), pp 1-8.
#' @examples
#' 
#' 
#' ##load example data
#' data(ExampleData.De, envir = environment())
#' 
#' ##plot ESR sprectrum and peaks
#' fit_SSE(input.data = ExampleData.De, fit.weights = 'prop')
#' 
#' 
#' @export fit_SSE
fit_SSE <- function(input.data, fit.weights = "equal", bootstrap = FALSE, 
                    bootstrap.replicates = 999, output.console = TRUE, output.plot = TRUE, 
                    ...) {
  
  
  ## ==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ## ==========================================================================##
  
  ## check if provided data fulfill the requirements
  
  # 1. check if input.data is data.frame
  if (is.data.frame(input.data) == FALSE) {
    stop("\n [fit_SSE] >> input.data has to be of type data.fame!")
  }
  
  # 2. very if data frame has two or three columns
  if (length(input.data) < 2 | length(input.data) > 3) {
    cat(paste("Please provide a data frame with at two or three columns", 
              "(x=Dose, y=ESR.intensity, z=ESR.intensity.Error)."), fill = FALSE)
    stop(domain = NA)
  }
  
  
  #### satisfy R CMD check Notes
  x <- NULL
  
  ## ==========================================================================##
  ## SET FUNCTIONS FOR FITTING & PREPARE INPUT DATA
  ## ==========================================================================##
  
  ## label input.data data frame for easier addressing
  if (length(input.data) == 2) {
    colnames(input.data) <- c("x", "y")
  }
  if (length(input.data) == 3) {
    colnames(input.data) <- c("x", "y", "y.Err")
  }
  
  ## set EXP function for fitting
  fit.functionEXP <- function(a, b, c, x) {
    a * (1 - exp(-(x + c)/b))
  }
  
  
  ## ==========================================================================##
  ## START PARAMETER ESTIMATION
  ## ==========================================================================##
  
  ## general setting of start parameters for fitting
  
  # a - estimation for a take the maxium of the y-values
  a <- max(input.data[, 2])
  
  # b - get start parameters from a linear fit of the log(y) data
  fit.lm <- lm(log(input.data$y) ~ input.data$x)
  b <- as.numeric(1/fit.lm$coefficients[2])
  
  # c - get start parameters from a linear fit - offset on x-axis
  fit.lm <- lm(input.data$y ~ input.data$x)
  c <- as.numeric(abs(fit.lm$coefficients[1]/fit.lm$coefficients[2]))
  
  ## --------------------------------------------------------------------------##
  ## to be a little bit more flexible the start parameters varries within
  ## a normal distribution
  
  # draw 50 start values from a normal distribution a start values
  a.MC <- rnorm(50, mean = a, sd = a/100)
  b.MC <- rnorm(50, mean = b, sd = b/100)
  c.MC <- rnorm(50, mean = c, sd = c/100)
  
  # set start vector (to avoid errors witin the loop)
  a.start <- NA
  b.start <- NA
  c.start <- NA
  
  
  ## try to create some start parameters from the input values to make the
  ## fitting more stable
  for (i in 1:50) {
    
    a <- a.MC[i]
    b <- b.MC[i]
    c <- c.MC[i]
    
    fit <- try(nls(y ~ fit.functionEXP(a, b, c, x), data = input.data, 
                   start = c(a = a, b = b, c = c), trace = FALSE, algorithm = "port", 
                   lower = c(a = 0, b > 0, c = 0), nls.control(maxiter = 100, 
                                                               warnOnly = FALSE, minFactor = 1/2048)  #increase max. iterations
    ), silent = TRUE)
    
    if (class(fit) != "try-error") {
      # get parameters out of it
      parameters <- (coef(fit))
      b.start[i] <- as.vector((parameters["b"]))
      a.start[i] <- as.vector((parameters["a"]))
      c.start[i] <- as.vector((parameters["c"]))
    }
  }
  
  # use mean as start parameters for the final fitting
  a <- median(na.exclude(a.start))
  b <- median(na.exclude(b.start))
  c <- median(na.exclude(c.start))
  
  
  ## ==========================================================================##
  ## CALCULATE FITTING WEIGHTS
  ## ==========================================================================##
  
  # all datapoints are equally weighted
  if (fit.weights == "equal") {
    weights <- rep(1, length(input.data$x))
  }
  # datapoints are weighted proportionally to their ESR intensity
  if (fit.weights[1] == "prop") {
    weights <- 1/input.data$y/(sum(1/input.data$y))  # SHOULD THIS BE 1/I^2?! (Gruen & Rhodes 1991, ATL)
  }
  # EXPERIMENTAL: Weight datapoints by their true error in ESR intensity
  if (fit.weights[1] == "error") {
    weights <- 1/input.data$y.Err/(sum(1/input.data$y.Err))
  }
  
  
  ## ==========================================================================##
  ## FITTING OF THE SINGLE SATURATING EXPONENTIAL
  ## ==========================================================================##
  
  # non-linear least square fit with an SSE | a*(1-exp(-(x+c)/b))
  nls.fit <- function(x) {
    nls.bs.res <- try(nls(y ~ fit.functionEXP(a, b, c, x), data = x, 
                          start = c(a = a, b = b, c = c), trace = FALSE, weights = weights, 
                          algorithm = "port", nls.control(maxiter = 500)), silent = TRUE)  #end nls
  }
  
  fit <- nls.fit(input.data)
  
  
  # retrieve fitting results
  nls.par <- try(summary(fit)$parameters, silent = TRUE)
  
  if (class(fit) == "try-error") {
    nls.par <- NA
  }
  
  
  ## ==========================================================================##
  ## EQUIVALENT DOSE CALCULATION
  ## ==========================================================================##
  
  # calculate De by solving SSE for x
  De.solve <- round(-c - b * log(1 - 0/a), digits = 2)
  
  # obtain DE error and characteristic saturation dose D0
  if (class(fit) != "try-error") {
    De.solve.error <- round(nls.par["c", "Std. Error"], 2)
    d0 <- round(nls.par["b", "Estimate"], 0)
  } else {
    De.solve.error <- NA
    d0 <- NA
  }
  
  
  ## ==========================================================================##
  ## BOOTSTRAP
  ## ==========================================================================##
  if (bootstrap == TRUE) {
    
    nls.bs <- function(bs.data, i, FUN) {
      
      d <- bs.data[i, ]
      nls.bs.fit <- nls.fit(d)
      nls.bs.par <- try(summary(nls.bs.fit)$parameters, silent = TRUE)
      
      if (class(nls.bs.fit) == "try-error") {
        de <- NA
      } else {
        de <- nls.bs.par["c", "Estimate"]
      }
      return(de)
    }
    
    nls.bs.res <- boot(input.data, nls.bs, R = bootstrap.replicates, 
                       parallel = "multicore")
    
    
    nls.bs.des <- na.exclude(nls.bs.res$t)
    
    nls.bs.mean <- mean(nls.bs.des)
    nls.bs.median <- median(nls.bs.des)
    nls.bs.sd <- sd(nls.bs.des)
    
  }
  
  ## ==========================================================================##
  ## CONSOLE OUTPUT
  ## ==========================================================================##
  
  if (output.console == TRUE) 
  {
    
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
    
    # final console output
    cat("\n [fit_SSE]")
    cat(paste("\n\n ---------------------------------------------------------"))
    cat(paste("\n number of datapoints       :", length(input.data$x)))
    cat(paste("\n maximum additive dose (Gy) :", max(input.data$x)))
    cat(paste("\n Error weighting            :", weight.method))
    cat(paste("\n Satuation dose D0 (Gy)     :", d0))
    cat(paste("\n De (Gy)                    :", abs(De.solve)))
    cat(paste("\n De error (Gy)              :", De.solve.error))
    cat(paste("\n ---------------------------------------------------------\n"))
    
    # results of bootstrapping
    if (bootstrap == TRUE) {
      cat(paste("\n\n ------------------- BOOTSTRAP RESULTS -------------------"))
      cat(paste("\n Mean (Gy)                  :", round(nls.bs.mean, 
                                                         2)))
      cat(paste("\n Median (Gy)                :", round(nls.bs.median, 
                                                         2)))
      cat(paste("\n Standard deviation (Gy)    :", round(nls.bs.sd, 
                                                         2)))
      cat(paste("\n ---------------------------------------------------------\n"))
    }
  }  #:EndOf output.console
  
  ## ==========================================================================##
  ## CHECK ... ARGUMENTS
  ## ==========================================================================##
  
  extraArgs <- list(...)
  
  main <- if ("main" %in% names(extraArgs)) {
    extraArgs$main
  } else {
    "Dose response cuve"
  }
  
  xlim <- if ("xlim" %in% names(extraArgs)) {
    c(extraArgs$xlim[1], extraArgs$xlim[2])
  } else {
    c(De.solve + De.solve * 0.5, (max(input.data$x) + max(input.data$x) * 
                                    0.1))
  }
  
  ylim <- if ("ylim" %in% names(extraArgs)) {
    c(extraArgs$ylim[1], extraArgs$ylim[2])
  } else {
    c(0, max(input.data$y) + max(input.data$y) * 0.1)
  }
  
  xlab <- if ("xlab" %in% names(extraArgs)) {
    extraArgs$xlab
  } else {
    "Dose (Gy)"
  }
  
  ylab <- if ("ylab" %in% names(extraArgs)) {
    extraArgs$ylab
  } else {
    "ESR intensity (a.u.)"
  }
  
  pch <- if ("pch" %in% names(extraArgs)) {
    extraArgs$pch
  } else {
    21
  }
  
  col <- if ("col" %in% names(extraArgs)) {
    extraArgs$col
  } else {
    "black"
  }
  
  
  ## ==========================================================================##
  ## PLOTTING
  ## ==========================================================================##
  
  
  if (output.plot == TRUE) 
  {
    
    # save previous plot parameter and set new ones
    .pardefault <- par(no.readonly = TRUE)
    
    if (bootstrap == TRUE) {
      
      jack.after.boot(nls.bs.res, index = 1)
      
      layout(matrix(c(1, 3, 2, 3), 2, 2))
      par(oma = c(0, 4, 0, 4))
      
      hist(nls.bs.res$t, breaks = "FD", freq = FALSE, main = "Histogram", 
           xlab = "Equivalent Dose (Gy)", col = "gray80", border = "gray66")
      
      lines(density(na.exclude(nls.bs.res$t)), col = "black", 
            lwd = 1.5)
      
      # plot vertical dashed line at t0
      abline(v = c(nls.bs.res$t0, mean(na.exclude(nls.bs.res$t)), 
                   median(na.exclude(nls.bs.res$t))), col = "black", lwd = 1.5, 
             lty = c(2, 3, 4))
      
      
      legend("topright", legend = c("KDE", "t0", "mean t*", "median t*"), 
             lty = c(1, 2, 3, 4), bty = "n")
      
      qqnorm(nls.bs.res$t)
      qqline(nls.bs.res$t, lty = 2)
    }
    
    # plot only if fitting was successful
    if (class(fit) != "try-error") 
    {
      
      # set plot layout
      par(cex = 1, xaxs = "i", yaxs = "i")
      
      # plot input data: ESR intensity vs dose (Gy)
      plot(input.data[, 1], input.data[, 2], main = main, ylim = ylim, 
           xlim = xlim, ylab = ylab, xlab = xlab, pch = pch, bty = "n", 
           col = col, lab = c(10, 5, 7))
      
      # insert subtitle with information on De, n and fit method
      mtext(substitute(D[e] == De, list(De = paste(abs(De.solve), 
                                                   "+/-", De.solve.error, "Gy", " | n =", length(input.data$x), 
                                                   " | fit: SSE"))), side = 3, line = -1, cex = 0.8)
      
      # plot vertical dashed line at x=0 par(new=TRUE)
      v.ylim <- pretty(input.data[, 2])
      points(x = c(0, 0), y = c(0, v.ylim[length(v.ylim)]), 
             col = "black", type = "l", lty = "1111AA")
      
      # plot fitted curve through data
      curve(fit.functionEXP(a, b, c, x), lwd = 1.5, col = "black", 
            add = TRUE, lty = 1)
      
      ## restore previous plot parameters
      par(.pardefault)
      
    }  #:EndOf output.plot
    
    
    
  }  #::EndOf fit_SSE()
  
  ## ==========================================================================##
  ## RETURN VALUES
  ## ==========================================================================##
  
  # create data frame for output
  if (bootstrap == FALSE) {
    nls.bs.res <- NA
  }
  
  output <- try(data.frame(De = abs(De.solve), De.Error = De.solve.error, 
                           d0 = d0, n = length(input.data$x), weigths = fit.weights), silent = TRUE)
  
  # return output data.frame and nls.object fit
  invisible(list(output = output, fit = fit, bootstrap = nls.bs.res))
  
  
}