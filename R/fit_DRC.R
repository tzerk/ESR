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
#' 
#' @param input.data \code{\link{data.frame}} (\bold{required}): data frame
#' with two columns for x=Dose, y=ESR.intensity. Optional: a third column
#' containing individual ESR intensity errors can be provided.
#' 
#' @param model \code{\link{character}} (with default): Currently implemented
#' models: single-saturating exponential (\code{"EXP"}), linear (\code{"LIN"}).
#' 
#' @param fit.weights \code{\link{logical}} (with default): option whether the
#' fitting is done with equal weights (\code{'equal'}) or weights proportional
#' to intensity (\code{'prop'}). If individual ESR intensity errors are
#' provided, these can be used as weights by using \code{'error'}.
#' 
#' @param algorithm \code{\link{character}} (with default): specify the applied
#' algorithm used when fitting non-linear models. If \code{'port'} the 'nl2sol'
#' algorithm from the Port library is used. The default (\code{'LM'}) uses
#' the implementation of the Levenberg-Marquardt algorithm from
#' the \code{'minpack.lm'} package.
#' 
#' @param bootstrap \code{\link{logical}} (with default): generate replicates
#' of the input data for a nonparametric bootstrap.
#' 
#' @param bootstrap.replicates \code{\link{numeric}} (with default): amount of
#' bootstrap replicates.
#' 
#' @param plot \code{\link{logical}} (with default): plot output
#' (\code{TRUE/FALSE}).
#' 
#' @param \dots further arguments
#' 
#' 
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
#' ##load example data
#' data(ExampleData.De, envir = environment())
#' 
#' ##plot ESR sprectrum and peaks
#' fit_DRC(input.data = ExampleData.De, fit.weights = 'prop')
#' 
#' @export fit_DRC
fit_DRC <- function(input.data, model = "EXP", fit.weights = "equal", 
                    algorithm = "LM", bootstrap = FALSE, 
                    bootstrap.replicates = 999, plot = FALSE, 
                    ...) {
  
  
  ## ==========================================================================##
  ## CONSISTENCY CHECK OF INPUT DATA
  ## ==========================================================================##
  
  ## check if provided data fulfill the requirements
  
  # 1. check if input.data is data.frame
  if (!is.data.frame(input.data)) {
    stop("\n [fit_DRC] >> input.data has to be of type data.fame!")
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
  ## CHECK ... ARGUMENTS
  ## ==========================================================================##
  
  extraArgs <- list(...)
  
  verbose <- if ("verbose" %in% names(extraArgs)) {
    extraArgs$verbose
  } else {
    TRUE
  }
  
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
  EXP <- y ~ a * (1 - exp(-(x + c)/b))
  
  
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
    
    fit <- try(nls(EXP, 
                   data = input.data, 
                   start = c(a = a, b = b, c = c), 
                   trace = FALSE, algorithm = "port", 
                   lower = c(a = 0, b > 0, c = 0),
                   nls.control(maxiter = 100, 
                               warnOnly = FALSE, 
                               minFactor = 1/2048)  #increase max. iterations
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
    nls.bs.res <- try(nls(EXP, data = x, 
                          start = c(a = a, b = b, c = c), trace = FALSE, weights = weights, 
                          algorithm = "port", nls.control(maxiter = 500)), silent = TRUE)  #end nls
  }
  #
  nlsLM.fit <- function(x) {
    nls.bs.res <- minpack.lm::nlsLM(EXP, data = x,
                                    start = c(a = a, b = b, c = c), trace = FALSE, weights = weights, 
                                    control = minpack.lm::nls.lm.control(maxiter = 500))
  }
  
  if (model == "EXP") {
    if (algorithm == "port") fit <- nls.fit(input.data)
    else if (algorithm == "LM") fit <- nlsLM.fit(input.data)
    
    # retrieve fitting results
    nls.par <- try(summary(fit)$parameters, silent = TRUE)
    
    if (class(fit) == "try-error") {
      nls.par <- NA
    }
  }
  
  if (model == "LIN") {
    fit <- lm(input.data[ ,2] ~ input.data[ ,1])
    lm.coef <- as.numeric(coef(fit))
    De.solve <- round(-lm.coef[1] / lm.coef[2], 2)
    CI <- confint(fit, level = 0.95)
    De.solve.error <- round(as.numeric(dist(CI[2, ]) / 2), 2)
    d0 <- NA
    d0.error <- NA
  }
  
  ## ==========================================================================##
  ## EQUIVALENT DOSE CALCULATION
  ## ==========================================================================##
  
  if (model == "EXP") {
    # calculate De by solving SSE for x
    De.solve <- round(-c - b * log(1 - 0/a), digits = 2)
    
    # obtain DE error and characteristic saturation dose D0
    if (class(fit) != "try-error") {
      d0 <- round(nls.par["b", "Estimate"], 0)
      CI <- confint(fit, level = 0.67)
      De.solve.error <- round(as.numeric(dist(CI["c", ]) / 2), 2)
      d0.error <- round(as.numeric(dist(CI["b", ]) / 2), 2)
    } else {
      De.solve.error <- NA
      d0 <- NA
      d0.error <- NA
      Rsqr <- NA
    }
  }
  
  ## ==========================================================================##
  ## DETERMINE FIT QUALITY
  ## ==========================================================================##
  if (class(fit) != "try-error") {
    RSS.p <- sum(residuals(fit)^2)
    TSS <- sum((input.data[, 2] - mean(input.data[, 2]))^2)
    Rsqr <- 1-RSS.p/TSS
  }
  
  ## ==========================================================================##
  ## BOOTSTRAP
  ## ==========================================================================##
  if (bootstrap) {
    
    nls.bs <- function(bs.data, i, FUN) {
      
      d <- bs.data[i, ]
      
      if (model == "EXP")  {
        nls.bs.fit <- nls.fit(d)
        nls.bs.par <- try(summary(nls.bs.fit)$parameters, silent = TRUE)
        if (class(nls.bs.fit) == "try-error") {
          de <- NA
        } else {
          de <- nls.bs.par["c", "Estimate"]
        }
      }
      if (model == "LIN") { 
        lm.bs.fit <- lm(d[, 2] ~ d[ ,1])
        lm.coef <- as.numeric(coef(lm.bs.fit))
        de <- abs(round(-lm.coef[1] / lm.coef[2], 2))
      }
      return(de)
    }
    
    nls.bs.res <- boot(input.data, nls.bs, R = bootstrap.replicates, 
                       parallel = "multicore")
    
    
    nls.bs.des <- na.exclude(nls.bs.res$t)
    
    nls.bs.mean <- round(mean(nls.bs.des), 2)
    nls.bs.median <- round(median(nls.bs.des), 2)
    nls.bs.sd <- round(sd(nls.bs.des), 2)
    
  }
  
  ## ==========================================================================##
  ## CONSOLE OUTPUT
  ## ==========================================================================##
  
  if (verbose) 
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
    cat("\n [fit_DRC]")
    cat(paste("\n\n ---------------------------------------------------------"))
    cat(paste("\n number of datapoints         :", length(input.data$x)))
    cat(paste("\n maximum additive dose (Gy)   :", max(input.data$x)))
    cat(paste("\n Error weighting              :", weight.method))
    cat(paste("\n Satuation dose D0 (Gy)       :", d0))
    cat(paste("\n Satuation dose D0 error (Gy) :", d0.error))
    cat(paste("\n De (Gy)                      :", abs(De.solve)))
    cat(paste("\n De error (Gy)                :", De.solve.error))
    cat(paste("\n R^2                          :", round(Rsqr, 4)))
    cat(paste("\n ---------------------------------------------------------\n"))
    
    # results of bootstrapping
    if (bootstrap) {
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
  ## RETURN VALUES
  ## ==========================================================================##
  
  # create data frame for output
  if (!bootstrap) {
    nls.bs.res <- NA
  }
  
  output <- try(data.frame(De = ifelse(bootstrap, nls.bs.median, abs(De.solve)), 
                           De.Error = ifelse(bootstrap, nls.bs.sd, De.solve.error), 
                           d0 = d0,
                           d0.error = d0.error,
                           n = length(input.data$x), 
                           weights = fit.weights,
                           model = model,
                           rsquared = Rsqr),
                silent = TRUE)
  
  results <- list(data = input.data,
                  output = output, 
                  fit = fit, 
                  bootstrap = nls.bs.res)
  
  if (plot) try(plot_DRC(results, ...))
  
  # return output data.frame and nls.object fit
  invisible(results)
}