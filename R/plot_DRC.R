#' Plot a dose response curve
#' 
#' This functions plots the dose response curve of an object that
#' was created with the fit_DRC() function
#'
#'
#' @param object \code{\link{list}} (required): a results object returned from
#' the \code{fit_DRC} function
#' @param ... further graphical parameters to be passed to \code{plot}
#'
#' @return Returns a plot
#' @export
#'
#' @examples
#' 
#' #' ##load example data
#' data(ExampleData.De, envir = environment())
#' obj <- fit_DRC(input.data = ExampleData.De, fit.weights = 'prop')
#' plot_DRC(obj)
#' 
plot_DRC <- function(object, ...) {
  
  ## ==========================================================================##
  ## ADDITIONAL ARGUMENTS
  ## ==========================================================================##
  
  settings <- list(xlim = c(-object$output$De + -object$output$De * 0.5, 
                            (max(object$data$x) + max(object$data$x) * 0.1)),
                   ylim = c(0, max(object$data$y) + max(object$data$y) * 0.1),
                   main = "Dose response curve",
                   xlab = "Dose (Gy)",
                   ylab = "ESR intensity (a.u.)",
                   pch = 21,
                   col = "black")
  
  settings <- modifyList(settings, list(...))
  
  
  
  ## ==========================================================================##
  ## PLOTTING
  ## ==========================================================================##
  
  # save previous plot parameter and set new ones
  .pardefault <- par(no.readonly = TRUE)
  
  if (inherits(object$bootstrap, "boot")) {
    
    jack.after.boot(object$bootstrap, index = 1)
    
    layout(matrix(c(1, 3, 2, 3), 2, 2))
    par(oma = c(0, 4, 0, 4))
    
    hist(object$bootstrap$t, breaks = "FD", freq = FALSE, main = "Histogram", 
         xlab = settings$xlab, col = "gray80", border = "gray66")
    
    lines(density(na.exclude(object$bootstrap$t)), col = "black", 
          lwd = 1.5)
    
    # plot vertical dashed line at t0
    abline(v = c(object$bootstrap$t0, mean(na.exclude(object$bootstrap$t)), 
                 median(na.exclude(object$bootstrap$t))), col = "black", lwd = 1.5, 
           lty = c(2, 3, 4))
    
    
    legend("topright", legend = c("KDE", "t0", "mean t*", "median t*"), 
           lty = c(1, 2, 3, 4), bty = "n")
    
    qqnorm(object$bootstrap$t)
    qqline(object$bootstrap$t, lty = 2)
  }
  
  # plot only if fitting was successful
  if (class(object$fit) != "try-error") {
    
    # set plot layout
    par(cex = 1, xaxs = "i", yaxs = "i")
    
    # plot input data: ESR intensity vs dose (Gy)
    plot(object$data[, 1], object$data[, 2], main = settings$main, ylim = settings$ylim, 
         xlim = settings$xlim, ylab = settings$ylab, xlab = settings$xlab,
         pch = settings$pch, bty = "n", col = settings$col, lab = c(10, 5, 7))
    
    # insert subtitle with information on De, n and fit method
    mtext(substitute(D[e] == De, list(De = paste(abs(-object$output$De), 
                                                 "+/-", object$output$De.Error, "Gy", " | n =", length(object$data$x), 
                                                 " | fit: SSE"))), side = 3, line = 0.5, cex = 0.7)
    
    # plot vertical dashed line at x=0 par(new=TRUE)
    v.ylim <- pretty(object$data[, 2])
    points(x = c(0, 0), y = c(0, v.ylim[length(v.ylim)]), 
           col = settings$col, type = "l", lty = "1111AA")
    
    # plot fitted curve through data
    # curve(EXP, lwd = 1.5, col = "black", add = TRUE, lty = 1)
    newX <- seq(-object$output$De, max(object$data[ ,1]), length.out = 1000)
    newY <- predict(object$fit, list(x = newX))
    lines(newX, newY)
    
    ## restore previous plot parameters
    par(.pardefault)
    
  }  #:EndOf plot
}