#' Run ESR shiny apps
#' 
#' A wrapper for shiny::runApp() to start interactive shiny apps
#' 
#' This is a wrapper function for \code{runApp}.
#' 
#' @param app \code{\link{character}} (required): name of the ESR app to start
#' @param ... further arguments to pass to \code{\link{runApp}}
#' @return #
#' @note #
#' @author Christoph Burow, University of Cologne (Germany)
#' @seealso \code{\link{runApp}}
#' @references In progress
#' @examples 
#' 
#' \dontrun{
#' app_ESR("converter")
#' }
#' 
#' @export app_ESR

app_ESR <- function(app, ...) {
  
  valid_apps <- c("converter", "spectrum")
  if (!app %in% valid_apps) stop(paste("invalid app name:", app))
  
  runApp(system.file(paste0("shiny/",app), package = "ESR"), ...)
  
}