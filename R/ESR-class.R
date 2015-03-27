#' ESR class objects
#' 
#' Objects of class \code{ESR}, \code{ESR.Spectrum} are \link{R6} objects and hence are 
#' usable in a more typical object-oriented language than R. For the user the main difference 
#' to R's internal \code{S3} and \code{S4} object implementation is that methods for \link{R6} 
#' objects belong to the object itself. See details.
#' 
#' @docType class
#' @format \link{R6Class} generator objects
#' @keywords data
#' @author Christoph Burow, University of Cologne (Germany)
#' @examples
#' 
#' ## create an empty ESR base class object
#' x <- ESR$new()
#' 
#' ## create an object of class ESR.Spectrum
#' ## and fill random data
#' x <- ESR.Spectrum$new()
#' x$set_data(data.frame(seq(3350, 3450, length.out = 1024), 
#'                                 runif(1024, -1000, 1000)))
#' 
#' # plot the random data
#' plot(x)
#' 
#' \dontrun{
#' ## public set methods
#' x$set_par()
#' x$set_data()
#' x$set_origin()
#' }
#' 
#' ## public get methods
#' x$get_diff()
#' x$get_integral()
#' x$get_spline()
#' \dontrun{
#' x$get_gvalues()
#' }
#' @name ESR-class
NULL

#' @rdname ESR-class
#' @export
ESR <- R6Class("ESR",
               public = list(
                 originator = NA,
                 data = NA,
                 initialize = function(originator, data) {
                   if (!missing(originator)) 
                     self$originator <- originator
                   else 
                     self$originator <- "ESR$new()"
                   if (!missing(data))
                     self$data <- data
                   else 
                     self$data <- setnames(data.frame(matrix(ncol = 2, nrow = 1024)), c("x","y"))
                 },
                 set_origin = function(s) {
                   self$originator <- s
                 },
                 set_data = function(x) {
                   if (!is.null(x) && ncol(x) == 2) {
                     self$data <- setnames(x, c("x", "y"))
                     attr(self$data, "spectrum") <- "spectrum"
                   }
                   else
                     message("Invalid data format! Data was not saved.")
                 }
               )
)

#' @rdname ESR-class
#' @include methods.R
#' @export
ESR.Spectrum <- R6Class("ESR.Spectrum",
                        inherit = ESR,
                        public = list(
                          parameter = NA,
                          type = "Spectrum",
                          initialize = function() {
                            super$initialize(originator = "ESR.Spectrum$new()")
                          },
                          set_par = function(p) {
                            self$parameter <- p
                          },
                          get_gvalues = function(v, H, x = self$data) {
                            if (missing(v)) {
                              if(typeof(self$parameter) != "list")
                                return(message("Missing microwave frequency (GHz)!"))
                              if ("MF" %in% self$parameter[ ,1])
                                v <- as.numeric(self$parameter[which(self$parameter[,1]=="MF"), 2])
                              if ("MWFQ" %in% self$parameter[,1])
                                v <- as.numeric(self$parameter[which(self$parameter[,1]=="MWFQ"), 2])
                            }
                            if (missing(H)) {
                              H <- self$data$x
                              if (any(H<=2048))
                                return(message("Invalid or no magnetic field data!"))
                            }
                            invisible(gval(v, H, x))
                          },
                          get_diff = function(x = self$data, order = 1, ...) {
                            invisible(differential(x, order, ...))
                          },
                          get_integral = function(x = self$data) {
                            invisible(integrate(x))
                          },
                          get_spline = function(x = self$data, ...) {
                            invisible(s.spline(x, ...))
                          },
                          get_peaks = function(x = self$data, interval, th = 10) {
                            invisible(peaks(x, interval, th))
                          }
                        )
)