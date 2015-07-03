#' ESR class objects
#' 
#' Objects of class \code{ESR}, \code{ESR.Spectrum} are \link{R6} objects and hence are 
#' usable in a more typical object-oriented language than R. For the user the main difference 
#' to R's internal \code{S3} and \code{S4} object implementation is that methods for \link{R6} 
#' objects belong to the object itself. See details.
#' 
#' @docType class
#' 
#' @slot originator \code{\link{character}} Name of the function call that
#' created this object
#' @slot data \code{\link{data.frame}} XY data of the ESR spectrum
#' 
#' @param originator \code{\link{character}} name of the function call that
#' created this object
#' @param data \code{\link{data.frame}} the ESR spectrum to be stored in the
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(originator, data)}}{ Initialize a new object of class \code{ESR}}
#'   \item{\code{set_data(data)}}{ Sets the data of the object }
#'   \item{\code{set_origin(originator)}}{ Sets the originator of the object }
#' }
#' 
#' @usage 
#' # ESR$new(originator, data)
#' # ESR$set_data(data)
#' # ESR$set_origin(originator)
#' 
#' @format \link{R6Class} generator objects
#' @keywords classes methods
#' @author Christoph Burow, University of Cologne (Germany)
#' @examples
#' 
#' ## create an empty ESR base class object
#' x <- ESR$new()
#' 
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
                 set_origin = function(originator) {
                   self$originator <- originator
                 },
                 set_data = function(data) {
                   if (!is.null(data) && ncol(data) == 2) {
                     self$data <- setnames(data, c("x", "y"))
                     attr(self$data, "spectrum") <- "spectrum"
                   }
                   else
                     message("Invalid data format! Data was not saved.")
                 }
               )
)


#' ESR.Spectrum class objects
#' 
#' Objects of class \code{ESR}, \code{ESR.Spectrum} are \link{R6} objects and hence are 
#' usable in a more typical object-oriented language than R. For the user the main difference 
#' to R's internal \code{S3} and \code{S4} object implementation is that methods for \link{R6} 
#' objects belong to the object itself. See details.
#' 
#' @docType class
#' 
#' @slot originator \code{\link{character}} Name of the function call that
#' created this object
#' @slot data \code{\link{data.frame}} XY data of the ESR spectrum
#' @slot parameter \code{\link{data.frame}} experimental parameters
#' @slot type \code{\link{character}} currently unused
#' 
#' @param x \code{\link{data.frame}} XY data of the ESR spectrum
#' @param originator \code{\link{character}} name of the function call that
#' created this object
#' @param data \code{\link{data.frame}} XY values of the ESR spectrum
#' @param par \code{\link{data.frame}} experimental parameters
#' @param order \code{\link{integer}} order of the differential
#' @param v  \code{\link{numeric}} The microwave frequency in GHz
#' @param H  \code{\link{numeric}} A vector of magnetic field values in Gauss
#' @param interval \code{\link{numeric}} a vector of length two specifying
#' the range of x-values where peaks are searched
#' @param th \code{\link{numeric}}: an integer specifying the number of neighbouring values
#' to compare each x-value to
#' @param ... Further arguments passed to method specific functions.
#' 
#' 
#' @section Methods:
#' \describe{
#'   \item{\code{new(originator, data)}}{ Initialize a new object of class \code{ESR.Spectrum}}
#'   \item{\code{set_data(data)}}{ Sets the data of the object }
#'   \item{\code{set_origin(originator)}}{ Sets the originator of the object }
#'   \item{\code{set_par(par)}}{ Sets the experimental settings of the object }
#'   \item{\code{get_diff(x, order = 1, ...)}}{ Returns the differential of the spectrum }
#'   \item{\code{get_integral(x)}}{ Returns the integrant of the spectrum }
#'   \item{\code{get_spline(x, ...)}}{ Returns a smoothed spectrum using the \code{\link{smooth.spline}} function }
#'   \item{\code{get_gvalues(v, H, x)}}{ Calculates and returns the g-values of the spectrum }
#'   \item{\code{get_peaks(x, interval, th = 10)}}{ Returns a \code{\link{vector}} of local minima and maxima in the spectrum. For details see \code{\link{find_Peaks}}. }
#' }
#' 
#' @usage 
#' # ESR.Spectrum$new(originator, data)
#' # ESR.Spectrum$set_origin(originator)
#' # ESR.Spectrum$set_data(data)
#' # ESR.Spectrum$set_par(par)
#' # ESR.Spectrum$get_diff(x, order, ...)
#' # ESR.Spectrum$get_integral(x)
#' # ESR.Spectrum$get_spline(x, ...)
#' # ESR.Spectrum$get_gvalues(v, H, x)
#' # ESR.Spectrum$get_peaks(x, interval, th = 10)
#' 
#' @format \link{R6Class} generator objects
#' @keywords classes methods
#' @author Christoph Burow, University of Cologne (Germany)
#' @examples
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
                          set_par = function(par) {
                            self$parameter <- par
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
                          get_diff = function(data = self$data, order = 1, ...) {
                            invisible(get_diff(data, order, ...))
                          },
                          get_integral = function(data = self$data) {
                            invisible(get_integral(data))
                          },
                          get_spline = function(data = self$data, ...) {
                            invisible(get_spline(data, ...))
                          },
                          get_peaks = function(data = self$data, interval, th = 10) {
                            invisible(get_peaks(data, interval, th))
                          }
                        )
)