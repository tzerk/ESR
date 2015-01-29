#' @include methods.R
NULL

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
                   if (!is.null(x) && ncol(x) == 2)
                     self$data <- setnames(x, c("x", "y"))
                   else
                     message("Invalid data format! Data was not saved.")
                 }
               )
)

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
                          set_gvalues = function(v, H, x = self$data) {
                            # TODO: move checks to gval() in methods.R
                            if (missing(v) && missing(H))
                              if (typeof(self$parameter) != "list") 
                                return(message("No parameters available!"))
                            
                            if (missing(v) && "MF" %in% self$parameter[ ,1])
                              v <- as.numeric(self$parameter[which(self$parameter[,1]=="MF"), 2])
                            
                            if (missing(H) && any(d$x<=2048))
                              return(message("Invalid or no magnetic field data!"))
                            
                            if (missing(H))
                              H <- self$data$x
                            
                            # TODO: Keep only this line here
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
                          }
                        )
)