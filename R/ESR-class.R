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
                   if (!is.null(x) && ncol(x)==2)
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
                          data.backup = NA,
                          type = "Spectrum",
                          initialize = function() {
                            super$initialize(originator = "ESR.Spectrum$new()")
                          },
                          set_par = function(p) {
                            self$parameter <- p
                          },
                          diff = function() {
                            self$data.backup <- self$set_backup()
                            self$data <- differential(self$data)
                          }
                        ),
                        private = list(
                          set_backup = function() {
                            self$data.backup <- self$data
                          }
                        )
)