#' @export
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
                   self$data <- x
                 }
               )
)

ESR.Spectrum <- R6Class("ESR.Spectrum",
                        inherit = ESR,
                        public = list(
                          typeof = "Spectrum",
                          initialize = function() {
                            super$initialize(originator = "ESR.Spectrum$new()")
                          }
                          )
                        )