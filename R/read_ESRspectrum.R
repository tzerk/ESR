#' Read in multiple associated ESR spectra
#' 
#' Use this function to import a series of associated ESR spectra into R.
#' 
#' This is a wrapper function for \code{read.table}. The function should be
#' used to read in a series of associated ESR spectrum files. A list with all
#' spectrum data is returned, which can be passed to \code{plot_ESRspectrum}
#' for plotting the spectra. \cr\cr \bold{Compressed zip archives} \cr For
#' \code{file} a zip archive can be provided. The file has to have a .zip
#' file extension and must contain all spectrum files in no subfile.
#' 
#' @param file \code{\link{character}} (required): directory where the
#' spectra files are stored. Alternatively, a compressed zip file can be
#' provided. See details!
#' @param ... further arguments
#' @return Returns a terminal output. In addition an
#' \code{\linkS4class{RLum.Results}} object is returned. \cr\cr The output
#' should be accessed using the function \code{\link{get_RLum.Results}}
#' @export
#' @note #
#' @author Christoph Burow, University of Cologne (Germany) Who wrote it
#' @seealso \code{\link{read.table}}, \code{\link{unzip}}, \code{\link{unz}}
#' @references In progress
#' @examples
#' 
#' # Import ASCII text file
#' file1 <- system.file("extdata", "coral.txt", package = "ESR")
#' spec1 <- read_ESRspectrum(file)
#' 
#' # Import .zip archive
#' file2 <- system.file("extdata", "mollusc.zip", package = "ESR") 
#' spec2 <- read_ESR(file)
#' 
#' # Import Bruker ESP300-E raw binary spectrum
#' file3 <- system.file("extdata", "mollusc.SPC", package = "ESR")
#' spec3 <- read_ESRspectrum(file)
#' 
#' #' # Import Bruker ELEXSYS500 spectrum (ASCII)
#' file4 <- system.file("extdata", "quartz.ASC", package = "ESR")
#' spec4 <- read_ESRspectrum(file)
#' 
#' #' # Import Bruker ELEXSYS500 raw binary spectrum
#' file5 <- system.file("extdata", "quartz.DTA", package = "ESR")
#' spec5 <- read_ESRspectrum(file)
#' 
#' @export read_ESRspectrum
read_ESRspectrum <- function(file, ...) {
  
  ## FUNCTIONS ----
  check_type <- function(f) {
    valid_ext <- c("txt", "zip", "asc", "spc")
    ext <- substr(f, nchar(f)-2, nchar(f))
    val <- match(ext, valid_ext)

    if (is.na(val)) stop(paste("Invalid file extension:", ext))
    return(ext)
  }
  
  
  
  ## CHECK TYPE ----
  type <- check_type(file)
  
  print(type)
  
  ## CONSOLE ----
  
  ## RETURN ----

  invisible()
}