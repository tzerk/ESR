% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_Spectrum.R
\name{read_Spectrum}
\alias{read_Spectrum}
\title{Read in single or multiple ESR spectra}
\usage{
read_Spectrum(file, device = "auto", ...)
}
\arguments{
\item{file}{\link{character} (\strong{required}):
file path or directory where the spectra files are stored.}

\item{device}{\link{character} (\emph{with default}):
Manually specify the device the spectrum files were produced by.
By default, the proper binary format is deduced from the file
extension, which may however fail in case of ambiguous file
endings. See details.

Allowed options are:
\itemize{
\item \code{"auto"} (the default)
\item \code{"ESP300-E"} (.SPC)
\item \code{"ELEXSYS500"} (.DTA)
\item \code{"EMXplus"} (.SPC)
}}

\item{...}{further arguments
(e.g., \code{n} to specify the number of datapoints; \code{sw} to specify
the sweep width).}
}
\value{
Returns a terminal output. In addition an
\code{\link{R6Class}} object is returned. \cr
}
\description{
Use this function to import a series of associated ESR spectra into R.
}
\details{
This is a wrapper function for \code{read.table} and \code{readBin}.
The function should be
used to read in a series of associated ESR spectrum files. A list with all
spectrum data is returned, which can be passed to \code{plot_Spectrum}
for plotting the spectra.

\strong{Binary formats}

This function is able to read in binary spectrum files produced by Bruker
ESR devices. By default (\code{device = 'auto'}), the function assumes the
proper mode of the vector (integer or numeric), endianness and number of
bytes per element based on the file extension. Currently, the following
devices are supported:
\itemize{
\item \strong{Bruker ESP300-E} (\emph{.SPC} files): \code{what = 'int', endian = 'big', size = 4}
\item \strong{Bruker ELEXSYS500} (\emph{.DTA} files): \code{what = 'numeric', endian = 'big', size = 8}
\item \strong{Bruker EMXplus (old)} (\emph{.SPC} files): \code{what = 'numeric', endian = 'little', size = 4}
}

Note that the Bruker ESP300-E and EMXplus devices share a common file
extension (.SPC) and that \code{device = 'auto'} (the default) will always
assume that the SPC file is from a ESP300-E. If your SPC file is
from a EMXplus, however, you should manually specify this using
\code{device = 'EMXplus'}.
}
\examples{

# Import ASCII text file
file1 <- system.file("extdata", "coral.txt", package = "ESR")
spec1 <- read_Spectrum(file1)

# Import .zip archive
file2 <- system.file("extdata", "mollusc.zip", package = "ESR") 
spec2 <- read_Spectrum(file2)

# Import Bruker ESP300-E raw binary spectrum
file3 <- system.file("extdata", "mollusc.SPC", package = "ESR")
spec3 <- read_Spectrum(file3)

# Import Bruker ELEXSYS500 spectrum (ASCII)
file4 <- system.file("extdata", "dpph.ASC", package = "ESR")
spec4 <- read_Spectrum(file4)

# Import Bruker ELEXSYS500 raw binary spectrum
file5 <- system.file("extdata", "quartz.DTA", package = "ESR")
spec5 <- read_Spectrum(file5)

# Import Bruker EMXplus raw binary spectrum
file6 <- system.file("extdata", "DL_alanine.spc", package = "ESR")
spec6 <- read_Spectrum(file6, device = "EMXplus")

# Import all example data sets at once by providing only the directory
dir <- system.file("extdata", package = "ESR")
specs <- read_Spectrum(dir)

}
\references{
In progress
}
\seealso{
\code{\link{read.table}}, \code{\link{readBin}}, \code{\link{read.csv}}
}
\author{
Christoph Burow, University of Cologne (Germany)
}
