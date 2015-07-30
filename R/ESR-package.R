#' Collection of functions for Electron Spin Resonance Dating data analysis
#' 
#' This package provides various functions developed for the purpose of
#' Electron Spin Resonance Dating data analysis.
#' 
#' \tabular{ll}{ Package: \tab ESR\cr Type: \tab Package\cr Version: \tab
#' 1.2.4\cr Date: \tab 2015-07-03\cr Author: \tab Christoph Burow \cr
#' Maintainer: \tab Christoph Burow <christoph.burow@@uni-koeln.de>\cr License:
#' \tab GPL-3\cr}
#' 
#' @name ESR-package
#' @aliases ESR-package
#' @docType package
#' @author Christoph Burow (University of Cologne, Germany)
#' @keywords package
#' @import Luminescence boot R6 data.table shiny ggplot2 googleVis graphics grDevices stats utils
NULL

#' Synthetic ESR equivalent dose data set
#' 
#' Synthetic ESR equivalent dose data set
#' 
#' Just a synthetic data set.
#' 
#' @name ExampleData.De
#' @docType data
#' @format A data frame with 2 observations on the following 2 variables.
#' \describe{ \item{V1}{Gamma dose} \item{V2}{ESR intensity} }
#' @references ##
#' @source ##
#' @keywords datasets
#' @examples
#' 
#' data(ExampleData.De, envir = environment())
#' 
NULL

#' ESR spectra of a mollusc and the dpph standard
#' 
#' ESR spectra of a mollusc from the foothills of the Alps and the dpph
#' standard measured at 100 K
#' 
#' In progress
#' 
#' @name ExampleData.ESRspectra
#' @docType data
#' @format A list of ESR spectra where each element contains 1024 observations
#' on the following 2 variables.
#' @references In progress
#' @source In progress
#' @keywords datasets
#' @examples
#' 
#' data(ExampleData.ESRspectra, envir = environment())
#' 
NULL
