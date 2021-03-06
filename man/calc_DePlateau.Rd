% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_DePlateau.R
\name{calc_DePlateau}
\alias{calc_DePlateau}
\title{Fit a dose response curve to ESR data and create a DE-DEmax Plot after
Schellmann & Radtke (2001)}
\usage{
calc_DePlateau(input.data, min.DosePoints = 5, fit.weights = "equal",
  model = "EXP", mean.natural = FALSE, show.grid = TRUE,
  output.console = TRUE, ...)
}
\arguments{
\item{input.data}{\code{\link{data.frame}} (\bold{required}): data frame
with two columns for x=Dose, y=ESR.intensity. Optional: a third column
containing individual ESR intensity errors can be provided.}

\item{min.DosePoints}{\code{\link{integer}} (with default): minimum number
of datapoints used for fitting the single saturating exponential.}

\item{fit.weights}{\code{\link{logical}} (with default): option whether the
fitting is done with equal weights (\code{'equal'}) or weights proportional
to intensity (\code{'prop'}). If individual ESR intensity errors are
provided, these can be used as weights by using \code{'error'}.}

\item{model}{\code{\link{character}} (with default): Currently implemented
models: single-saturating exponential (\code{"EXP"}), linear (\code{"LIN"}).}

\item{mean.natural}{\code{\link{logical}} (with default): If there are repeated
measurements of the natural signal should the mean amplitude be used for
fitting?}

\item{show.grid}{\code{\link{logical}} (with default): show horizontal grid
lines in plots (\code{TRUE/FALSE})}

\item{output.console}{\code{\link{logical}} (with default): plot console
output (\code{TRUE/FALSE})}

\item{...}{further arguments passed to \code{\link{plot}} and \code{\link{par}}.}
}
\value{
Returns terminal output and a plot. In addition, a list is returned
containing the following elements:

\item{output}{data frame containing the De (datapoints n, De, De.Error,
max.Dose).}
}
\description{
This function repeatedly fits a single saturating exponential to ESR data
and calculates the De while removing the highest datapoint datapoint after
each iteration. It then creates a DE-DEmax-Plot after Schellmann & Radtke
(2001) where the DE is plotted against the number of datapoints used for
fitting.
}
\details{
\bold{Fitting methods} \cr\cr For fitting of the dose response curve the
\code{nls} function with the \code{port} algorithm is used. A single
saturating exponential in the form of \deqn{y = a*(1-exp(-(x+c)/b))} is
fitted to the data. Parameters b and c are approximated by a linear fit
using \code{lm}.\cr\cr \bold{Fit weighting} \cr\cr If \code{'equal'} all
datapoints are weighted equally. For \code{'prop'} the datapoints are
weighted proportionally by their respective ESR intensity: \deqn{fit.weights
= 1/intensity/(sum(1/intensity))} If individual errors on ESR intensity are
available, choosing \code{'error'} enables weighting in the form of:
\deqn{fit.weights = 1/error/(sum(1/error))}
}
\note{
Fitting of the dose response curve using \code{\link{fit_DRC}} is
largely derived from the \code{plot_GrowthCurve} function of the
'Luminescence' package by Kreutzer et al. (2012).\cr\cr \bold{Fitting
methods} \cr Currently, only fitting of a single saturating exponential is
supported. Fitting of two exponentials or an exponential with a linear term
may be implemented in a future release.
}
\examples{

\dontrun{
##load example data
data(ExampleData.De, envir = environment())

#calculate and plot De-Dmax Plateau
calc_DePlateau(input.data = ExampleData.De,
               min.DosePoints = 5, 
               fit.weights = 'prop', 
               show.grid = TRUE, 
               output.console = FALSE)
}

}
\references{
Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of
equivalent dose and error calculation and display in OSL dating: An overview
and some recommendations. Quaternary Geochronology, 11, pp. 1-27. \cr\cr
Kreutzer, S., Schmidt, C., Fuchs, M.C., Dietze, M., Fischer, M., Fuchs, M.,
2012. Introducing an R package for luminescence dating analysis. Ancient TL,
30 (1), pp 1-8.
}
\seealso{
\code{\link{plot}}, \code{\link{nls}}, \code{\link{lm}},
\code{\link{fit_DRC}}
}
\author{
Christoph Burow, University of Cologne (Germany) Who wrote it
}
