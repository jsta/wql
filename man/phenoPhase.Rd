\name{phenoPhase}
\Rdversion{1.1}
\alias{phenoPhase}
\alias{phenoPhase,ts-method}
\alias{phenoPhase,zoo-method}

\title{
Phenological phase
}
\description{
Finds various measures of the phase of the annual cycle, or of some specified month range.
}

\usage{
\S4method{phenoPhase}{ts}(x, season.range = c(1, 12), ...)

\S4method{phenoPhase}{zoo}(x, season.range = c(1, 12), out = c('date', 'doy', 'julian'), ...)
}

\arguments{
  \item{x}{
A seasonal time series, or a class \code{zoo} object.
}
  \item{season.range}{
A vector of two numbers specifying the season range to be considered.
}
  \item{out}{
The form of the output.
}
  \item{...}{
Additional arguments to be passed for changing integration defaults.
}
}

\details{
\code{phenoPhase} gives three measures of the phasing of a seasonal cycle: the time of the maximum (Cloern and Jassby 2008), the \emph{fulcrum} or center of gravity, and the weighted mean season (Colebrook 1979). The latter has sometimes been referred to in the literature as \dQuote{centre of gravity}, but it is not actually the same. These measures differ in their sensitivity to changes in the seasonal pattern, and therefore also in their susceptibility to sampling variability. The time of maximum is the most sensitive, the weighted mean the least.

These measures can be restricted to a subset of the year by giving the desired range of seasons. This can be useful for isolating measures of, say, the spring and autumn phytoplankton blooms in temperate waters. In the case of a seasonal time series, a non-missing value is required for every season or the result will be \code{NA}, so using a period shorter than one year can also help avoid any seasons that are typically not covered by the sampling program. Similarly, in the case of dated observations, a shorter period can help avoid times of sparse data. The method for time series allows for other than monthly frequencies, but \code{season.range} is always interpreted as months for \code{zoo} objects. The method for time series requires data for all seasons in \code{season.range}. The method for \code{zoo} objects will provide a result regardless of number of sampling days, so make sure that data are sufficient for a meaningful result.

The measures are annum-centric, i.e., they reflect the use of calendar year as the annum, which may not be appropriate for cases in which important features occur in winter and span two calendar years. Such cases can be handled by lagging the time series by an appropriate number of months, or by subtracting an appropriate number of days from the individual dates.

\code{\link{tsMake}} can be used to produce \code{ts} and \code{zoo} objects suitable as arguments to this function.

The default parameters used for the \code{integrate} function in \code{phenoPhase} may fail for certain datasets. Try increasing the number of subdivisions above its default of 100 by adding, for example, \code{subdivisions = 1000} to the arguments of \code{phenoPhase}.
}

\value{
A data frame with columns year, time of the maximum, fulcrum, weighted mean time and -- in the case of \code{zoo} objects -- number of observations. In the case of seasonal time series, the results are all given as decimal seasons of the year. In the case of dated observations, the results can be dates, day of the year, or julian day with an origin of 1970-01-01, depending on the option \code{out}.
}

\references{
Cloern, J.E. and Jassby, A.D. (2008) Complex seasonal patterns of primary producers at the land-sea interface. \emph{Ecology Letters} \bold{11,} 1294--1303.

Colebrook, J.M. (1979) Continuous plankton records - seasonal cycles of phytoplankton and copepods in the North Atlantic ocean and the North Sea. \emph{Marine Biology} \bold{51,} 23--32.
}

\seealso{
\code{\link{phenoAmp}}, \code{\link{tsMake}}
}

\examples{
# ts example
y <- sfbayChla[, 's27']
p1 <- phenoPhase(y)
p1
apply(p1, 2, sd, na.rm=TRUE)  # max.time > fulcrum > mean.wt
phenoPhase(y, c(3, 10))

# zoo example
sfb <- wqData(sfbay, c(1,3,4), 5:12, site.order = TRUE, type = "wide",
  time.format = "\%m/\%d/\%Y")
y <- tsMake(sfb, focus = 'chl', layer = c(0, 5), type = 'zoo')
phenoPhase(y[, 's27'])
}

\keyword{manip}
\keyword{ts}
