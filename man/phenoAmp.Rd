\name{phenoAmp}
\Rdversion{1.1}
\alias{phenoAmp}
\alias{phenoAmp,ts-method}
\alias{phenoAmp,zoo-method}

\title{
Phenological amplitude
}

\description{
Finds various measures of the amplitude of the annual cycle, or of some specified month range.
}

\usage{
\S4method{phenoAmp}{ts}(x, mon.range = c(1, 12))

\S4method{phenoAmp}{zoo}(x, mon.range = c(1, 12))
}

\arguments{
  \item{x}{
A monthly time series, or a class \code{"zoo"} object.
}
  \item{mon.range}{
A vector of two numbers specifying the month range to be considered.
}
}

\details{
\code{phenoAmp} gives three measures of the amplitude of a seasonal cycle: the range, the range divided by the mean, and the standard deviation divided by the mean, i.e., the coefficient of variation.

These measures can be restricted to a subset of the year by giving the desired range of month numbers. This can be useful for isolating measures of, say, the spring and autumn phytoplankton blooms in temperate waters. In the case of a monthly time series, a non-missing value is required for every month or the result will be \code{NA}, so using a period shorter than one year can also help avoid any months that are typically not covered by the sampling program. Similarly, in the case of dated observations, a shorter period can help avoid times of sparse data.

\code{\link{tsMake}} can be used to produce \code{"ts"} and \code{"zoo"} objects suitable as arguments to this function.
}

\value{
A data frame.
}

\references{
Cloern, J.E. and Jassby, A.D. (2008) Complex seasonal patterns of primary producers at the land-sea interface. \emph{Ecology Letters} \bold{11,} 1294--1303.
}

\seealso{
\code{\link{phenoPhase}}, \code{\link{tsMake}}
}

\examples{
y <- sfbayChla[, 's27']

p2 <- phenoAmp(y)
p2
apply(p2, 2, mean, na.rm=TRUE)

phenoAmp(y, c(1, 6))
}

\keyword{manip}
\keyword{ts}
