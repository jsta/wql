\name{interpTs}
\Rdversion{1.1}
\alias{interpTs}

\title{
Interpolate missing time series values
}

\description{
Imterpolates missing data in a time series for gaps up to a specified size.
}

\usage{
interpTs(x, gap = 1)
}

\arguments{
  \item{x}{
object of class \code{"ts"} or \code{"mts"}
}
  \item{gap}{
maximum gap length to be interpolated
}
}

\details{
Performs linear interpolation of any \code{NA} runs of length smaller than or equal to \code{gap}. Does not change leading or trailing \code{NA} runs. This interpolation approach is best for periods of low biological activity when sampling is routinely suspended.

Intended for time series but will work with any vector or matrix. Matrices, however, will be interpolated only by column.
}
\value{
The time series with some or all missing values interpolated.
}

\seealso{
\code{\link{decompTs}}
}

\examples{
# Interpolate a vector time series and highlight the imputed data
chl27 <- sfbayChla[, 's27']
x1 <- interpTs(chl27, gap = 3)
plot(x1, col = 'red')
lines(chl27, col = 'blue')

# Interpolate a matrix time series and plot results
x2 <- interpTs(sfbayChla, gap = 1)
plot(x2[, 1:10], main = "SF Bay Chl-a\n(gaps of 1 month interpolated)")
}

\keyword{utility}
\keyword{manip}