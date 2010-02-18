\name{ts2df}
\Rdversion{1.1}
\alias{ts2df}
\alias{monthCor}

\title{
Convert time series to data frame
}

\description{
Convert monthly time series vector to a year \code{x} month data frame for several possible subsequent analyses. Leading and trailing empty rows are removed.
}

\usage{
ts2df(x, mon1 = 1, addYr = FALSE, omit = FALSE)

monthCor(x)
}

\arguments{
  \item{x}{
monthly time series vector
}
  \item{mon1}{
starting month number, i.e., first column of the data frame
}
  \item{addYr}{
rows are normally labelled with the year of the starting month, but \code{addYr = TRUE} will add 1 to this year number
}
  \item{omit}{
if \code{TRUE}, then rows with any \code{NA} will be removed.
}
}

\details{
Our main use of \code{ts2df} is to convert a single monthly time series into a year \code{x} month data frame for \acronym{EOF} analysis of interannual variability. 

\code{monthCor} finds the month-to-month correlations in a monthly time series \code{x}. It is useful for deciding where to start the 12-month period for an \code{EOF} analysis (\code{mon1} in \code{ts2df}), namely, at a time of low serial correlation in \code{x}.
}

\value{
An \code{n x 12} data frame, where \code{n} is the number of years.}

\references{
Craddock, J. (1965) A meteorological application of principal component analysis. \emph{Statistician} \bold{15,} 143--156.
}

\seealso{
\code{\link{eof}}
}

\examples{
# San Francisco Bay station 27 chlorophyll has the lowest serial 
# correlation in Oct-Nov, with Sep-Oct a close second
chl27 <- sfbayChla[, 's27']
monthCor(chl27)

# Convert to a data frame with October, the first month of the 
# local "water year", in the first column
tsp(chl27)
chl27 <- round(chl27, 1)
ts2df(chl27, mon1 = 10, addYr = TRUE)
ts2df(chl27, mon1 = 10, addYr = TRUE, omit = TRUE)
}

\keyword{ts}
\keyword{manip}
