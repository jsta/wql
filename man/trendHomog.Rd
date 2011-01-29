\name{trendHomog}
\alias{trendHomog}
\title{
Trend homogeneity test
}
\description{
Tests for homogeneity of seasonal trends using method proposed by van Belle and Hughes (1984).
}
\usage{
trendHomog(x)
}
\arguments{
  \item{x}{
A vector time series.
}
}
\value{
\item{chisq.trend}{"Trend" chi-square.}
\item{chisq.homog}{"Homogeneous" chi-square.}
\item{p.value}{For null hypothesis that trends are homogeneous.}
}
\references{
van Belle, G. and Hughes, J.P. (1984) Nonparametric tests for trend in water quality. \emph{Water Resources Research} \bold{20,} 127-136.
}
\seealso{
\code{\link{seaKen}}
}
\examples{
## Apply to a monthly vector time series to test homogeneity 
## of seasonal trends.
x <- sfbayChla[, 's27']
trendHomog(x)
}
\keyword{ts}
