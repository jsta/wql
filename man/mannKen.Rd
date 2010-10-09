\name{mannKen}
\Rdversion{1.1}
\alias{mannKen}

\title{Mann-Kendall test and the Sen slope}

\description{
Applies Kendall's tau test for the significance of a monotonic time series trend (Mann 1945). Also calculates the Sen slope as an estimate of this trend.
}

\usage{
mannKen(x)
}

\arguments{
  \item{x}{
A time series vector.
}
}

\details{
The Sen slope (alternately, Theil or Theil-Sen slope)---the median slope joining all pairs of observations---is expressed both by quantity per unit time and percent of the mean quantity per unit time. The fraction of missing slopes involving the first and last fifths of the data are provided so that the appropriateness of the slope estimate can be assessed and results flagged. Other results are used for further analysis by other functions.
}

\value{
A list with the following members:
\item{sen.slope}{Sen slope.}
\item{sen.slope.pct}{Sen slope as percent of mean.}
\item{p.value}{Significance of slope.}
\item{S}{Kendall's S.}
\item{varS}{Variance of S.}
\item{miss}{Fraction of missing slopes connecting first and last fifths of \code{x}.}
}

\references{
Mann, H.B. (1945) Nonparametric tests against trend. \emph{Econometrica} \bold{13,} 245--259.

Slack, J.R., Lorenz, D.L., and others (2003) \emph{USGS library for S-PLUS for Windows.} Open-File Report 03-357, U.S. Geological Survey.
}

\note{The \code{varS} calculation is based on \code{kensen} in the \acronym{USGS} library for \acronym{S-PLUS} (Slack et al. 2003).}

\seealso{
\code{\link{seaKen}}, \code{\link{plotSeasonTrend}}
}

\examples{
tsp(Nile)  # an annual time series
mannKen(Nile)
}

\keyword{ts}