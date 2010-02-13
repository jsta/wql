\name{seaKen}
\Rdversion{1.1}
\alias{seaKen}
\alias{seaRoll}

\title{
Seasonal Kendall test
}

\description{
Calculates the Seasonal Kendall test of significance for seasonal time series, including an estimate of the Sen slope.
}

\usage{
seaKen(x)

seaRoll(x, w = 5, rule = 2)
}

\arguments{
  \item{x}{
A time series vector.
}
  \item{w}{
The window width for \dQuote{rolling} estimates of slope.
}
  \item{rule}{
The rule number for excluding windows with excessive missing data.
}
}

\details{
The Seasonal Kendall tests are described by Helsel and Hirsch (1992). The \emph{p-}values provided here are the raw values, not the ones corrected for serial correlation among seasons. In any case, the raw values are recommended for series lengths less than 10 years. But consider using smaller p-values than usual as a threshold for significance.

The function \code{seaRoll} applies \code{seaKen} to rolling time windows of width \code{w}. A minimum \code{w} of five years is required. The only rules currently implemented are: (1) ignore missing data; and (2) report a result only if more than half the seasons are each missing less than half the possible comparisons between the first and last 20\% of the years (Schertz et al. [1991] discuss these and related decisions about missing data).
}

\value{
\code{seaKen} returns a list with the following members:
\item{sen.slope }{Sen slope}
\item{sen.slope.pct}{Sen slope as percent of mean}
\item{p.value}{significance of slope}
\item{miss}{for each season, the fraction missing of slopes connecting first and last 20\% of the years}

\code{seaRoll} returns a matrix with one row per time window containing the Sen slope, the corresponding percent, and the \emph{p-}value. Rows are labelled with the final year of the window.
}

\references{
Helsel, D. and Hirsch, R. (1992) \emph{Statistical methods in water resources.} Elsevier.

Schertz, T.L., Alexander, R.B., and Ohe, D.J. (1991) \emph{The computer program EStimate TREND (ESTREND), a system for the detection of trends in water-quality data.} Water-Resources Investigations Report 91-4040, U.S. Geological Survey.
}

\examples{
chl27 <- sfbayChla[, 's27']
seaKen(chl27)
seaRoll(chl27)
}

\keyword{ts}