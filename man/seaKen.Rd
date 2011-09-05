\name{seaKen}
\Rdversion{1.1}
\alias{seaKen}
\alias{seaRoll}

\title{
Seasonal and Regional Kendall test
}

\description{
Calculates the Seasonal or Regional Kendall test of significance, including an estimate of the Sen slope.
}

\usage{
seaKen(x)

seaRoll(x, w = 5, rule = 2, plot = FALSE, ylab = NULL, legend = FALSE)
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
  \item{plot}{
Indicates if a plot should be drawn.
}
  \item{ylab}{
An optional y-axis label.
}
  \item{legend}{
Indicates if the legend is drawn.
}
}

\details{
The Seasonal Kendall tests were introduced by Hirsch et al. (1982) and are further described by Helsel and Hirsch (2002). The \emph{p}-values provided here are the raw values, not the ones corrected for serial correlation among seasons. In any case, the raw values are recommended for series lengths less than 10 years.

The function \code{seaRoll} applies \code{seaKen} to rolling time windows of width \code{w}. A minimum \code{w} of five years is required. The only rules currently implemented are: (1) ignore missing data; and (2) report a result only if more than half the seasons are each missing less than half the possible comparisons between the first and last 20\% of the years (Schertz et al. [1991] discuss these and related decisions about missing data). 

If \code{plot = TRUE} in the latter function, a point plot will be drawn with the Sen slope plotted at the leading year of the trend window. Filled circles indicate \emph{p}-value < 0.01, and a legend will be drawn if requested.

Both functions can be used in conjunction with \code{mts2ts} to calculate a Regional Kendall test of significance for annualized data, along with a regional estimate of trend (Helsel and Frans 2006). See the examples below.
}

\value{
\code{seaKen} returns a list with the following members:
\item{sen.slope }{Sen slope}
\item{sen.slope.pct}{Sen slope as percent of mean}
\item{p.value}{significance of slope}
\item{miss}{for each season, the fraction missing of slopes connecting first and last 20\% of the years}

\code{seaRoll} returns a matrix with one row per time window containing the Sen slope, the corresponding percent, and the \emph{p-}value. Rows are labelled with the leading year of the window.
}

\references{
Helsel, D.R. and Hirsch, R.M. (2002) \emph{Statistical methods in water resources.} Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey. 522 pages. \url{http://pubs.usgs.gov/twri/twri4a3/}

Helsel, D.R. and Frans, L. (2006) Regional Kendall test for trend. \emph{Environmental Science and Technology} \bold{40(13),} 4066-4073.

Hirsch, R.M., Slack, J.R., and Smith, R.A. (1982) Techniques of trend analysis for monthly water quality data. \emph{Water Resources Research} \bold{18,} 107-121.

Schertz, T.L., Alexander, R.B., and Ohe, D.J. (1991) \emph{The computer program EStimate TREND (ESTREND), a system for the detection of trends in water-quality data.} Water-Resources Investigations Report 91-4040, U.S. Geological Survey.
}
\seealso{
\code{\link{mts2ts}}, \code{\link{trendHomog}}
}

\examples{
chl27 <- sfbayChla[, 's27']
seaKen(chl27)
seaRoll(chl27)
seaRoll(chl27, plot = TRUE, legend = TRUE)
chl <-sfbayChla
seaKen(mts2ts(chl))  # too much missing data
seaKen(mts2ts(chl, seas = 2:4))  # better when just Feb-Apr, spring 
    # bloom period, but last 4 stations still missing too much data.
seaKen(mts2ts(chl[, 1:12], 2:4))  # more reliable result
}

\keyword{ts}