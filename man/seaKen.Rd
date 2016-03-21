\name{seaKen}
\Rdversion{1.1}
\alias{seaKen}

\title{
Seasonal and Regional Kendall trend test
}

\description{
Calculates the Seasonal or Regional Kendall test of significance, including an estimate of the Sen slope.
}

\usage{
seaKen(x, plot = FALSE, type = c("slope", "relative"), order = FALSE, 
         pval = .05, mval = .5, pchs = c(19, 21), ...)
}

\arguments{
\item{x}{
A numeric vector, matrix or data frame made up of seasonal time series
}
\item{plot}{
Should the trends be plotted when x is a matrix or data frame?
}
\item{type}{
Type of trend to be plotted, actual or relative to series median
}
\item{order}{
Should the plotted trends be ordered by size?
}
\item{pval}{
p-value for significance
}
\item{mval}{
Minimum fraction of seasons needed with non-missing slope estimates
}
\item{pchs}{
Plot symbols for significant and not significant trend estimates, respectively
}
\item{...}{
Other arguments to pass to plotting function
}
}

\details{
The Seasonal Kendall test (Hirsch et al. 1982) is based on the Mann-Kendall tests for the individual seasons (see \code{\link{mannKen}}). \emph{p}-values provided here are not corrected for serial correlation among seasons.

If \code{plot = TRUE}, then either the Sen slope (\code{type = "slope"}) or the relative Sen slope (\code{type = "relative"}) are plotted. Plotting the relative slope is useful when the variables in \code{x} have different units. The plot symbols indicate, respectively, that the trend is significant or not significant. The plot can be customized by passing any arguments used by \code{\link{dotchart}} such as \code{xlab}, as well as graphical parameters described in \code{\link{par}}.

If \code{mval} or more of the seasonal slope estimates are missing, then that trend is considered to be missing. The seasonal slope estimate (\code{\link{mannKen}}), in turn, is missing if half or more of the possible comparisons between the first and last 20\% of the years are missing.

The function can be used in conjunction with \code{mts2ts} to calculate a Regional Kendall test of significance for annualized data, along with a regional estimate of trend (Helsel and Frans 2006). See the examples below.
}

\value{
A list of the following if \code{x} is a vector:
\code{seaKen} returns a list with the following members:
\item{sen.slope }{Sen slope}
\item{sen.slope.pct}{Sen slope as percent of mean}
\item{p.value}{significance of slope}
\item{miss}{for each season, the fraction missing of slopes connecting first and last 20\% of the years}
or a matrix with corresponding columns if \code{x} is a matrix or data frame.
}

\references{
Helsel, D.R. and Frans, L. (2006) Regional Kendall test for trend. \emph{Environmental Science and Technology} \bold{40(13),} 4066-4073.

Hirsch, R.M., Slack, J.R., and Smith, R.A. (1982) Techniques of trend analysis for monthly water quality data. \emph{Water Resources Research} \bold{18,} 107-121.
}
\seealso{
\code{\link{mannKen}}, \code{\link{mts2ts}}, \code{\link{trendHomog}}
}

\examples{
# Seasonal Kendall test:
chl <- sfbayChla # monthly chlorophyll at 16 stations in San Francisco Bay
seaKen(sfbayChla[, 's27']) # results for a single series at station 27
seaKen(sfbayChla) # results for all stations
seaKen(sfbayChla, plot=TRUE, type="relative", order=TRUE)

# Regional Kendall test:
# Use mts2ts to change 16 series into a single series with 16 "seasons"
seaKen(mts2ts(chl))  # too many missing data
seaKen(mts2ts(chl, seas = 2:4)) # better when just Feb-Apr, spring bloom period,
                                # but last 4 stations still missing too much.
seaKen(mts2ts(chl[, 1:12], 2:4)) # more reliable result
}

\keyword{ts}