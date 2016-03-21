\name{mannKen}
\Rdversion{1.1}
\alias{mannKen}

\title{Mann-Kendall trend test and the Sen slope}

\description{
Applies Kendall's tau test for the significance of a monotonic time series trend (Mann 1945). Also calculates the Sen slope as an estimate of this trend.
}

\usage{
mannKen(x, plot = FALSE, type = c("slope", "relative"), order = FALSE,
        pval = .05, pchs = c(19, 21), ...)
}

\arguments{
\item{x}{
A numeric vector, matrix or data frame
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
\item{pchs}{
Plot symbols for significant and not significant trend estimates, respectively
}
\item{...}{
Other arguments to pass to plotting function
}
}

\details{
The Sen slope (alternately, Theil or Theil-Sen slope)---the median slope joining all pairs of observations---is expressed  by quantity per unit time and by the same value divided by the series median. The fraction of missing slopes involving the first and last fifths of the data are provided so that the appropriateness of the slope estimate can be assessed and results flagged. Schertz et al. [1991] discuss this and related decisions about missing data. Other results are used for further analysis by other functions. The relative slope makes sense only as long as the measurement scale is strictly non-negative and the median is not zero (not, e.g., temperature on the Celsius scale). Serial correlation is ignored, so the interval between points should be long enough to avoid strong serial correlation.

If \code{plot = TRUE}, then either the Sen slope (\code{type = "slope"}) or the relative Sen slope (\code{type = "relative"}) are plotted. Plotting the relative slope is useful when the variables in \code{x} have different units. The plot symbols indicate, respectively, that the trend is significant or not significant. The plot can be customized by passing any arguments used by \code{\link{dotchart}} such as \code{xlab} or \code{xlim}, as well as graphical parameters described in \code{\link{par}}.
}

\value{
A list of the following if \code{x} is a vector:
\item{sen.slope}{Sen slope}
\item{sen.slope.rel}{Sen slope divided by median}
\item{p.value}{Significance of slope}
\item{S}{Kendall's S}
\item{varS}{Variance of S}
\item{miss}{Fraction of missing slopes connecting first and last fifths of \code{x}}
or a matrix with corresponding columns if \code{x} is a matrix or data frame.
}

\references{
Mann, H.B. (1945) Nonparametric tests against trend. \emph{Econometrica} \bold{13,} 245--259.

Helsel, D.R. and Hirsch, R.M. (2002) \emph{Statistical methods in water resources.} Techniques of Water Resources Investigations, Book 4, chapter A3. U.S. Geological Survey. 522 pages. \url{http://pubs.usgs.gov/twri/twri4a3/}

Schertz, T.L., Alexander, R.B., and Ohe, D.J. (1991) \emph{The computer program EStimate TREND (ESTREND), a system for the detection of trends in water-quality data.} Water-Resources Investigations Report 91-4040, U.S. Geological Survey.
}

\note{Approximate p-values with corrections for ties and continuity are used if \eqn{n > 10} or if there are any ties. Otherwise, exact p-values based on Table B8 of Helsel and Hirsch (2002) are used. In the latter case, \eqn{p = 0.0001} should be interpreted as \eqn{p < 0.0002}.
}
\seealso{
\code{\link{seaKen}}, \code{\link{seasonTrend}}, \code{\link{tsSub}}
}

\examples{
tsp(Nile)  # an annual time series
mannKen(Nile)

y <- sfbayChla
y1 <- interpTs(y, gap=1)  # interpolate single-month gaps only
y2 <- aggregate(y1, 1, mean, na.rm=FALSE)
mannKen(y2)
mannKen(y2, plot=TRUE)
mannKen(y2, plot=TRUE, type='relative', order = TRUE, pval = .001,
  xlab = "Relative trend")
legend("topleft", legend = "p < 0.05", pch = 19, bty="n")
}

\keyword{ts}
