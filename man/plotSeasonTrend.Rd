\name{plotSeasonTrend}
\alias{plotSeasonTrend}
\title{
Plot seasonal trends
}
\description{
Plots the trend for each season of a time series (matrix or vector) and indicates statistical significance.
}
\usage{
plotSeasonTrend(x, first, last, type = c("slope", "slope.pct"), method = c("mk", "lin"), plot = TRUE, miss = FALSE, leg = FALSE, ...)
}
\arguments{
  \item{x}{
Time series vector, or time series matrix with column names.
}
  \item{first}{
First year of desired time interval. Will be adjusted if less than start of series.
}
  \item{last}{
Last year of desired time interval. Will be adjusted if more than end of series.
}
  \item{type}{
Type of trend, either in original units per year, or percent per year.
}
  \item{method}{
'mk' for Mann-Kendall (Theil-Sen slope), and 'lin' for linear regression (linear slope).
}
  \item{plot}{
If \code{TRUE}, a plot is generated; otherwise the results are listed.
}
  \item{miss}{
If \code{TRUE}, dot shape is coded to the extent of missing values.
}
  \item{leg}{
If \code{TRUE}, a legend is included.
}
  \item{\dots}{
Further parameters to pass to the plotting function.
}
}
\details{
The slope estimate and its significance are calculated for each season and time series. If \code{type = "slope.pct"}, the slopes are multiplied by 100 and divided by the overall mean (not the median, which can be zero even in cases where a trend estimate is useful). If \code{method = 'mk'}. the Theil-Sen slope is calculated with the Mann-Kendall test of significance. Otherwise, linear regression is used to determine the slope and significance. The results can be shown as a listing, or as a graph in which each time series is represented by a dot plot showing the trend for each season. The colour of the dot indicates whether the trend is significant or not. When \code{method = 'mk'}, the dot shape (solid or empty circle) indicates whether the proportion of slopes joining the first and last fifths of the data is < 0.5 or not. If this value is 0.5 or more, the trend results may not be a good representation of the entire period and a different time window should be considered. Indication of missing values and the graph legend is optional.

Parameters can be passed to the plotting function, in particular, to   \code{facet_wrap} in \pkg{ggplot2}. The most useful parameters here are \code{ncol} (or \code{nrow}), which determines the number of columns (or rows) of plots, and \code{scales}, which can be set to \code{"free_x"} to allow the x-axis to change for each time series.
}
\value{
A plot, or a data frame with the following fields:
\item{trend}{Theil-Sen slope in original units per year, or percent per year.}
\item{p}{p-value for the trend according to the Mann-Kendall test.}
\item{missing}{Proportion of slopes joining first and last fifths of the data that are missing.}
\item{season}{Season number.}
\item{tsname}{Name of time series.}
}
\seealso{
\code{\link{mannKen}}, \code{\link{plotSeason}}, \code{\link[ggplot2]{facet_wrap}}
}
\examples{
x <- sfbayChla
plotSeasonTrend(x, first=1978, last=2009, ncol = 4, scales = 'free_x')
plotSeasonTrend(x, type = 'slope.pct', plot = FALSE)
}
\keyword{Graphics}
\keyword{ts}