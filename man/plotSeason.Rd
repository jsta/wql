\name{plotSeason}
\alias{plotSeason}

\title{
Plots seasonal patterns for a time series
}
\description{
Divides the time range for a monthly time series into different eras and plots composites of seasonal pattern. Can also plot each month separately for the entire record. 
}
\usage{
plotSeason(x, type = c("by.era", "by.month"), num = 4, ylab = NULL, ...)
}

\arguments{
  \item{x}{
Monthly time series.
}
  \item{type}{
Plot seasonal pattern by era, or each month for the entire record.
}
  \item{num}{
Number of eras.
}
  \item{ylab}{
Optional character string label for y-axis.
}
  \item{\dots}{
Further parameters to pass to the plotting function.
}
}

\details{
When plotted \code{"by.era"}, the composite patterns are plotted in a horizontal row for easier comparison, which limits the number of periods that can be examined. Boxes based on fewer than half of the maximum possible years available are outlined in red. When plotted \code{"by.month"}, parameters can be passed to \code{facet_wrap} in \pkg{ggplot2}. The most useful parameters here are \code{ncol} (or \code{nrow}), which determines the number of columns (or rows) of plots, and \code{scales}, which can be set to \code{"free_y"} to allow the y-axis to change for each individual month. A loess smoother with \code{span = 1} is added to each plot. As always, and especially with these plots, experiment with the device aspect ratio and size to get the clearest information. 
}

\value{
A plot (and the corresponding object of class \code{"ggplot"}).
}

\seealso{
\code{\link{decompTs}}, \code{\link{plotSeasonTrend}}
}

\examples{
\dontrun{
chl27 <- sfbayChla[, 's27']
plotSeason(chl27, num = 3, ylab = 'Stn 27 Chl-a')
}
}

\keyword{Graphics}
\keyword{ts}