\name{plotTsAnom}
\alias{plotTsAnom}

\title{
Anomaly plot of time series
}

\description{
Series are illustrated by vertical lines extending from individual data values to the long-term mean. The axes are not scaled in any way. Anomaly plots are useful for visualizing shifts in time series levels.
}

\usage{
plotTsAnom(x, xlab, ylab, plot.order = colnames(x), 
  strip.labels = colnames(x), ...)
}

\arguments{
  \item{x}{
matrix or vector time series
}
  \item{xlab}{
optional x-axis label
}
  \item{ylab}{
optional y-axis label
}
  \item{plot.order}{
column names of matrix time series, possibly in a different order
}
  \item{strip.labels}{
labels for individual time series plots
}
  \item{\dots}{
additional options
}
}

\details{
Options are passed to the underlying \code{facet_wrap} function in \pkg{ggplot2}. The main ones of interest are \code{ncol} for setting the number of plotting columns and \code{scales = "free_y"} for allowing the y scales of the different plots to be independent.
}

\seealso{
\code{\link{plotTs}}
}

\value{
A plot and corresponding object of class \dQuote{ggplot}.
}

\examples{
### Spring bloom size for 6 stations in SF Bay
bloom <- aggregate(sfbayChla[, 1:6], 1, meanSub, sub=3:5)
plotTsAnom(bloom, ylab = 'Chl-a')
}

\keyword{Graphics}
\keyword{ts}