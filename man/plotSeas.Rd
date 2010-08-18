\name{plotSeas}
\alias{plotSeas}

\title{
Plots seasonal pattern composites for a time series
}
\description{
Divides the time range for a monthly time series into four periods, and plots composites of seasonal pattern for each period using boxplots. 
}
\usage{
plotSeas(x, num = 4, ylab = NULL)
}

\arguments{
  \item{x}{
Monthly time series.
}
  \item{num}{
Number of time periods.
}
  \item{ylab}{
Optional character string label for y-axis.
}
}

\details{
The composite patterns are plotted in a horizontal row for easier comparison. Experiment with the device aspect ratio (0.5 suggested) and size to get the clearest information. Boxes based on fewer than half of the maximum possible years available are outlined in red.
}

\value{
A plot (and the corresponding object of class \code{"ggplot"}).
}

\seealso{
\code{\link{decompTs}}
}

\examples{
\dontrun{
chl27 <- sfbayChla[, 's27']
plotSeas(chl27, num = 3, ylab = 'Stn 27 Chl-a')
}
}

\keyword{Graphics}
\keyword{ts}