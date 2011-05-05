\name{plotTsAnom}
\Rdversion{1.1}
\alias{plotTsAnom}

\title{
  Anomaly plot of time series
}

\description{
  The series is illustrated by vertical lines extending from individual data values to the long-term mean. The axes are not scaled in any way.
}

\usage{
plotTsAnom(x, xlab, ylab)
}

\arguments{
  \item{x}{
    time series.
  }
  \item{xlab}{
    optional x-axis label.
  }
  \item{ylab}{
    optional y-axis label.
  }
}

\value{
  A plot and corresponding object of class \dQuote{ggplot}.
}

\examples{
### Spring bloom size
bloom.mon <-  tsSub(sfbayChla[, 's27'], 3:5)
bloom = aggregate(bloom.mon, 1, mean)
plotTsAnom(bloom, ylab = 'Chl-a')
}

\keyword{ts}
