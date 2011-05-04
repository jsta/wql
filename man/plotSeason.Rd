\name{plotSeason}
\alias{plotSeason}

\title{
Plots seasonal patterns for a time series
}
\description{
Divides the time range for a monthly time series into different eras and plots composites of seasonal pattern. Can also plot each month separately for the entire record.
}
\usage{
plotSeason(x, type = c("by.era", "by.month"), num.era = 4,
    same.plot = TRUE, ylab = NULL, num.col = 3)
}

\arguments{
    \item{x}{
    Monthly time series.
    }
    \item{type}{
    Plot seasonal pattern by era, or each month for the entire record.
    }
    \item{num.era}{
    Number of eras.
    }
    \item{same.plot}{
    Should eras be plotted by month?
    }
    \item{ylab}{
    Optional character string label for y-axis.
    }
    \item{num.col}{
    Number of columns when plotted \code{"by.month"}.
    }
}

\details{
When plotted \code{"by.era"} and \code{same.plot = FALSE}, the composite patterns are plotted in a horizontal row for easier comparison, which limits the number of periods that can be examined. Boxes based on fewer than half of the maximum possible years available are outlined in red. If \code{same.plot = TRUE}, a single plot is produced with era boxplots arranged by month. When plotted \code{"by.month"}, values for each month are first converted to standardized anomalies, i.e., by subtraction of long-term mean and division by standard deviation. As always, and especially with these plots, experiment with the device aspect ratio and size to get the clearest information.
}

\value{
A plot (and the corresponding object of class \code{"ggplot"}).
}

\seealso{
\code{\link{decompTs}}, \code{\link{seasonTrend}}
}

\examples{
chl27 <- sfbayChla[, 's27']
plotSeason(chl27, num.era = 3, ylab = 'Stn 27 Chl-a')
plotSeason(chl27, num.era = 3, same.plot = FALSE, ylab = 'Stn 27 Chl-a')
plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a')
}

\keyword{Graphics}
\keyword{ts}
