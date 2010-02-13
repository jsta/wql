\name{plotTsTile}
\Rdversion{1.1}
\alias{plotTsTile}

\title{
  Image plot of monthly time series 
}

\description{
  Monthly values are transformed into deciles or other bins, and corresponding colors are plotted in a month by year matrix.
}

\usage{
plotTsTile(x, title = NULL, trim = TRUE, four = TRUE, loganom = TRUE)
}

\arguments{

  \item{x}{
    monthly time series.
  }
  \item{title}{
    main title for plot
  }
  \item{trim}{
    logical value indicating whether leading and trailing NA values should be removed.
  }
  \item{four}{
    logical value indicating whether data should be binned into four special groups or into deciles.
  }
  \item{loganom}{
    logical value indicating whether data should be transformed into log-anomalies.
  }

}
\details{
  If \code{four = TRUE}, then \code{x} is first divided into a positive and negative bin. Each bin is then further divided into two bins by its mean, yielding a total of four bins. If \code{four=FALSE}, then \code{x} is simply divided into deciles. In either case, each bin has its own assigned color, with colors ranging from dark blue (smallest numbers) through light blue and pink to red.

  Although \code{four = TRUE} can be useful for any data in which 0 represents a value with special significance, it is especially so for data converted into log-anomalies, i.e., \code{log10(x)/mean(x, na.rm=TRUE)}. The mean month then has value 0, and a value of -1, for example, indicates original data equal to one-tenth the mean. Log-anomaly transforms can be particularly appropriate for biological populations, in which variability is often approximately proportional to the mean.
}

\value{
  An image plot of monthly values classified into either deciles or into four bins as described above (and corresponding object of class \dQuote{trellis}).
}

\references{
  O'Brien T., Lopez-Urrutia A., Wiebe P.H., Hay S. (editors) (2008) \emph{ICES Zooplankton Status Report 2006/2007.} ICES Cooperative Research Report 292, International Council for the Exploration of the Sea, Copenhagen, 168 p.
}

\note{
  When \code{loganom = TRUE}, the anomalies are calculated with respect to the overall mean month. This differs from, for example, the log-anomaly zooplankton plot of O'Brien et al. (2008), in which a monthly anomaly is calculated with respect to the mean value of the same month.
}

\examples{
# plot log-anomalies in four bins
chl27 = sfbayChla[, 's27']
plotTsTile(chl27, 
  title = 'SF Bay station 27\nlog-anomalies of chlorophyll')

# plot deciles
plotTsTile(chl27, title = 'SF Bay station 27\n chlorophyll', 
  four = FALSE, loganom = FALSE)
}

\keyword{hplot}
\keyword{ts}