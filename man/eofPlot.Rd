\name{eofPlot}
\Rdversion{1.1}
\alias{eofPlot}

\title{
Plot results of an EOF analysis
}

\description{
Plots the rotated empirical orthogonal functions or amplitude time series resulting from \code{\link{eof}}.
}

\usage{
eofPlot(x, type = c('coef', 'amp'), rev = FALSE, ord = FALSE)
}

\arguments{
  \item{x}{
result of the function \code{\link{eof}} 
}
  \item{type}{
whether the EOF coefficients or amplitudes should be plotted
}
  \item{rev}{
if \code{TRUE}, coefficients and amplitudes are multiplied by \code{-1}
}
  \item{ord}{
if \code{TRUE}, coefficients are ordered by size
}
}

\details{
When the columns of the original data have a natural order, such as stations along a transect or months of the year, there may be no need to reorder the EOF coefficients. But if there is no natural order, such as when columns represents disparate sites around the world, the plot can be more informative if coefficients are ordered by size (\code{ord = TRUE}).

Coefficients and amplitudes for a given EOF may be more easily interpreted if \code{rev = TRUE}, because the sign of the first coefficient is arbitrarily determined and all the other signs follow from that choice.

The vertical guide lines at 0.2 and 0.35 are meant to reflect the Monte Carlo studies of Richman and Gong (1999): \dQuote{For investigators who do not wish to reproduce a part of the Monte Carlo analysis for their research, the most general rule of thumb is that if there is a sufficient sample size, a correlation-based PC loading value of [plus or minus] 0.2-0.35 will likely suffice to separate the hyperplane out...} \sQuote{Sufficient sample size} in this case refers to \code{n > 50}. 
}

\value{
A plot of the EOF coefficients or amplitudes (and corresponding object of class \dQuote{ggplot}).
}

\references{
Richman, M. and Gong, X. (1999) Relationships between the definition of the hyperplane width to the fidelity of principal component loading patterns. \emph{Journal of Climate} \bold{12,} 1557--1576.
}

\seealso{
\code{\link{eof}}
}

\examples{
# Create an annual matrix time series
chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
chla1 <- chla1[, 1:12]  # remove stations with missing years

# eofNum (see examples) suggests n = 1
e1 <- eof(chla1, n = 1)
eofPlot(e1, type = 'coef')
eofPlot(e1, type = 'amp')
}

\keyword{Graphics}