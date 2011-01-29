\name{eofNum}
\Rdversion{1.1}
\alias{eofNum}
\alias{ruleN}

\title{
Assess significance of eigenvalues
}

\description{
Plots the eigenvalue spectrum (scree plot) for the correlation matrix of a space-time field of observations. Useful for deciding how many empirical orthogonal functions (\acronym{EOF}s, or principal components) to retain for rotation.
}

\usage{
eofNum(x, distr = c("normal", "lognormal"), n = nrow(x), reps = 10000) 

ruleN(n, p, type = c("normal", "lognormal"), reps = 10000)
}

\arguments{
  \item{x}{
an object of class \code{"data.frame"} or \code{"matrix"}, with no missing values
}
  \item{distr, type}{
suspected distribution of data
}
  \item{n}{
number of independent observations in the sample, also known as effective sample size (\code{n <= nrow(x)})
}
  \item{reps}{
number of repetitions to use for rule N
}
 \item{p}{
number of columns in data
}
}

\details{
Computes singular values of the correlation matrix for a space-time field represented as a data frame or matrix of observations \code{x} locations. The eigenvalue variances are plotted against eigenvalue number, and the cumulative variance as \% of total is plotted over each eigenvalue. The approximate 0.95 confidence limits are depicted for each eigenvalue, using North et al.'s (1982) rule of thumb. The significance of each eigenvalue is also estimated using \emph{rule N} (Overland and Preisendorfer 1982), which repeatedly computes eigenvalues of the correlation matrix for an \code{n x p} matrix of a random variable and returns the 0.95 quantiles. The \code{dist} determines the distribution for the random variable. \code{ruleN} is not normally called directly.

Both North's rule-of-thumb and rule N as calculated here by default ignore any autocorrelation in the data and are therefore lenient in accepting the significance of eigenvalues. Their findings should therefore be taken as upper limits to the number of significant eigenvalues. If the autocorrelation structure is assessed separately and can be expressed in terms of effective sample size (e.g., Thiebaux and Zwiers 1984), then \code{n} can be set equal to this number. The default is to assume that the effective and actual sample sizes are the same.
}

\value{
A plot (and corresponding object of class \dQuote{ggplot}).
}

\references{
North, G., Bell, T., Cahalan, R., and Moeng, F. (1982) Sampling errors in the estimation of empirical orthogonal functions. \emph{Monthly Weather Review} \bold{110,} 699--706.

Overland, J.E. and Preisendorfer, R.W. (1982). A significance test of principal components applied to a cyclone climatology. \emph{Monthly Weather Review} \bold{110,} 1--4.

Thiebaux H.J. and Zwiers F.W. (1984) The interpretation and estimation of effective sample sizes. \emph{Journal of Climate and Applied Meteorology} \bold{23,} 800--811.
}

\seealso{
\code{\link{eof}}, \code{\link{interpTs}}, \code{\link{monthCor}}, \code{\link{eofPlot}}
}

\examples{
# Create an annual time series data matrix from sfbay chlorophyll data
chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)  # average over each year
chla1 <- chla1[, 1:12]  # remove stations with missing years
eofNum(chla1, distr = 'lognormal', reps = 2000)
# These stations appear to act as one with respect to chlorophyll 
# variability on the annual scale.
}

\keyword{Graphics}
\keyword{ts}