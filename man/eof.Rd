\name{eof}
\Rdversion{1.1}
\alias{eof}

\title{
Empirical orthogonal functions
}

\description{
Finds and rotates empirical orthogonal functions (\acronym{EOF}s, or principal components).
}

\usage{
eof(x, n)
}

\arguments{
  \item{x}{
a numeric object of class \code{"matrix"} or \code{"data.frame"}, with no \code{NA} values
}
  \item{n}{
number of \acronym{EOF}s to retain for rotation
}
}

\details{
This function is basically just a wrapper for \code{prcomp} followed by \code{promax} and is meant to facilitate routine examination of variability modes in collections of time series. For the usage intended in this package, each row of \code{x} usually will be an observation for a specific time or time period. Each column will be a specific location---the most common usage---but can also be seasons of the year (Jassby et al. 1999) or even a combination of seasons and depth layers (Jassby et al. 1990). First, eigenvalues are found by singular value decomposition of the \emph{correlation} matrix. \code{\link{eofNum}} can be used to help choose \code{n}. The first \code{n} \acronym{EOF}s are then rotated using the \code{\link{promax}} method with power \code{m = 2} (Richman 1986). Hannachi et al. (2007) give a detailed discussion of this exploratory approach with emphasis on meteorological data.
}

\value{
A list with the following members:
\item{REOF}{a data frame with rotated \acronym{EOF}s}
\item{amplitude}{a data frame with amplitude time series of \acronym{REOF}s}
\item{eigen.pct}{all eigenvalues of correlation matrix as percent of total variance}
\item{variance}{variance explained by retained \acronym{EOF}s}
}

\references{
Hannachi, A., Jolliffe, I.T., and Stephenson, D.B. (2007) Empirical orthogonal functions and related techniques in atmospheric science: A review. \emph{International Journal of Climatology} \bold{27,} 1119--1152.

Jassby, A.D., Powell, T.M., and Goldman, C.R. (1990) Interannual fluctuations in primary production: Direct physical effects and the trophic cascade at Castle Lake, California (USA). \emph{Limnology and Oceanography} \bold{35,} 1021--1038.

Jassby, A.D., Goldman, C.R., Reuter, J.E., and Richards, R.C. (1999) Origins and scale dependence of temporal variability in the transparency of Lake Tahoe, California-Nevada. \emph{Limnology and Oceanography} \bold{44,} 282--294.

Richman, M. (1986) Rotation of principal components. \emph{Journal of Climatology} \bold{6,} 293--335.
}

\seealso{
\code{\link{eofNum}}, \code{\link{plotEof}}, \code{\link{monthCor}}, \code{\link{ts2df}}
}
\examples{
# Create an annual matrix time series
chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
chla1 <- chla1[, 1:12]  # remove stations with missing years
# eofNum (see examples) suggests n = 1
eof(chla1, 1)
}

\keyword{ts}