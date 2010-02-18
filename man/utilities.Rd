\name{utilities}
\Rdversion{1.1}
\alias{decyear2date}
\alias{layerMean}
\alias{leapYear}
\alias{monthNum}
\alias{years}

\title{
  Miscellaneous utility functions
}

\description{
  A variety of small utilities used in other functions.
  
  \code{decyear2date}: Converts decimal year to object of class \code{"Date"}.

  \code{layerMean}: Acts on a matrix or data frame with depth in the first column and observations for different variables (or different sites, or different times) in each of the remaining columns. The trapezoidal mean over the given depths is calculated for each of the variables. Replicate depths are averaged, and missing values or data with only one unique depth are handled. Data are not extrapolated to cover missing values at the top or bottom of the layer. The result can differ markedly from the simple mean even for equal spacing of depths, because the top and bottom values are weighted by 0.5 in a trapezoidal mean.
  
  \code{leapYear}: \code{TRUE} if \code{x} is a leap year, \code{FALSE} otherwise.
  
  \code{monthNum}: Converts dates to the corresponding numeric month.

  \code{years}: Converts dates to the corresponding numeric years.
}

\usage{
decyear2date(x)

layerMean(d)

leapYear(x)

monthNum(y)

years(y)
}

\arguments{
  \item{d}{
    A numeric matrix or data frame with depth in the first column and observations for some variable in each of the remaining columns.
  }
  \item{x}{
    A vector of class \code{"numeric"}.
  }
  \item{y}{
    A vector of class \code{"Date"} or \code{"POSIX"} date-time.
  }
}

\examples{
decyear2date(c(1996.001, 1999.999, 2004.162, 2005.162))

z = c(1,2,3,5,10)  # 5 depths
x = matrix(rnorm(30), nrow = 5)  # 6 variables at 5 depths
layerMean(cbind(z, x))

leapYear(seq(1500, 2000, 100))
leapYear(c(1996.9, 1997))

monthNum(as.Date(c('2007-03-17', '2003-06-01')))

y = Sys.time()
years(y)
}

\keyword{manip}
