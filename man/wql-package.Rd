% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wql-package.R
\docType{package}
\name{wql-package}
\alias{wql-package}
\alias{wql}
\title{The main purpose of \pkg{wql} is to explore seasonal time series through
plots and nonparametric trend tests. It was created originally to examine
water quality data sets (hence, \dQuote{wql}) but is suitable as a more
general purpose set of tools for looking at annual or seasonal time series.}
\description{
One of the more tedious tasks in exploring environmental data sets is
creating usable time series from the original complex data sets, especially
when you want many series at will that group data in different ways. So
\pkg{wql} also provides a way of transforming data sets to a common format
that then allows a diversity of time series to be created quickly. A few
functions are specific to the fields of limnology and oceanography.
}
\details{
The plots are designed for easy use, not for publication-quality graphs.
Nonetheless, extensive customization is possible by passing options through
\code{\ldots{}}, adding annotations in the case of base graphics, and adding
layers in the case of \pkg{ggplot2} objects.

Two functions are used mainly for preparing the times series:

\itemize{ \item a function that transforms incoming data to a common data
structure in the form of the \code{WqData} class \item a function that
easily prepares time series objects from this class }

The \code{WqData} class can be easily adapted to non-aquatic data.
Obviously, the \code{depth} field can be used for elevation in atmospheric
studies. But more generally, the \code{site} and \code{depth} fields can be
used for many two-way classifications and don't need to refer to spatial
location.

Some of the time series functions include:

\itemize{ \item a variety of plots to examine changes in seasonal patterns
\item nonparametric trend tests \item time series interpolation and related
manipulations \item a simple decomposition of a series into different time
scales \item phenological analyses \item the use of empirical orthogonal
functions to detect multiple independent mechanisms underlying temporal
change }

A few functions are specialized for the aquatic sciences:

\itemize{ \item converting between oxygen concentrations and percent
saturation \item converting between salinity and conductivity }

The capabilities of \pkg{wql} are more fully explained in the accompanying
vignette: \dQuote{wql: Exploring environmental monitoring data}.
}
\seealso{
Useful links:
\itemize{
  \item \url{https://github.com/jsta/wql}
  \item Report bugs at \url{https://github.com/jsta/wql/issues}
}

}
\author{
\strong{Maintainer}: Jemma Stachelek \email{jemma.stachelek@gmail.com} [contributor]

Authors:
\itemize{
  \item Alan Jassby
}

Other contributors:
\itemize{
  \item James Cloern [contributor]
}

}
\keyword{package}
