\name{wqData}
\Rdversion{1.1}
\alias{wqData}

\title{
  Construct an object of class "WqData"
}
\description{
\code{wqData} is a constructor for the \code{"WqData"} class that is often more convenient to use than \code{new}. It converts a data.frame containing water quality data in \dQuote{long} or \dQuote{wide} format to a \code{"WqData"} object. In \dQuote{long} format, observations are all in one column and a second column is used to designate the variable being observed. In \dQuote{wide} format, observations for each variable are in a separate column.
}

\usage{
wqData(data, locus, wqdata, site.order, time.format = "\%Y-\%m-\%d", 
        type = c("long", "wide"))
}

\arguments{
  \item{data}{
    Data frame containing water quality data.
  }
  \item{locus}{
Character or numeric vector designating column names or numbers, respectively, in \code{data} that correspond to \code{time}, \code{site} and \code{depth}.
  }
  \item{wqdata}{
In the case of \dQuote{long} data, character or numeric vector designating column names or numbers, respectively, in \code{data} that correspond to \code{variable} and \code{value}. In the case of \dQuote{wide} data, character or numeric vector designating column names or numbers, respectively, in \code{data} that denote water quality variable data.
  }
  \item{site.order}{
If \code{TRUE}, \code{site} factor levels will be ordered in alphanumeric order.
  }
  \item{time.format}{
Conversion specification for \code{time} defined by \acronym{ISO C/POSIX} standard (see \code{\link{strptime}}).
  }
  \item{type}{
    Either \dQuote{long} or \dQuote{wide} \code{data}.
  }
}

\details{ If the data are already in long format, the function has little to do but rename the data fields. If in wide format, the \pkg{reshape} package is called to \code{melt} the data. The function also removes \code{NA} observations, converts \code{site} to (possibly ordered) factors with valid variable names, and converts \code{time} to class \code{"Date"} or \code{"POSIXct"} and ISO 8601 format, depending on \code{time.format}.  }

\value{
  An object of class \code{"WqData"}.
}

\seealso{
  \code{\link{as.Date}}, \code{\link{strptime}}, \code{\link{WqData-class}}
}
\references{
  International Organization for Standardization (2004) ISO 8601. Data elements and interchange formats - Information interchange - Representation of dates and times. \url{http://www.iso.org/iso/en/prods-services/popstds/datesandtime.html}
}
\examples{
# Create new WqData object from sfbay data. First combine date and time
# into a single string after making sure that all times have 4 digits.
sfb <- transform(sfbay, time = substring(10000 + time, 2, 5))
sfb <- transform(sfb, time = paste(date, time, sep = ' '))
sfb <- wqData(sfb, 2:4, 5:12, site.order = TRUE, type = "wide",
    time.format = "\%m/\%d/\%Y \%H\%M")

head(sfb)

# If time of day were not required, then the following would suffice:
sfb <- wqData(sfbay, c(1,3,4), 5:12, site.order = TRUE, type = "wide", 
  time.format = "\%m/\%d/\%Y")

head(sfb)
summary(sfb)
}

\keyword{classes}
\keyword{data}