\name{tsMake}
\Rdversion{1.1}
\alias{tsMake}
\alias{tsMake,WqData-method}

\title{
Create time series from water quality data
}
\description{
Creates a matrix time series object from an object of class \code{"WqData"}, either all variables for a single site or all sites for a single variable.
}

\usage{
\S4method{tsMake}{WqData}(object, focus, layer, type = c("ts.mon", "zoo")) 
}

\arguments{
  \item{object}{
Object of class \code{"WqData"}.
}
  \item{focus}{
Name of a site or water quality variable.
}
  \item{layer}{
Number specifying a depth, numeric vector specifying top and bottom depths of layer, or list specifying multiple depths and/or layers. 
}
  \item{type}{
\code{ts.mon} to get a monthly time series, \code{zoo} to get an object of class \code{"zoo"} with individual observation dates.
}  
}

\details{
The function currently averages all included depths for each day, the implicit assumption being that the layer is well-mixed and/or the samples are evenly distributed with depth in the layer. It produces a matrix time series of all variables for the specified site or all sites for the specified variable. A monthly time series is produced, from which a quarterly or annual series can be created if needed. If no layer is specified, all depths will be used. If you want values for the actual dates of observation, then set \code{type = "zoo"}.
}

\note{
The layer list can include negative numbers, which may have been used in the \code{WqData} object to denote variables that apply to the water column as a whole, such as, say, -1 for light attenuation coefficient. This enables \code{focus = 's27'} and \code{layer = list(-1, c(0, 5))} to produce a time series matrix for station 27 that includes both attenuation coefficient and chlorophyll averaged over the top 5 m. Negative numbers may also have been used in the \code{WqData} object to identify qualitative depths such as \dQuote{near bottom}, which is not uncommon in historical data sets. So data from such depths can  be aggregated easily with other data to make these time series.
}

\value{
A matrix of class \code{"mts"} or \code{"zoo"}.
}

\seealso{
\code{\link{WqData-class}}
}

\examples{
# Create new WqData object
sfb <- wqData(sfbay, c(1, 3:4), 5:12, site.order = TRUE, 
         time.format = "\%m/\%d/\%Y", type = "wide")

y <- tsMake(sfb, focus = 's27', layer = c(0, 2)) 
plot(y, main = 'Station 27')

y <- tsMake(sfb, focus = 'chl', layer = list(c(0, 2), 5))
plot(y, main = 'Chlorophyll a, ug/L')

y <- tsMake(sfb, focus = 'chl', layer = c(0, 5), type = 'zoo')
head(y)
plot(y, main = 'Chlorophyll a, ug/L')
}

\keyword{ts}