

#' Class "DateTime"
#' 
#' A class union of \code{"Date"} and \code{"POSIXct"} classes.
#' 
#' 
#' @name DateTime-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @seealso \code{\link{WqData-class}}
#' @keywords classes
#' @examples
#' 
#' showClass("DateTime")
#' 
NULL





#' Methods for Function phenoAmp
#' 
#' Finds various measures of the amplitude of the annual cycle.
#' 
#' 
#' @name phenoAmp-methods
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(x = \"ts\")")}{See
#' \code{\link{phenoAmp,ts-method}}} \item{list("signature(x = \"zoo\")")}{See
#' \code{\link{phenoAmp,zoo-method}}} }
#' @keywords methods
NULL





#' Phenological amplitude
#' 
#' Finds various measures of the amplitude of the annual cycle, or of some
#' specified season range.
#' 
#' \code{phenoAmp} gives three measures of the amplitude of a seasonal cycle:
#' the range, the variance, and the median absolute deviation, along with the
#' mean and median to allow calculation of other statistics as well.
#' 
#' These measures can be restricted to a subset of the year by giving the
#' desired range of season numbers. This can be useful for isolating measures
#' of, say, the spring and autumn phytoplankton blooms in temperate waters. In
#' the case of a monthly time series, for example, a non-missing value is
#' required for every month or the result will be \code{NA}, so using a period
#' shorter than one year can also help avoid any months that are typically not
#' covered by the sampling program. Similarly, in the case of dated
#' observations, a shorter period can help avoid times of sparse data. The
#' method for time series allows for other than monthly frequencies, but
#' \code{season.range} is always interpreted as months for \code{zoo} objects.
#' 
#' Note that the amplitude is sensitive to the number of samples for small
#' numbers. This could be a problem for \code{zoo} objects if the sample number
#' is changing greatly from year to year, depending on the amplitude measure
#' and the underlying data distribution. So use \code{ts} objects or make sure
#' that the sample number stays more or less the same over time.
#' 
#' \code{\link{tsMake}} can be used to produce \code{ts} and \code{zoo} objects
#' suitable as arguments to this function.
#' 
#' @name phenoAmp 
#' @aliases phenoAmp,ts-method phenoAmp,zoo-method
#' @param x A seasonal time series, or a class \code{zoo} object.
#' @param season.range A vector of two numbers specifying the season range to
#' be considered.
#' @return A matrix of class \code{ts} or \code{zoo} with individual series for
#' the range, variance, median absolute deviation, mean, median and -- in the
#' case of \code{zoo} objects -- number of samples.
#' @seealso \code{\link{phenoPhase}}, \code{\link{tsMake}}
#' @references Cloern, J.E. and Jassby, A.D. (2008) Complex seasonal patterns
#' of primary producers at the land-sea interface. \emph{Ecology Letters}
#' \bold{11,} 1294--1303.
#' @keywords manip ts
#' @examples
#' 
#' y <- sfbayChla[, 's27']
#' phenoAmp(y) # entire year
#' phenoAmp(y, c(1, 6)) # i.e., Jan-Jun only, which yields results for more years
#' 
NULL





#' Methods for Function phenoPhase
#' 
#' Finds various measures of the phase of the annual cycle.
#' 
#' 
#' @name phenoPhase-methods
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(x = \"ts\")")}{See
#' \code{\link{phenoPhase,ts-method}}} \item{list("signature(x =
#' \"zoo\")")}{See \code{\link{phenoPhase,zoo-method}}} }
#' @keywords methods
NULL





#' Phenological phase
#' 
#' Finds various measures of the phase of the annual cycle, or of some
#' specified month range.
#' 
#' \code{phenoPhase} gives three measures of the phasing of a seasonal cycle:
#' the time of the maximum (Cloern and Jassby 2008), the \emph{fulcrum} or
#' center of gravity, and the weighted mean season (Colebrook 1979). The latter
#' has sometimes been referred to in the literature as \dQuote{centre of
#' gravity}, but it is not actually the same. These measures differ in their
#' sensitivity to changes in the seasonal pattern, and therefore also in their
#' susceptibility to sampling variability. The time of maximum is the most
#' sensitive, the weighted mean the least.
#' 
#' These measures can be restricted to a subset of the year by giving the
#' desired range of seasons. This can be useful for isolating measures of, say,
#' the spring and autumn phytoplankton blooms in temperate waters. In the case
#' of a seasonal time series, a non-missing value is required for every season
#' or the result will be \code{NA}, so using a period shorter than one year can
#' also help avoid any seasons that are typically not covered by the sampling
#' program. Similarly, in the case of dated observations, a shorter period can
#' help avoid times of sparse data. The method for time series allows for other
#' than monthly frequencies, but \code{season.range} is always interpreted as
#' months for \code{zoo} objects. The method for time series requires data for
#' all seasons in \code{season.range}. The method for \code{zoo} objects will
#' provide a result regardless of number of sampling days, so make sure that
#' data are sufficient for a meaningful result.
#' 
#' The measures are annum-centric, i.e., they reflect the use of calendar year
#' as the annum, which may not be appropriate for cases in which important
#' features occur in winter and span two calendar years. Such cases can be
#' handled by lagging the time series by an appropriate number of months, or by
#' subtracting an appropriate number of days from the individual dates.
#' 
#' \code{\link{tsMake}} can be used to produce \code{ts} and \code{zoo} objects
#' suitable as arguments to this function.
#' 
#' The default parameters used for the \code{integrate} function in
#' \code{phenoPhase} may fail for certain datasets. Try increasing the number
#' of subdivisions above its default of 100 by adding, for example,
#' \code{subdivisions = 1000} to the arguments of \code{phenoPhase}.
#' 
#' @name phenoPhase
#' @aliases phenoPhase,ts-method phenoPhase,zoo-method
#' @param x A seasonal time series, or a class \code{zoo} object.
#' @param season.range A vector of two numbers specifying the season range to
#' be considered.
#' @param out The form of the output.
#' @param ...  Additional arguments to be passed for changing integration
#' defaults.
#' @return A data frame with columns year, time of the maximum, fulcrum,
#' weighted mean time and -- in the case of \code{zoo} objects -- number of
#' observations. In the case of seasonal time series, the results are all given
#' as decimal seasons of the year. In the case of dated observations, the
#' results can be dates, day of the year, or julian day with an origin of
#' 1970-01-01, depending on the option \code{out}.
#' @seealso \code{\link{phenoAmp}}, \code{\link{tsMake}}
#' @references Cloern, J.E. and Jassby, A.D. (2008) Complex seasonal patterns
#' of primary producers at the land-sea interface. \emph{Ecology Letters}
#' \bold{11,} 1294--1303.
#' 
#' Colebrook, J.M. (1979) Continuous plankton records - seasonal cycles of
#' phytoplankton and copepods in the North Atlantic ocean and the North Sea.
#' \emph{Marine Biology} \bold{51,} 23--32.
#' @keywords manip ts
#' @examples
#' 
#' # ts example
#' y <- sfbayChla[, 's27']
#' p1 <- phenoPhase(y)
#' p1
#' apply(p1, 2, sd, na.rm=TRUE)  # max.time > fulcrum > mean.wt
#' phenoPhase(y, c(3, 10))
#' 
#' # zoo example
#' sfb <- wqData(sfbay, c(1,3,4), 5:12, site.order = TRUE, type = "wide",
#'   time.format = "%m/%d/%Y")
#' y <- tsMake(sfb, focus = 'chl', layer = c(0, 5), type = 'zoo')
#' phenoPhase(y[, 's27'])
#' 
NULL





#' San Francisco Bay water quality data
#' 
#' Selected observations and variables from U.S. Geological Survey water
#' quality stations in south San Francisco Bay. Data include \acronym{CTD} and
#' nutrient measurements.
#' 
#' The original downloaded dataset was modified by taking a subset of six
#' well-sampled stations and the period 1985--2004. Variable names were also
#' simplified. The data frames \code{sfbayStns} and \code{sfbayVars} describe
#' the stations and water quality variables in more detail; they were created
#' from information at the same web site. Note that the station numbers in
#' \code{sfbayStns} have been prefixed with \code{s} to make station codes into
#' legal variable names. \code{sfbayChla} was constructed from the entire
#' downloaded sfbay dataset and encompasses the period 1969--2009.
#' 
#' @name sfbay
#' @aliases sfbay sfbayStns sfbayVars sfbayChla
#' @docType data
#' @format \code{sfbay} is a data frame with 23207 observations (rows) of 12
#' variables (columns):
#' 
#' \tabular{rll}{ \code{[, 1]} \tab \code{date} \tab date\cr \code{[, 2]} \tab
#' \code{time} \tab time\cr \code{[, 3]} \tab \code{stn} \tab station code\cr
#' \code{[, 4]} \tab \code{depth} \tab measurement depth\cr \code{[, 5]} \tab
#' \code{chl} \tab chlorophyll \emph{a}\cr \code{[, 6]} \tab \code{dox.pct}
#' \tab dissolved oxygen\cr \code{[, 7]} \tab \code{spm} \tab suspended
#' particulate matter\cr \code{[, 8]} \tab \code{ext} \tab extinction
#' coefficient\cr \code{[, 9]} \tab \code{sal} \tab salinity\cr \code{[, 10]}
#' \tab \code{temp} \tab water temperature\cr \code{[, 11]} \tab \code{nox}
#' \tab nitrate + nitrite\cr \code{[, 12]} \tab \code{nhx} \tab ammonium\cr }
#' 
#' \code{sfbayStns} is a data frame with 16 observations of 6 variables:
#' 
#' \tabular{rll}{ \code{[, 1]} \tab \code{site} \tab station code\cr \code{[,
#' 2]} \tab \code{description} \tab station description\cr \code{[, 3]} \tab
#' \code{lat} \tab latitude\cr \code{[, 4]} \tab \code{long} \tab longitude\cr
#' \code{[, 5]} \tab \code{depthMax} \tab maximum depth, in m\cr \code{[, 6]}
#' \tab \code{distFrom36} \tab distance from station 36, in km\cr }
#' 
#' \code{sfbayVars} is a data frame with 7 observations of 3 variables:
#' 
#' \tabular{rll}{ \code{[, 1]} \tab \code{variable} \tab water quality variable
#' code\cr \code{[, 2]} \tab \code{description} \tab description\cr \code{[,
#' 3]} \tab \code{units} \tab measurement units\cr }
#' 
#' \code{sfbayChla} is a time series matrix (380 months \code{x} 16 stations)
#' of average 0-5 m chlorophyll \emph{a} concentrations calculated from the
#' data in \code{sfbay}.
#' @source Downloaded from \url{http://sfbay.wr.usgs.gov/access/wqdata} on
#' 2009-11-17.
#' @keywords datasets
#' @examples
#' 
#' data(sfbay)
#' str(sfbay)
#' str(sfbayStns)
#' str(sfbayVars)
#' plot(sfbayChla[, 1:10], main = "SF Bay Chl-a")
#' 
NULL





#' Methods for Function tsMake
#' 
#' Creates a matrix of observations indexed by time.
#' 
#' 
#' @name tsMake-methods
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(x = \"WqData\")")}{See
#' \code{\link{tsMake,WqData-method}}} }
#' @keywords methods
NULL





#' Create time series from water quality data
#' 
#' Creates a matrix time series object from an object of class \code{"WqData"},
#' either all variables for a single site or all sites for a single variable.
#' 
#' When \code{qprob = NULL}, the function averages all included depths for each
#' day, the implicit assumption being that the layer is well-mixed and/or the
#' samples are evenly distributed with depth in the layer. If \code{layer =
#' "max.depths"}, then only the value at the maximum depth for each time, site
#' and variable combination will be used. If no layer is specified, all depths
#' will be used.
#' 
#' The function produces a matrix time series of all variables for the
#' specified site or all sites for the specified variable. If \code{type =
#' "ts.mon"}, available daily data are averaged to produce a monthly time
#' series, from which a quarterly or annual series can be created if needed. If
#' you want values for the actual dates of observation, then set \code{type =
#' "zoo"}.
#' 
#' When \code{qprob} is a number from 0 to 1, it is interpreted as a
#' probability and the corresponding quantile is used to aggregate observations
#' within the specified layer. So to get the maximum, for example, use qprob =
#' 1. If \code{type = "ts.mon"}, the same quantile is used to aggregate all the
#' available daily values.
#' @name tsMake
#' @aliases tsMake,WqData-method
#' @export
#' @importFrom reshape2 dcast
#' @importFrom zoo zoo as.yearmon
#' @importFrom stats as.ts
#' @param object Object of class \code{"WqData"}.
#' @param focus Name of a site or water quality variable.
#' @param layer Number specifying a single depth; a numeric vector of length 2
#' specifying top and bottom depths of layer; a list specifying multiple depths
#' and/or layers; or just the string \code{"max.depths"}.
#' @param type \code{ts.mon} to get a monthly time series, \code{zoo} to get an
#' object of class \code{"zoo"} with individual observation dates.
#' @param qprob quantile probability, a number between 0 and 1.
#' @return A matrix of class \code{"mts"} or \code{"zoo"}.
#' @note The layer list is allowed to include negative numbers, which may have
#' been used in the \code{WqData} object to denote variables that apply to the
#' water column as a whole, such as, say, -1 for light attenuation coefficient.
#' This enables \code{focus = 's27'} and \code{layer = list(-1, c(0, 5))} to
#' produce a time series matrix for station 27 that includes both attenuation
#' coefficient and chlorophyll averaged over the top 5 m. Negative numbers may
#' also have been used in the \code{WqData} object to identify qualitative
#' depths such as \dQuote{near bottom}, which is not uncommon in historical
#' data sets. So data from such depths can be aggregated easily with other data
#' to make these time series.
#' @seealso \code{\link{WqData-class}}
#' @keywords ts
#' @examples
#' 
#' # Create new WqData object
#' sfb <- wqData(sfbay, c(1, 3:4), 5:12, site.order = TRUE,
#'          time.format = "%m/%d/%Y", type = "wide")
#' 
#' # Find means in the 0-10 m layer
#' y <- tsMake(sfb, focus = 's27', layer = c(0, 10))
#' plot(y, main = 'Station 27')
#' # Or select medians in the same layer
#' y1 <- tsMake(sfb, focus = 's27', layer = c(0, 10), qprob = 0.5)
#' plot(y1, main = 'Station 27')
#' # Compare means:medians
#' apply(y/y1, 2, mean, na.rm=TRUE)
#' 
#' # Combine a layer with a single additional depth
#' y <- tsMake(sfb, focus = 'chl', layer = list(c(0, 2), 5))
#' plot(y, main = 'Chlorophyll a, ug/L')
#' 
#' # Use values from the deepest samples
#' y <- tsMake(sfb, focus = 'dox', layer = "max.depths", type = 'zoo')
#' head(y)
#' plot(y, type="h", main = "'Bottom' DO, mg/L")
#' 
NULL





#' Miscellaneous utility functions
#' 
#' A variety of small utilities used in other functions.
#' 
#' \code{date2decyear}: Converts object of class \code{"Date"} to decimal year
#' assuming time of day is noon.
#' 
#' \code{decyear2date}: Converts decimal year to object of class \code{"Date"}.
#' 
#' \code{layerMean}: Acts on a matrix or data frame with depth in the first
#' column and observations for different variables (or different sites, or
#' different times) in each of the remaining columns. The trapezoidal mean over
#' the given depths is calculated for each of the variables. Replicate depths
#' are averaged, and missing values or data with only one unique depth are
#' handled. Data are not extrapolated to cover missing values at the top or
#' bottom of the layer. The result can differ markedly from the simple mean
#' even for equal spacing of depths, because the top and bottom values are
#' weighted by 0.5 in a trapezoidal mean.
#' 
#' \code{leapYear}: \code{TRUE} if \code{x} is a leap year, \code{FALSE}
#' otherwise.
#' 
#' \code{meanSub}: Mean of a subset of a vector.
#' 
#' \code{monthNum}: Converts dates to the corresponding numeric month.
#' 
#' \code{tsSub}: Drops seasons from a matrix or vector time series.
#' 
#' \code{years}: Converts dates to the corresponding numeric years.
#' 
#' 
#' @aliases date2decyear decyear2date layerMean leapYear monthNum
#' @name years
#' @param d A numeric matrix or data frame with depth in the first column and
#' observations for some variable in each of the remaining columns.
#' @param na.rm Should missing data be removed?
#' @param seas An integer vector of seasons to be retained.
#' @param sub An integer vector.
#' @param w A vector of class \code{"Date"}.
#' @param x A numeric vector.
#' @param x1 A matrix or vector time series.
#' @param y A vector of class \code{"Date"} or \code{"POSIX"} date-time.
#' @keywords manip
#' @examples
#' 
#' dates <- as.Date(c("1996-01-01", "1999-12-31", "2004-02-29", "2005-03-01"))
#' date2decyear(dates)
#' 
#' decyear2date(c(1996.0014, 1999.9986, 2004.1626, 2005.1630))
#' 
#' z = c(1,2,3,5,10)  # 5 depths
#' x = matrix(rnorm(30), nrow = 5)  # 6 variables at 5 depths
#' layerMean(cbind(z, x))
#' 
#' leapYear(seq(1500, 2000, 100))
#' leapYear(c(1996.9, 1997))
#' 
#' ## Aggregate monthly time series over Feb-Apr only.
#' aggregate(sfbayChla, 1, meanSub, sub=2:4)
#' 
#' monthNum(as.Date(c('2007-03-17', '2003-06-01')))
#' 
#' ## Ignore certain seasons in a Seasonal Kendall test.
#' c27 <- sfbayChla[, 's27']
#' seaKen(tsSub(c27))  # Aug and Dec missing the most key data
#' seaKen(tsSub(c27, seas = c(1:7, 9:11)))
#' 
#' y = Sys.time()
#' years(y)
#' 
NULL





#' Class "WqData"
#' 
#' A simple extension or subclass of the \code{"data.frame"} class for typical
#' \dQuote{discrete} water quality monitoring programs that examine phenomena
#' on a time scale of days or longer. It requires water quality data to be in a
#' specific \dQuote{long} format, although a generating function
#' \code{\link{wqData}} can be used for different forms of data.
#' 
#' 
#' @name WqData-class
#' @aliases WqData-class [,WqData-method summary,WqData-method
#' plot,WqData-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("WqData", d)}, where \code{d} is a data.frame. \code{d} should
#' have columns named \code{time, site, depth, variable, value} of class
#' \code{"DateTime", "factor", "numeric", "factor", "numeric"}, respectively.
#' @seealso \code{\link{DateTime-class}}, \code{\link{tsMake,WqData-method}},
#' \code{\link{wqData}}
#' @keywords classes
#' @examples
#' 
#' showClass("WqData")
#' # Construct the WqData object sfb as shown in the wqData examples.
#' sfb <- wqData(sfbay, c(1,3,4), 5:12, site.order = TRUE, type = "wide", 
#'               time.format = "%m/%d/%Y")
#' # Summarize the data
#' summary(sfb)
#' # Create boxplot summary of data
#' plot(sfb, vars = c('chl', 'dox', 'spm'), num.col = 2)
#' # Extract some of the data as a WqData object
#' sfb[1:10,]  # first 10 observations
#' sfb[sfb$depth==20,]  # all observations at 20 m
#' 
NULL





#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
#' "wql")\Sexpr{tools:::Rd_package_title("wql")}
#' 
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
#' "wql")\Sexpr{tools:::Rd_package_description("wql")}
#' 
#' The main purpose of \pkg{wql} is to explore seasonal time series through
#' plots and nonparametric trend tests. It was created originally to examine
#' water quality data sets (hence, \dQuote{wql}) but is suitable as a more
#' general purpose set of tools for looking at annual or seasonal time series.
#' 
#' One of the more tedious tasks in exploring environmental data sets is
#' creating usable time series from the original complex data sets, especially
#' when you want many series at will that group data in different ways. So
#' \pkg{wql} also provides a way of transforming data sets to a common format
#' that then allows a diversity of time series to be created quickly. A few
#' functions are specific to the fields of limnology and oceanography.
#' 
#' The plots are designed for easy use, not for publication-quality graphs.
#' Nonetheless, extensive customization is possible by passing options through
#' \code{\ldots{}}, adding annotations in the case of base graphics, and adding
#' layers in the case of \pkg{ggplot2} objects.
#' 
#' Two functions are used mainly for preparing the times series:
#' 
#' \itemize{ \item a function that transforms incoming data to a common data
#' structure in the form of the \code{WqData} class \item a function that
#' easily prepares time series objects from this class }
#' 
#' The \code{WqData} class can be easily adapted to non-aquatic data.
#' Obviously, the \code{depth} field can be used for elevation in atmospheric
#' studies. But more generally, the \code{site} and \code{depth} fields can be
#' used for many two-way classifications and don't need to refer to spatial
#' location.
#' 
#' Some of the time series functions include:
#' 
#' \itemize{ \item a variety of plots to examine changes in seasonal patterns
#' \item nonparametric trend tests \item time series interpolation and related
#' manipulations \item a simple decomposition of a series into different time
#' scales \item phenological analyses \item the use of empirical orthogonal
#' functions to detect multiple independent mechanisms underlying temporal
#' change }
#' 
#' A few functions are specialized for the aquatic sciences:
#' 
#' \itemize{ \item converting between oxygen concentrations and percent
#' saturation \item converting between salinity and conductivity }
#' 
#' The capabilities of \pkg{wql} are more fully explained in the accompanying
#' vignette: \dQuote{wql: Exploring environmental monitoring data}.
#' 
#' @name wql-package
#' @aliases wql-package wql
#' @docType package
#' @author
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
#' "wql")\Sexpr{tools:::Rd_package_author("wql")}
#' 
#' Maintainer:
#' c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
#' "wql")\Sexpr{tools:::Rd_package_maintainer("wql")}
#' @keywords package
NULL





#' Class "zoo"
#' 
#' Registration of S3 class \code{"zoo"} as a formally defined class. Used here
#' to allow the \code{"zoo"} class to appear in method signatures.
#' 
#' 
#' @name zoo-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @seealso \code{\link{phenoAmp}}, \code{\link{phenoPhase}}
#' @keywords classes
#' @examples
#' 
#' showClass("zoo")
#' 
NULL



