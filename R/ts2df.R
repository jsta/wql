#' Convert time series to data frame
#' 
#' Convert monthly time series vector to a year \code{x} month data frame for
#' several possible subsequent analyses. Leading and trailing empty rows are
#' removed.
#' 
#' Our main use of \code{ts2df} is to convert a single monthly time series into
#' a year \code{x} month data frame for EOF analysis of interannual
#' variability.
#' 
#' \code{monthCor} finds the month-to-month correlations in a monthly time
#' series \code{x}. It is useful for deciding where to start the 12-month
#' period for an \code{EOF} analysis (\code{mon1} in \code{ts2df}), namely, at
#' a time of low serial correlation in \code{x}.
#' 
#' @param x monthly time series vector
#' @param mon1 starting month number, i.e., first column of the data frame
#' @param addYr rows are normally labelled with the year of the starting month,
#' but \code{addYr = TRUE} will add 1 to this year number
#' @param omit if \code{TRUE}, then rows with any \code{NA} will be removed.
#' @return An \code{n x 12} data frame, where \code{n} is the number of years.
#' @seealso \code{\link{eof}}
#' @references Craddock, J. (1965) A meteorological application of principal
#' component analysis. \emph{Statistician} \bold{15,} 143--156.
#' @keywords ts manip
#' @importFrom methods is
#' @export
#' @examples
#' 
#' # San Francisco Bay station 27 chlorophyll has the lowest serial 
#' # correlation in Oct-Nov, with Sep-Oct a close second
#' chl27 <- sfbayChla[, 's27']
#' monthCor(chl27)
#' 
#' # Convert to a data frame with October, the first month of the 
#' # local "water year", in the first column
#' tsp(chl27)
#' chl27 <- round(chl27, 1)
#' ts2df(chl27, mon1 = 10, addYr = TRUE)
#' ts2df(chl27, mon1 = 10, addYr = TRUE, omit = TRUE)
#' 
ts2df <-
function(x, mon1 = 1, addYr = FALSE, omit = FALSE) {

	# validate args
	if (!is(x, 'ts') || is(x, 'mts') || !identical(frequency(x), 12))
		stop("x must be a monthly 'ts' vector")
	if (!mon1 %in% 1:12)
		stop("mon1 must be between 1 and 12")
		
	# convert to data.frame
	x1 <- window(x, start = c(start(x)[1] - 1, mon1), end = c(end(x)[1] +
		1, ifelse(mon1 == 1, 12, mon1 - 1)), extend = TRUE)
	d1 <- as.data.frame(matrix(x1, byrow = TRUE, ncol = 12))	
	colnames(d1) <- if (mon1 == 1) month.abb else month.abb[c(mon1:12,
		1:(mon1 - 1))]
	rownames(d1) <- (start(x1)[1] + addYr):(start(x1)[1] + nrow(d1) - 1 +
		addYr)
	
	# trim leading and trailing NA rows, and optionally, rows with any NAs
	d1 <- d1[apply(d1, 1, function(x) !all(is.na(x))),]
	if (omit) d1 <- na.omit(d1)
	d1
}
