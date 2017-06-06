#' Interpolate or substitute missing time series values
#' 
#' Imterpolates or substitutes missing data in a time series for gaps up to a
#' specified size.
#' 
#' When \code{type = "linear"}, the function performs linear interpolation of
#' any \code{NA} runs of length smaller than or equal to \code{gap}. When
#' \code{gap = NULL}, gaps of any size will be replaced. Does not change
#' leading or trailing \code{NA} runs. This interpolation approach is best for
#' periods of low biological activity when sampling is routinely suspended.
#' 
#' When \code{type = "series.median"} or \code{"series.mean"}, missing values
#' are replaced by the overall median or mean, respectively. This may be
#' desirable when missing values are not allowed but one wants, for example, to
#' avoid spurious enhancement of trends.
#' 
#' When \code{type = "cycle.median"} or \code{type = "cycle.mean"}, missing
#' values are replaced by the median or mean, respectively, for the same cycle
#' position (i.e., same month, quarter, etc., depending on the frequency). This
#' may give more realistic series than using the overall mean or median.
#' 
#' Intended for time series but first three types will work with any vector or
#' matrix. Matrices will be interpolated by column.
#' 
#' @param x object of class \code{"ts"} or \code{"mts"}
#' @param type method of interpolation or substitution
#' @param gap maximum gap to be replaced
#' @export
#' @importFrom zoo na.approx
#' @importFrom stats tsp
#' @return The time series with some or all missing values replaced.
#' @seealso \code{\link{decompTs}}
#' @keywords utilities manip
#' @examples
#' 
#' ### Interpolate a vector time series and highlight the imputed data
#' chl27 <- sfbayChla[, 's27']
#' x1 <- interpTs(chl27, gap = 3)
#' plot(x1, col = 'red')
#' lines(chl27, col = 'blue')
#' x2 <- interpTs(chl27, type = "series.median", gap = 3)
#' plot(x2, col = 'red')
#' lines(chl27, col = 'blue')
#' 
#' ### Interpolate a matrix time series and plot results
#' x3 <- interpTs(sfbayChla, type = "cycle.mean", gap = 1)
#' plot(x3[, 1:10], main = "SF Bay Chl-a\n(gaps of 1 month replaced)")
#' 
interpTs <-
function(x, type = c("linear", "series.median", "series.mean", "cycle.median", 
         "cycle.mean"), gap = NULL) {

	# Validate arguments
  gap.max <- nrow(as.matrix(x)) - 2
  if (is.null(gap))
    gap <- gap.max
	if (is.na(as.numeric(gap)) || gap < 1 || gap > gap.max)
		stop("gap must be a number between 1 and the length - 2")
  type <- match.arg(type)
  if (!is.ts(x) && type %in% c("cycle.median", "cycle.mean"))
    stop("x must be a time series for these types")
 	
  # Define function for replacement by cycle
  tspx <- tsp(x)
  replaceNA <- function (x, stat) {
    x <- ts(x, start = tspx[1], frequency = tspx[3])
    x1 <- window(x, start = start(x)[1], end = c(end(x)[1], 12), extend = TRUE)
    x2 <- matrix(x1, byrow = TRUE, ncol = 12)
    stats <- apply(x2, 2, stat, na.rm = TRUE)
    indx  <- (1:length(x1))[is.na(x1)]
    x3 <- replace(x1, indx, stats[cycle(x1)[indx]])
    window(x3, start = tspx[1], end = tspx[2])
  }
  
	# Define function for vectors
	f1 <- function(x, gap, type) {
		if (sum(!is.na(x)) < 2) {
			x1 <- x
		} else {
	    x1 <- switch(type,
	      linear = na.approx(x, na.rm = FALSE),
	      series.median = ifelse(is.na(x), median(x, na.rm = TRUE), x),
	      series.mean = ifelse(is.na(x), mean(x, na.rm = TRUE), x),
        cycle.median = replaceNA(x, median),
        cycle.mean = replaceNA(x, mean)
	      )
      for (i in seq_len(length(x) - gap)) {
        seq1 <- i:(i + gap)
        if (all(is.na(x[seq1])))
          x1[seq1] <- x[seq1]
      }
		}
		x1
	}

	# Do the interpolation
	if (is.matrix(x)) {
		ans <- apply(x, 2, f1, gap, type)
	} else {
  	ans <- f1(x, gap, type)
	}
  if (is.ts(x)) {
    ans <- ts(ans, start = start(x), frequency = frequency(x))
  }
	ans
}
