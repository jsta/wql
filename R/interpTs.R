interpTs <-
function(x, gap = 1) {

  # adj 11/6/09 3:51 PM
  # Imputes missing data in a time series vector or matrix by linear interpolation
  # Args:
  #   x: data
  #   gap: length of largest gap to be interpolated
  # Returns: interpolated vector or matrix
	
	require(zoo)
	
	# Validate arguments
	if (is.na(as.numeric(gap)) || gap < 1 || gap > nrow(as.matrix(x)) - 2)
		stop("gap must be a number between 1 and the length - 2")
	
	# Define function for vectors
	f1 <- function(x, gap) {
		if (sum(!is.na(x)) < 2) {
			x1 <- x
		} else {
			if (gap > length(x))
				stop("gap cannot be longer than x")		
			x1 <- na.approx(x, na.rm = FALSE)
			for (i in seq_len(length(x) - gap)) {
				seq1 <- i:(i + gap)
				if (all(is.na(x[seq1])))
					x1[seq1] <- x[seq1]
			}
		}
		x1
	}

	# Do the interpolation
	if (is(x, "matrix")) {
		ans <- apply(x, 2, f1, gap)
		if (is(x, "mts")) {
			ans <- ts(ans, start = start(x), frequency = frequency(x))
		} else {}
	} else {
		ans <- f1(x, gap)
	}
	
	ans
}