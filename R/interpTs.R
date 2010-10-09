interpTs <-
function(x, gap = 1, type = c("linear", "median", "mean")) {

### Imputes missing data in a time series vector or matrix
### Args:
###   x: data
###   gap: length of largest gap to be interpolated
###   type: interpolation method
### Returns: interpolated series
	
	require(zoo)
	
	## Validate arguments
	if (is.na(as.numeric(gap)) || gap < 1 || gap > nrow(as.matrix(x)) - 2)
		stop("gap must be a number between 1 and the length - 2")
	type = match.arg(type)
	
	## Define function for vectors
	f1 <- function(x, gap, type) {
		if (sum(!is.na(x)) < 2) {
			x1 <- x
		} else {
		    x1 <- switch(type,
		      linear = na.approx(x, na.rm = FALSE),
		      median = ifelse(is.na(x), median(x, na.rm = TRUE), x),
		      mean = ifelse(is.na(x), mean(x, na.rm = TRUE), x)
		      )
        for (i in seq_len(length(x) - gap)) {
          seq1 <- i:(i + gap)
          if (all(is.na(x[seq1])))
            x1[seq1] <- x[seq1]
        }
		}
		x1
	}

	## Do the interpolation
	if (is.matrix(x)) {
		ans <- apply(x, 2, f1, gap, type)
	} else {
  	ans <- f1(x, gap, type)
	}
  if (is.ts(x))
    ans <- ts(ans, start = start(x), frequency = frequency(x))
	ans
}