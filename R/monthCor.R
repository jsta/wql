monthCor = function(x) {

	# aj 10/21/09 4:07 PM
	# Find the month-to-month correlations in a monthly time series x. Useful for deciding where to start the 12-month period for an EOF analysis of seasonal patterns.
	
	# Validate args
	if (!is(x, "ts") || is(x, "mts") || !identical(frequency(x), 12))
		stop("x must be a monthly 'ts'")

	# Calculate cors
	x <- window(x, s = start(x)[1], end = c(end(x)[1], 12), extend = TRUE)
	d1 <- matrix(x, byrow = TRUE, ncol = 12)
	d2 <- cbind(d1, c(NA, d1[-1, 1]))
	d3 <- cor(d2, use = 'pairwise.complete.obs')
	cors <- d3[cbind(1:12, 2:13)]
	
	# Return
	names(cors) <- paste(month.abb, c(month.abb[-1], month.abb[1]), sep = '-')
	round(cors, 2)
}