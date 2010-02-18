seaRoll <-
function(x, w = 5, rule = 2) {

	# aj 10/22/09 3:36 PM
	# Rolling estimate of sen slope
	# Args:
		# x: seasonal time series
		# w: time window in years
		# rule: rule no. for excluding windows with excessive missing data
	# Returns: a matrix with
		# sen.slope: Sen slope
		# sen.slope.pct: slope as percent of mean
		# p.value: slope significance

	# Validate args
	if (!is(x, 'ts'))
		stop("x must be a 'ts'")
	if (w < 5)
		stop("A minimum window of 5 years is required")

	fr <- frequency(x)		
	sx <- start(x)[1]
	ex <- end(x)[1]
	
	ans <- NULL
	for (yr in sx:(ex-w+1)) {
	
		# Set current window and get slope
		if (fr > 1)	
			x1 <- window(x, s = yr, end = c(yr + w - 1, 12), extend = TRUE) else 
			x1 <- window(x, s = yr, end = yr + w - 1, extend = TRUE)
		sk <- seaKen(x1)
				
		# Make sure enough data are present
		rule.ok <- switch(rule,
			TRUE,
			sum(sk$miss >= 0.5)/fr < 0.5,
		)
		N <- sum(!is.na(x1))
		if (N < 3 * fr || N < 10 || !rule.ok)
			ans1 <- c(NA, NA, NA) else 
			ans1 <- c(sk$sen.slope, sk$sen.slope.pct, sk$p.value)

		ans <- rbind(ans, ans1)		
	}
	
	# Return the results
	colnames(ans) <- c('sen.slope', 'sen.slope.pct', 'p.value')
	rownames(ans) <- (sx + w - 1):ex
	ans[, 1:2] <- signif(ans[, 1:2], 3)
	ans[, 3] <- round(ans[, 3], 3)
	return(ans)
}