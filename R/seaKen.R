seaKen <-
function(x) {

	# aj 10/22/09 3:16 PM
	# Calculate Seasonal Sen slope and Seasonal Kendall significance test
	# Args:
		# x: ts object
	# Returns: a list with
		# sen.slope: Sen slope
		# sen.slope.pct: slope as percent of mean
		# p.value: slope significance
		# miss: fraction missing in first and last fifths of data for each season
		
	# Validate args
	if (!is(x, 'ts'))
		stop("x must be a 'ts'")
		
	fr <- frequency(x)
	S <- 0
	varS <- 0
	miss <- NULL
	slopes <- NULL
	for (m in 1:fr) {
	
		# select data for current season
		xm <- x[cycle(x) == m]
		tm <- time(x)[cycle(x) == m]
		
		# get kendall statistics for current season
		ken <- mannKen(ts(xm, start = start(x)[1], frequency = 1))
		S <- S + ken$S
		varS <- varS + ken$varS
		miss <- c(miss, ken$miss)
		
		# calculate slopes for current season
		outr=outer(xm, xm, '-')/outer(tm, tm, '-')
		slopes.m=outr[lower.tri(outr)]		
		slopes=c(slopes, slopes.m)		
	}
	
	# calculate sen slope
	sen.slope=median(slopes, na.rm=TRUE)
	sen.slope.pct <- 100 * sen.slope/mean(x, na.rm=TRUE)

	# calculate sen slope significance
	Z <- (S - sign(S))/sqrt(varS)
	p.value <- 2 * pnorm(-abs(Z))
	
	# return results
	names(miss) <- as.character(1:m)
	list(sen.slope = sen.slope, sen.slope.pct = sen.slope.pct, p.value = p.value, miss = round(miss, 3))
}

