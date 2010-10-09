plotTsTile <- 
function(x, title = NULL, trim = TRUE, four = TRUE, loganom = TRUE) {

### Produces a month x year image plot with data in 10 or 4 groups.
### x: monthly time series vector
### trim: If TRUE, leading and trailing NAs are removed.
### title: character string placed above plot
### four: If TRUE, divide into 4 special groups; otherwise deciles.
###       Useful for log-anomaly data.
### loganom: Should x be converted to log-anomaly data?
	
	require(lattice)
	require(zoo)
	
	## trim leading and trailing NAS
	if(trim) {
		x <- as.zoo(x)
		x <- na.trim(x)
		x <- as.ts(x)
	}
	
	## complete partial years by padding with NAs
	sx <- start(x)[1]; ex = end(x)[1]
	x1 <- window(x, start = c(sx, 1), end = c(ex, 12), extend = TRUE)
	
	## set color palette basis
	mypalette <- colorRampPalette(c('darkblue', 'lightblue', 'pink',
		'red'))
	
	## transform to log-anomalies
	if(loganom) x1 <- log10(x1/mean(x1, na.rm = TRUE))
	
	## calculate key if four colors
	if(four){
		mmin <- min(x1, na.rm = TRUE)
		mlo <- mean(x1[x1 < 0], na.rm = TRUE)
		mhi <- mean(x1[x1 > 0], na.rm = TRUE)
		mmax <- max(x1, na.rm = TRUE)
		if(length(unique(c(mmin, mlo, 0, mhi, mmax))) < 5) stop("Breaks
			between the 4 groups are not unique: insufficient unique data.")
		if(mmin >= 0 | mmax <= 0) stop('Data all of one sign; you may want
			four = FALSE or loganom = TRUE.')
		x2 <- cut(x1, breaks = c(mmin, mlo, 0, mhi, mmax), labels = FALSE,
			include.lowest = TRUE)
		cols <- mypalette(4)
		key <- list(col = cols, at = seq(.5, 4.5, 1), labels = list(labels =
			signif(c(mmin, mlo, 0, mhi, mmax), 2), at = seq(.5, 4.5, 1)))
		
	## calculate key if deciles	
	} else { 
		decs <- quantile(x1, probs = seq(0, 1, .1), na.rm = TRUE)
		if(length(unique(decs)) < 11) stop("Breaks between deciles are not
			unique: insufficient unique data.")
		x2 <- cut(x1, breaks = decs, labels = FALSE, include.lowest = TRUE)
		cols <- mypalette(10)
		key <- list(col = cols, at = seq(.5, 10.5, 1), labels = list(labels
			= signif(decs, 2)[seq(1, 11, 2)], at = seq(.5, 10.5, 1)[seq(1, 11,
			2)]))
	}
	
	## create data.frame for graph and plot it
	x3 <- data.frame(yr = floor(time(x1)), mon =
		factor(month.abb[cycle(x1)], levels = month.abb), value = x2)
	levelplot(value~yr+mon, data = x3, aspect = 'iso', 
		main = title, 
		ylim = c(.5, 12.5), 
		panel = function(z, ...) {
			panel.levelplot(z, ...)
			panel.abline(h = 1:11+.5, v = sx:(ex-1)+.5, col = 'white')
		}, 
		scales = list(y = list(at = seq(1, 11, 2), labels = month.abb[seq(1,
			11, 2)])),
		xlab = '', ylab = '', 
		col.regions = cols, 
		colorkey = key
	)	
	
}