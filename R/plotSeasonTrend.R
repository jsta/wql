plotSeasonTrend<- function(x, yrs, type = c("slope", "slope.pct"),
	method = c("mk", "lin"), plot = TRUE, miss = TRUE, leg = TRUE, ...)
	{

### Plot trends by season
### Args: 
###     x, ts or mts
###     yrs, vector of first and last year
###     type, divide slope by mean or not
###     method, Mann-Kendall or linear
###     plot, or list results
###     miss, for info about extent of missing
###     leg, for a legend
### Returns: a plot, or else a data.frame with the following columns
###     trend, linear slope as units/yr or %/yr
###     p, significance of slope
###     missing, extent of NAs in first and last fifth of data
###     season, number of season
###     tsname, name of time series
    
    require(ggplot2)
    
    ## Validate args
    if (!is(x, 'ts'))
        stop("x must be a 'ts'")
    if (!(length(yrs) == 2)) 
        stop("'yrs' must be of length 2, a starting and ending year")
    if (is(x, 'mts') && is.null(colnames(x)))
        stop("Time series matrix must have column names.")
    type <- match.arg(type)
    method <- match.arg(method)
    
    ## To handle single time series
    x0 <- ts.union(x, x)
    nc <- ncol(x0)/2
    
    ## Trend function for single series
    if (method == 'mk') {
        trend <-  function(x) unlist(mannKen(x)[c(1:3, 6)])
    } else {
        trend <-  function(x) {
            lm1 <- summary(lm(x ~ time(x)))[['coefficients']]
            slope <- lm1['time(x)', 'Estimate']
            slope.pct <- 100 * slope / mean(x, na.rm = TRUE)
            p.value <- lm1['time(x)', 'Pr(>|t|)']
            len <- length(x)
            fifth <- ceiling(len/5)
            miss <- sum(is.na(x[c(1:fifth, (len - fifth + 1):len)]))
            c(slope, slope.pct, p.value, miss)
        }
    }
    
    ## Gather trends for each ts and season
    fr <- frequency(x)
    ans <- as.data.frame(matrix(nrow = fr * nc, ncol = 4))
    colnames(ans) <- c('slope', 'slope.pct', 'p',  'missing')
    x1 <- window(x0, s = yrs[1], end = c(yrs[2], fr), extend = TRUE)
    for (j in 1:nc) {
        xj <- x1[, j]
        for (i in 1:fr) {
            xij <- ts(xj[cycle(x1) == i], s = yrs[1], end = yrs[2])
            ans[i + (j - 1) * fr, ] <- trend(xij)
        }
    }
    
    ## Identify results with ts names
    tsnames <- {if (nc == 1) deparse(substitute(x)) else colnames(x)}
    ts.id <- factor(rep(tsnames, each = fr), levels = tsnames, ordered = TRUE)
    ans1 <- data.frame( trend = ans[, type], ans[, 3:4], season =
    	ordered(rep(1:fr, times = nc), levels = 1:fr), ts.id)
    	
    ## Plot or tabulate results   	
    if (plot) {
        ans2 <- transform(na.omit(ans1), p.value = ifelse(p < 0.05, 
            '< 0.05', '>= 0.05'), miss.value = ifelse(missing < .5, 
            '< 0.5', '>= 0.5'))
        nr <- nrow(ans2)
        p1 <- ggplot(ans2, aes(season, trend)) +
            geom_hline(yintercept = 0) +
            scale_x_discrete(breaks = as.character(seq(1, fr, 2)),
            	labels = seq(1, fr, 2)) +
            labs(list(x = "Season", y = ifelse(type == 'slope',
            	expression(paste('Trend (units ', yr^{-1}, ')')),
            	expression(paste('Trend (% ', yr^{-1}, ')')))) ) +
            theme_bw()
        if (miss) {	
            p1 <- p1 + geom_point(aes(colour = p.value, shape =
            	miss.value)) +
            scale_colour_manual('p value', values = c('>= 0.05' = 'red',
            	'< 0.05' = 'blue')) +
            scale_shape_manual('missing', values = c('>= 0.5' = 1, 
                '< 0.5' = 16))
        } else {
            p1 <- p1 + geom_point(aes(colour = p.value)) +
            scale_colour_manual('p value', values = c('>= 0.05' =
        	    'red', '< 0.05' = 'blue')) 
        }
        if (nc > 1) p1 <- p1 + facet_wrap(~ ts.id, ...)
        if (!leg) p1 <- p1 + opts(legend.position = "none")
        p1
    } else {
        ans1
    }
}
