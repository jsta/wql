plotTsAnom <- function(x, xlab, ylab) {
   require(ggplot2)
   if (!is(x, 'ts') || is(x, 'mts')) stop("x must be a vector time series")
   if (missing(xlab)) xlab = ""
   if (missing(ylab)) ylab = ""
   x.mean = mean(x, na.rm=TRUE)
   d <- data.frame(time = time(x), x, x.mean)
   ggplot(d, aes(x=time, y=x, ymin = ifelse(x >= x.mean, x.mean, x), ymax = ifelse(x >= x.mean, x, x.mean))) +
      geom_linerange() +
      geom_hline(yintercept = x.mean) +
      labs(x = xlab, y = ylab)
}
