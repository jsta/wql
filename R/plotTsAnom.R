plotTsAnom <- function(x, xlab, ylab, plot.order = colnames(x), strip.labels = colnames(x), ...) {

  require(reshape)
  require(ggplot2)
 
  ## Validate arguments
  if (!is(x, 'ts'))
    stop("x must be of class 'ts'")
  if (missing(xlab)) 
    xlab = ""
  if (missing(ylab)) 
    ylab = ""

  if (is(x, 'mts')) {  # a matrix time series

    ## Create data frame
    x.mean = apply(x, 2, mean, na.rm=TRUE)
    x.mean.df <- data.frame(variable = factor(names(x.mean)), x.mean)
    d <- data.frame(time = as.numeric(time(x)), x)
    d1 <- melt(d, id = 'time')
    d2 <- merge(d1, x.mean.df)
    d3 <- transform(d2, variable = factor(variable, levels = plot.order, labels = strip.labels))
    
    ## Plot
    ggplot(d3, aes(x = time, y = value, ymin = ifelse(value >= x.mean, x.mean, value), ymax = ifelse(value >= x.mean, value, x.mean))) +
      geom_linerange() +
      geom_hline(aes(yintercept = x.mean), colour = "blue") +
      labs(x = xlab, y = ylab) +
      facet_wrap(~ variable, ...) +
      opts(axis.text.x = theme_text(angle=45, colour="grey50"))

  } else {  # a vector time series
    
    ## Create data frame
    x.mean <- mean(x, na.rm = TRUE)
    d1 <- data.frame(time = as.Date(x), x, x.mean)
    
    ## Plot
    ggplot(d1, aes(x = time, y = x, ymin = ifelse(x >= x.mean, x.mean, x), ymax = ifelse(x >= x.mean, x, x.mean))) +
      geom_linerange() +
      geom_hline(aes(yintercept = x.mean), colour = "blue") +
      labs(x = xlab, y = ylab)
  }
}
