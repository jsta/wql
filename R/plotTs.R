plotTs <- function(x, xlab, ylab, dot.size = 1, plot.order = colnames(x), strip.labels = colnames(x), ...) {

  require(reshape2)
  require(ggplot2)
  
  ## Validate arguments
  if (!is(x, 'ts'))
    stop("x must be of class 'ts'")
  if (missing(xlab)) 
    xlab = ""
  if (missing(ylab)) 
    ylab = ""

  if (is(x, 'mts')) {  # a matrix time series
    
    # Fix possible spaces in column names
    strip.labels <- strip.labels
    colnames(x) <- gsub(' ','.', colnames(x))
    plot.order <- gsub(' ','.', plot.order)

    ## identify isolated points
    x.forward <- rbind(rep(NA, ncol(x)), x[1:(nrow(x)-1), ])
    x.back <- rbind(x[2:nrow(x), ], rep(NA, ncol(x)))
    iso.pts <- is.na(x.forward) & is.na(x.back) & !is.na(x)
    iso <- data.frame(time = as.Date(x), ifelse(iso.pts & !is.na(x), x, NA))
    iso1 <- melt(iso, id = 'time')
    
    ## Create data frame
    d1 <- data.frame(time = as.Date(x), x)
    d2 <- melt(d1, id = 'time')
    d2 <- within(d2, variable <- factor(variable, levels = plot.order, labels = strip.labels))
    d2 <- cbind(d2, iso = iso1[, 'value'])
    
    ## Plot
    g1 <- ggplot(d2) +
      geom_line(aes_string(x = "time", y = "value")) +
      facet_wrap(~ variable, ...) +
      labs(x = xlab, y = ylab) +
      theme(axis.text.x = element_text(angle=45, colour="grey50"))
    if (sum(!is.na(d2$iso)) == 0) {
      g1
    } else {
      g1 + geom_point(aes(x = time, y = iso), size = dot.size)
    }

  } else {  # a vector time series

    ## identify isolated points
    x.forward <- c(NA, x[1:(length(x)-1)])
    x.back <- c(x[2:length(x)], NA)
    iso.pts <- is.na(x.forward) & is.na(x.back) & !is.na(x)
    iso <- ifelse(iso.pts, x, NA)
    
    ## Create data frame
    d1 <- data.frame(time = as.Date(x), value = x)
    d2 <- cbind(d1, iso)
    
    ## Plot
    g1 <- ggplot(d2) +
      geom_line(aes_string(x = "time", y = "value")) +
      labs(x = xlab, y = ylab)
    if (sum(!is.na(iso)) == 0) {
      g1
    } else {
      g1 + geom_point(aes(x = time, y = iso), size = dot.size)
    }
  }
}
