plotTsAnom <-
function(x, xlab = NULL, ylab = NULL,
         strip.labels = colnames(x), ...) {

  # Validate arguments
  if (!is.ts(x)) stop("x must be of class 'ts'")
  if (missing(xlab)) xlab = ""
  if (missing(ylab)) ylab = ""
  value <- NULL # "global" variables

  if (is.matrix(x)) {  # a matrix time series

    # fill possible spaces in column names so melt+merge will work
    strip.labels <- strip.labels
    colnames(x) <- gsub(' ', '.', colnames(x))

    # Create data frame
    x.mean = apply(x, 2, mean, na.rm=TRUE)
    x.mean.df <- data.frame(variable = factor(names(x.mean)), x.mean)
    d <- data.frame(time=as.Date(time(x)), x)
    d1 <- melt(d, id = 'time')
    d2 <- merge(d1, x.mean.df)
    d3 <- within(d2, variable <- factor(variable, levels = levels(variable),
                                        labels = strip.labels))
    d3 <- na.omit(d3)

    # Plot
    ggplot(d3, aes(x = time, y = value,
                   ymin = ifelse(value >= x.mean, x.mean, value),
                   ymax = ifelse(value >= x.mean, value, x.mean),
                   colour = value >= x.mean)) +
      geom_linerange() +
      geom_hline(aes(yintercept = x.mean), size = 0.25) +
      labs(x = xlab, y = ylab) +
      facet_wrap(~ variable, ...) +
      theme(legend.position='none', panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle=45, colour="grey50"))

  } else {  # a vector time series

    # Create data frame
    x.mean <- mean(x, na.rm = TRUE)
    d1 <- data.frame(time = as.Date(time(x)), x = as.numeric(x), x.mean)
    d1 <- na.omit(d1)
    # Plot
    ggplot(d1, aes(x = time, y = x,
                   ymin = ifelse(x >= x.mean, x.mean, x),
                   ymax = ifelse(x >= x.mean, x, x.mean),
                   colour = x >= x.mean)) +
      geom_linerange() +
      geom_hline(aes(yintercept = x.mean), size = 0.25) +
      labs(x = xlab, y = ylab) +
      theme(legend.position='none', panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle=45, colour="grey50"))
  }
}
