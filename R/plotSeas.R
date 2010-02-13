plotSeas <- function(x, ylab = NULL) {

  require(reshape)
  require(ggplot2)
  
  d <- as.data.frame(x)
  d <- transform(d, mon = ordered(month.abb[cycle(x)], levels=month.abb), yr = floor(time(x)))
  d <- transform(d, int = cut(yr, breaks=4, include.lowest=TRUE, dig.lab=4, ordered_result=TRUE))
  colnames(d)[1] <- 'value'
  d <- na.omit(d)
  
  len <- length(unique(d$yr))/4
  t1 <- table(d$mon, d$int)/len
  t2 <- t1 < 0.5
  t3 <- melt(t2)
  colnames(t3) <- c('mon', 'int', 'too.few')
  t3 <- transform(t3, mon = ordered(mon, levels = levels(d$mon)), int = ordered(int, levels = levels(d$int)))
  d1 <- merge(d, t3)
  
  cols <- c("TRUE" = "indianred3", "FALSE" = "skyblue3") 
  
  ggplot(d1, aes(x = mon, y = value, colour = too.few)) +
    geom_boxplot() +
    scale_x_discrete('', breaks = month.abb, labels = c('Jan', '', '', 'Apr', '', '', 'Jul', '', '', 'Oct', '', '')) +
    scale_y_continuous(ylab) +
    scale_colour_manual("", values = cols, legend = FALSE) +
    facet_wrap(~ int, nrow = 1) +
    opts(panel.grid.minor = theme_blank())
    
 }