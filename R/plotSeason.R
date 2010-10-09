plotSeason <- function(x, type = c('by.era', 'by.month'), num = 4, ylab = NULL, ...) {

    require(reshape)
    require(ggplot2)
    
    ## Validate args
    if (!is(x, 'ts') || is(x, 'mts'))
        stop("x must be a non-matrix 'ts'")
    type <- match.arg(type)
    
  if (type == 'by.era') {
    d <- as.data.frame(x)
    d <- transform(d, mon = ordered(month.abb[cycle(x)],
        levels=month.abb), yr = floor(time(x)))
    d <- transform(d, int = {if (num > 1) cut(yr, breaks = num,
    	include.lowest = TRUE, dig.lab = 4, ordered_result = TRUE) else
    	rep('all', nrow(d))})
    colnames(d)[1] <- 'value'
    d <- na.omit(d)
    
    len <- length(unique(d$yr))/num
    t1 <- table(d$mon, d$int)/len
    t2 <- t1 < 0.5
    t3 <- melt(t2)
    colnames(t3) <- c('mon', 'int', 'too.few')
    t3 <- transform(t3, mon = ordered(mon, levels = levels(d$mon)), int
    	= ordered(int, levels = levels(d$int)))
    d1 <- merge(d, t3)
    
    cols <- c("TRUE" = "red", "FALSE" = "blue") 
    
    p1 <- ggplot(d1, aes(x = mon, y = value, colour = too.few)) +
    geom_boxplot(size = .2) +
        scale_x_discrete('', breaks = month.abb, labels = c('Jan', '',
        	'', 'Apr', '', '', 'Jul', '', '', 'Oct', '', '')) +
        scale_y_continuous(ylab) +
        scale_colour_manual("", values = cols, legend = FALSE) +
        opts(panel.grid.minor = theme_blank()) +
        theme_bw()
    if (num > 1) p1 <- p1 + facet_wrap(~ int, nrow = 1) 
    p1
  } else {
    ggplot(d, aes(x=yr, y=x)) +
        geom_line(size=.5) +
        geom_point(size=2) +
        labs(x="", y=ylab) +
        facet_wrap(~mon, ...) +
        theme_bw()
  }
}