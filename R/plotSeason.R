plotSeason <-
function(x, type = c('by.era', 'by.month'), num.era = 4,
  same.plot = TRUE, ylab = NULL, num.col = 3) {

   require(reshape)
   require(ggplot2)
   
   ## Validate args
   if (!is(x, 'ts') || is(x, 'mts'))
      stop("x must be a single 'ts'")
   type <- match.arg(type)
   
   d <- data.frame(x = as.numeric(x), mon = ordered(month.abb[cycle(x)],
   	levels = month.abb), yr = as.numeric(floor(time(x))))

   if (type == 'by.era') {
      ## Break data into eras
      d <- transform(d, int = {if (num.era > 1) cut(yr, breaks = num.era,
      	include.lowest = TRUE, dig.lab = 4, ordered_result = TRUE) else
      	rep('all', nrow(d))})
      colnames(d)[1] <- 'value'
      d <- na.omit(d)
      
      ## Find missing fraction by month and era
      len <- length(unique(d$yr))/num.era
      t1 <- table(d$mon, d$int)/len
      t2 <- t1 < 0.5
      t3 <- melt(t2)
      colnames(t3) <- c('mon', 'int', 'too.few')
      t3 <- transform(t3, mon = ordered(mon, levels = levels(d$mon)),
         int = ordered(int, levels = levels(d$int)))
      d1 <- merge(d, t3)
   
      if (same.plot) {
         ## Nest eras within months
         ggplot(d1, aes(x=mon, y=value, fill=int)) +
            geom_boxplot(size=.2, position='dodge') +
            labs(x="", y=ylab, fill="Era")         
      } else {
         ## Nest months within eras
         cols <- c(`TRUE` = "red", `FALSE` = "blue") 
         p1 <- ggplot(d1, aes(x = mon, y = value, colour = too.few)) +
            geom_boxplot(size = .2) + 
            scale_x_discrete('', breaks = month.abb, labels = c('Jan',
            	'', '', 'Apr', '', '', 'Jul', '', '', 'Oct', '', '')) +
            scale_y_continuous(ylab) +
            scale_colour_manual("", values = cols, legend = FALSE) +
            opts(panel.grid.minor = theme_blank())
         if (num.era > 1) 
            p1 <- p1 + facet_wrap(~ int, nrow = 1) 
         p1
      }
   } else {
      ## Plot standardized anomalies for each month
      x1 <- matrix(x, ncol=12, byrow=TRUE)
      x2 <- scale(x1)
      x3 <- as.numeric(t(x2))
      d <- transform(d, x = as.numeric(x3))
      ggplot(d, aes(x=yr, y=x)) +
         geom_bar(colour = 'grey', stat='identity') + 
         opts(axis.text.x=theme_text(angle=90, hjust=1)) +
         labs(x="", y=ylab) +
         facet_wrap(~mon, ncol=num.col)
    }
}