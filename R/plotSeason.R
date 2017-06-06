#' Plots seasonal patterns for a time series
#' 
#' Divides the time range for a monthly time series into different eras and
#' plots composites of seasonal pattern. Can also plot each month separately
#' for the entire record.
#' 
#' If \code{num.era} is an integer, the time range is divided into that many
#' equal eras; otherwise, the time range is divided into eras determined by the
#' \code{num.era} vector of years. When plotted \code{"by.era"} and
#' \code{same.plot = FALSE}, the composite patterns are plotted in a horizontal
#' row for easier comparison, which limits the number of periods that can be
#' examined. Boxes based on fewer than half of the maximum possible years
#' available are outlined in red. If \code{same.plot = TRUE}, a single plot is
#' produced with era boxplots arranged by month. When plotted
#' \code{"by.month"}, values for each month are first converted to standardized
#' anomalies, i.e., by subtraction of long-term mean and division by standard
#' deviation. As always, and especially with these plots, experiment with the
#' device aspect ratio and size to get the clearest information.
#' 
#' @param x Monthly time series
#' @param type Plot seasonal pattern by era, or each month for the entire
#' record
#' @param num.era Integer number of eras, or vector of era year breaks
#' @param same.plot Should eras be plotted by month?
#' @param ylab Optional character string label for y-axis
#' @param num.col Number of columns when plotted \code{"by.month"}
#' @return A plot (and the corresponding object of class \code{"ggplot"}).
#' @seealso \code{\link{decompTs}}, \code{\link{seasonTrend}}
#' @keywords Graphics ts
#' @importFrom reshape2 melt
#' @examples
#' 
#' chl27 <- sfbayChla[, 's27']
#' plotSeason(chl27, num.era = c(1978, 1988, 1998, 2008), ylab = 'Stn 27 Chl-a')
#' plotSeason(chl27, num.era = 3, same.plot = FALSE, ylab = 'Stn 27 Chl-a')
#' plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a')
#' 
plotSeason <-
function(x, type = c('by.era', 'by.month'), num.era = 4,
  same.plot = TRUE, ylab = NULL, num.col = 3) {

  # Validate args
  if (!is(x, 'ts') || is(x, 'mts'))
    stop("x must be a single 'ts'")
  type <- match.arg(type)

  # Turn time series into data.frame
  sx <- start(x)[1]
  ex <- end(x)[1]
  x <- window(x, start = sx, end = c(ex, 12), extend = TRUE)
  d <- data.frame(x = as.numeric(x), mon = ordered(month.abb[cycle(x)],
      levels = month.abb), yr = as.numeric(floor(time(x))))

  # Take care of case where num.era is a scalar
  if (length(num.era)==1) {
    if (num.era<1 || round(num.era)!=num.era) {
      stop("num.era must be a whole number > 0")
    } else {
      num.era <- round((0:num.era) * (ex-sx)/num.era + sx, 0)
    }
  }

  if (type == 'by.era') {
    # Break data into eras
    d$era <- cut(d$yr, breaks = num.era, include.lowest = TRUE, dig.lab = 4,
                 ordered_result = TRUE)
    colnames(d)[1] <- 'value'
    d <- na.omit(d)

    # Find missing fraction by month and era
    t0 <- table(d$mon, d$era)
    t1 <- sweep(t0, 2, diff(num.era), '/')
    t2 <- t1 < 0.5
    t3 <- melt(t2)
    colnames(t3) <- c('mon', 'era', 'too.few')
    t4 <- within(t3, {
      mon <- ordered(mon, levels = levels(d$mon))
      if (length(unique(era))>1)
        era <- ordered(era, levels = levels(d$era))
      }
    )
    d1 <- merge(d, t4)

    if (same.plot) {
       # Nest eras within months
       ggplot(d1, aes_string(x="mon", y="value", fill="era")) +
          geom_boxplot(size=.2, position='dodge') +
          labs(x="", y=ylab, fill="Era")
    } else {
       # Nest months within eras
       cols <- c(`TRUE` = "red", `FALSE` = "blue")
       p1 <- ggplot(d1, aes_string(x="mon", y="value", colour="too.few")) +
          geom_boxplot(size = .2) +
          scale_x_discrete('', breaks = month.abb,
                           labels = c('Jan', ' ', ' ', 'Apr', ' ', ' ', 'Jul',
                                      ' ', ' ', 'Oct', ' ', ' ')) +
          scale_y_continuous(ylab) +
          scale_colour_manual("", values=cols, guide="none") +
          theme(panel.grid.minor = element_blank(),
                axis.text.x = element_text(angle=45, colour="grey50"))
       if (length(num.era) > 2)
          p1 <- p1 + facet_wrap(~ era, nrow = 1)
       p1
    }

    } else {
      # Plot standardized anomalies for each month
      x1 <- ts2df(x)
      x2 <- ts(x1, start = start(x))
      plotTsAnom(x2, ylab = ylab, scales = "free_y")
    }
}
