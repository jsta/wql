#' Image plot of monthly time series
#' 
#' Monthly values are transformed into deciles or other bins, and corresponding
#' colors are plotted in a month by year matrix.
#' 
#' If \code{four = TRUE}, then \code{x} is first divided into a positive and
#' negative bin. Each bin is then further divided into two bins by its mean,
#' yielding a total of four bins. If \code{four=FALSE}, then \code{x} is simply
#' divided into deciles. In either case, each bin has its own assigned color,
#' with colors ranging from dark blue (smallest numbers) through light blue and
#' pink to red.
#' 
#' Although \code{four = TRUE} can be useful for any data in which 0 represents
#' a value with special significance, it is especially so for data converted
#' into log-anomalies, i.e., \code{log10(x/xbar)} where \code{xbar = mean(x,
#' na.rm=TRUE)}. The mean month then has value 0, and a value of -1, for
#' example, indicates original data equal to one-tenth the mean. Log-anomaly
#' transforms can be particularly appropriate for biological populations, in
#' which variability is often approximately proportional to the mean.
#' 
#' When \code{loganom = TRUE}, the anomalies are calculated with respect to the
#' overall mean month. This differs from, for example, the log-anomaly
#' zooplankton plot of O'Brien et al. (2008), in which a monthly anomaly is
#' calculated with respect to the mean value of the same month. To get the
#' latter behavior, set \code{overall = FALSE}. A further option is to set
#' \code{stat = "median"} rather than the default \code{stat = "mean"}, in
#' which case \code{xbar = median(x, na.rm = TRUE)}, and the positive and
#' negative bins are each divided into two bins by their median instead of
#' mean. Using combinations of these different options can reveal complementary
#' information.
#' 
#' You may want to set \code{square = FALSE} and then adjust the plot window
#' manually if you plan to use the plot in a subsequent layout or if there is
#' too much white space.
#' 
#' @param x monthly time series.
#' @param plot.title plot title.
#' @param legend.title legend title.
#' @param four logical indicating if data should be binned into 4 special
#' groups or into deciles.
#' @param loganom logical indicating if data should be transformed into
#' log-anomalies.
#' @param square logical indicating if tiles should be square.
#' @param legend logical indicating if a legend should be included.
#' @param trim logical indicating if leading and trailing NA values should be
#' removed.
#' @param overall determines whether anomalies are calculated with respect to
#' overall mean or to long-term mean for the same month.
#' @param stat determines whether anomalies are calculated and binned using
#' mean or median.
#' @return An image plot of monthly values classified into either deciles or
#' into four bins as described above (and corresponding object of class
#' \dQuote{ggplot}).
#' @importFrom zoo as.zoo na.trim
#' @importFrom ggplot2 geom_tile scale_x_continuous scale_y_discrete theme_bw coord_equal
#' @importFrom grDevices colorRampPalette
#' @importFrom stats quantile
#' @references O'Brien T., Lopez-Urrutia A., Wiebe P.H., Hay S. (editors)
#' (2008) \emph{ICES Zooplankton Status Report 2006/2007.} ICES Cooperative
#' Research Report 292, International Council for the Exploration of the Sea,
#' Copenhagen, 168 p.
#' @keywords hplot ts
#' @examples
#' 
#' # plot log-anomalies in four bins
#' chl27 = sfbayChla[, 's27']
#' plotTsTile(chl27, legend.title = 'Chl log-anomaly')
#' 
#' # plot deciles
#' plotTsTile(chl27, plot.title = 'SF Bay station 27', legend.title =
#' 	'chlorophyll', four = FALSE, loganom = FALSE, square = FALSE)
#' 
plotTsTile <-
function(x, plot.title = NULL, legend.title = NULL, four = TRUE,
         loganom = TRUE, square = TRUE, legend = TRUE,
         trim = TRUE, overall = TRUE, stat = c("median", "mean")) {

  # Validate args
  if (!is(x, "ts") || is(x, "mts") || !identical(frequency(x), 12))
    stop("x must be a vector of class 'ts' with frequency = 12")
  stat <- match.arg(stat)

  # Define center function
  center <- function(x, type=stat) {
    switch(type,
           mean = mean(x, na.rm=TRUE),
           median = median(x, na.rm=TRUE)
    )
  }

  # trim leading and trailing NAS
  if (trim) {
    x <- as.zoo(x)
    x <- na.trim(x)
    x <- as.ts(x)
  }

  # Complete partial years by padding with NAs
  sx <- start(x)[1]
  ex <- end(x)[1]
  x1 <- window(x, start = c(sx, 1), end = c(ex, 12), extend = TRUE)

  # Transform to log-anomalies
  if (loganom) {
    if (any(x1 <= 0, na.rm = TRUE)) {
      stop("All values must be positive if loganom=TRUE")
    }
    else {
      if (overall) {
        x1 <- x1/center(x1)
      }
      else {
        x1 <- as.matrix(ts2df(x1))
        x1 <- sweep(x1, 2, apply(x1, 2, center), "/")
        x1 <- ts(as.vector(t(x1)), start = c(sx, 1), frequency=12)
      }
    }
    x1 <- log10(x1)
  }

  # Break data.
  if (four) {
    mmin <- min(x1, na.rm = TRUE)
    mlo <- center(x1[x1 < 0])
    mhi <- center(x1[x1 > 0])
    mmax <- max(x1, na.rm = TRUE)
    the.breaks <- c(mmin, mlo, 0, mhi, mmax)
  }
  else {
    the.breaks <- quantile(x1, probs = seq(0, 1, 0.1),
                           na.rm = TRUE)
  }
  len <- length(the.breaks)
  if (length(unique(the.breaks)) < len)
    stop("Breaks between groups are\nnot unique: insufficient unique data.")
  x2 <- cut(x1, breaks = the.breaks, include.lowest = TRUE,
            dig.lab = 2)
  x3 <- data.frame(yr = floor(time(x1)), mon = ordered(month.abb[cycle(x1)],
                   levels = month.abb), value = x2)

  # Plot it.
  mypalette <- colorRampPalette(c("darkblue", "lightblue",
                                  "pink", "red"))
  cols <- mypalette(len - 1)
  p1 <- ggplot(x3, aes_string(x="yr", y="mon", fill="value")) +
    geom_tile(colour = "white", size = 0.25) +
    scale_x_continuous(name = "", expand = c(0, 0)) +
    scale_y_discrete(name = "", expand = c(0, 0)) +
    scale_fill_manual(name = legend.title, values = cols,
                      breaks = levels(x3$value), labels = levels(x3$value)) +
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(title = plot.title)
  if (!legend)
    p1 <- p1 + theme(legend.position = "none")
  if (square)
    p1 + coord_equal()
  else p1
}
