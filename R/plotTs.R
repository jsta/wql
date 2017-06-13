#' Time series plot
#' 
#' Creates line plot of vector or matrix time series, including any data
#' surrounded by NAs as additional points.
#' 
#' The basic time series line plot ignores data points that are adjacent to
#' missing data, i.e., not directly connected to other observations. This can
#' lead to an uninformative plot when there are many missing data. If one
#' includes both a point and line plot, the resulting graph can be cluttered
#' and difficult to decipher. \code{plotTs} plots only isolated points as well
#' as lines joining adjacent observations.
#' 
#' Options are passed to the underlying \code{facet_wrap} function in
#' \pkg{ggplot2}. The main ones of interest are \code{ncol} for setting the
#' number of plotting columns and \code{scales = "free_y"} for allowing the y
#' scales of the different plots to be independent.
#' @author 
#' Alan Jassby, James Cloern
#' @param x matrix or vector time series
#' @param dot.size size of dots representing isolated data points
#' @param xlab optional x-axis label
#' @param ylab optional y-axis label
#' @param strip.labels labels for individual time series plots
#' @param ...  additional options
#' @importFrom zoo as.Date
#' @importFrom ggplot2 theme element_text geom_line labs geom_point aes
#' @export
#' @return A plot or plots and corresponding object of class \dQuote{ggplot}.
#' @seealso \code{\link{plotTsAnom}}
#' @keywords Graphics ts
#' @examples
#' 
#' # Chlorophyll at 4 stations in SF Bay
#' chl <- sfbayChla[, 1:4]
#' plotTs(chl, dot.size = 1.5, ylab = 'Chl-a', strip.labels = paste('Station',
#'   substring(colnames(chl), 2, 3)), ncol = 1, scales = "free_y")
#' 
plotTs <-
function(x, dot.size = 1, xlab = NULL, ylab = NULL,
        strip.labels = colnames(x), ...) {

  # Validate arguments
  if (!is.ts(x)) stop("x must be of class 'ts'")
  if (missing(xlab)) xlab <- ""
  if (missing(ylab)) ylab <- ""

  if (is.matrix(x)) {  # a matrix time series

    # identify isolated points
    x.forward <- rbind(rep(NA, ncol(x)), x[1:(nrow(x)-1), ])
    x.back <- rbind(x[2:nrow(x), ], rep(NA, ncol(x)))
    iso.pts <- is.na(x.forward) & is.na(x.back) & !is.na(x)
    iso <- data.frame(time = zoo::as.Date(x), 
                      ifelse(iso.pts & !is.na(x), x, NA))
    iso1 <- melt(iso, id = 'time')

    # Create data frame
    d1 <- data.frame(time = as.Date(x), x)
    d2 <- melt(d1, id = 'time')
    d2 <- within(d2, variable <- factor(variable, levels = levels(variable),
                                        labels = strip.labels))
    d2 <- cbind(d2, iso = iso1[, 'value'])

    # Plot
    g1 <- ggplot(d2) +
      geom_line(aes_string(x = "time", y = "value")) +
      facet_wrap(~ variable, ...) +
      labs(x = xlab, y = ylab) +
      theme(axis.text.x = element_text(angle=45, colour="grey50"))
    if (sum(!is.na(d2$iso)) == 0) {
      g1
    } else {
      g1 + geom_point(aes(x = time, y = iso), size = dot.size, na.rm = TRUE)
    }

  } else {  # a vector time series

    # identify isolated points
    x.forward <- c(NA, x[1:(length(x)-1)])
    x.back <- c(x[2:length(x)], NA)
    iso.pts <- is.na(x.forward) & is.na(x.back) & !is.na(x)
    iso <- ifelse(iso.pts, x, NA)

    # Create data frame
    d1 <- data.frame(time = as.Date(x), value = as.numeric(x))
    d2 <- cbind(d1, iso)

    # Plot
    g1 <- ggplot(d2) +
      geom_line(aes_string(x = "time", y = "value")) +
      labs(x = xlab, y = ylab) +
      theme(panel.grid.minor = element_blank())
    if (sum(!is.na(d2$iso)) == 0) {
      g1
    } else {
      g1 + geom_point(aes(x = time, y = iso), size = dot.size, na.rm = TRUE)
    }
  }
}
