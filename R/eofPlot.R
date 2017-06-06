#' Plot EOF analysis results
#' 
#' Plots the rotated empirical orthogonal functions or amplitude time series
#' resulting from \code{\link{eof}}.
#' 
#' When the columns of the original data have a natural order, such as stations
#' along a transect or months of the year, there may be no need to reorder the
#' EOF coefficients. But if there is no natural order, such as when columns
#' represents disparate sites around the world, the plot can be more
#' informative if coefficients are ordered by size (\code{ord = TRUE}).
#' 
#' Coefficients and amplitudes for a given EOF may be more easily interpreted
#' if \code{rev = TRUE}, because the sign of the first coefficient is
#' arbitrarily determined and all the other signs follow from that choice.
#' 
#' @param x result of the function \code{\link{eof}}
#' @param type whether the EOF coefficients or amplitudes should be plotted
#' @param rev logical indicating whether coefficients and amplitudes should be
#' multiplied by \code{-1}
#' @param ord logical indicating whether coefficients should be ordered by size
#' @importFrom ggplot2 geom_hline geom_vline
#' @return A plot of the EOF coefficients or amplitudes.
#' @seealso \code{\link{eof}}
#' @keywords Graphics
#' @examples
#' 
#' # Create an annual matrix time series
#' chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
#' chla1 <- chla1[, 1:12]  # remove stations with missing years
#' 
#' # eofNum (see examples) suggests n = 1
#' e1 <- eof(chla1, n = 1)
#' eofPlot(e1, type = 'coef')
#' eofPlot(e1, type = 'amp')
#' 
eofPlot <-
function(x, type = c("coef", "amp"), rev = FALSE, ord = FALSE) {

  # Validate args
  type <- match.arg(type)
  num <- ncol(x$REOF)

  # Plot
  if (type == "coef") {
    d1 <- x$REOF
    if (ord) d1 <- d1[order(d1[, 1]), ]
    if (rev) d1 <- -d1
    m1 <- melt(d1, varnames = c("variable", "eof"))
    ggplot(m1, aes_string(x = "value", y = "variable")) +
      geom_vline(xintercept = 0, colour = "red", size = 0.2) +
      geom_point(colour = "blue") +
      facet_wrap(~ eof, ncol = num) +
      labs(y = "", x = "Coefficient")
  } else {
    d1 <- x$amplitude
    if (rev) d1 <- -d1
    m1 <- melt(d1, varnames = c("obs", "eof"))
    g1 <- ggplot(m1, aes_string(x = "obs", y = "value")) +
      geom_hline(aes(yintercept = 0), colour = "red", size = 0.2) +
      geom_point(colour = "blue") +
      facet_wrap(~ eof, nrow = num) +
      labs(x = "", y = "Amplitude")
    if (is.factor(m1$obs)) return(g1)
    g1 + geom_line(colour = "blue")
  }
}
