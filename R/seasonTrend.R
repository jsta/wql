#' Determine seasonal trends
#' 
#' Finds the trend for each season and each variable in a time series.
#' 
#' The Mann-Kendall test is applied for each season and series (in the case of
#' a matrix). The actual and relative Sen slope (actual divided by median for
#' that specific season and series); the p-value for the trend; and the
#' fraction of missing slopes involving the first and last fifths of the data
#' are calculated (see \code{\link{mannKen}}).
#' 
#' If \code{plot = TRUE}, each season for each series is represented by a bar
#' showing the trend. The fill colour indicates whether \eqn{p < 0.05} or not.
#' If the fraction of missing slopes is 0.5 or more, the corresponding trends
#' are omitted.
#' 
#' Parameters can be passed to the plotting function, in particular, to
#' \code{facet_wrap} in \pkg{ggplot2}. The most useful parameters here are
#' \code{ncol} (or \code{nrow}), which determines the number of columns (or
#' rows) of plots, and \code{scales}, which can be set to \code{"free_y"} to
#' allow the y-axis to change for each time series. Like all \pkg{ggplot2}
#' objects, the plot output can also be customized extensively by modifying and
#' adding layers.
#' 
#' @param x Time series vector, or time series matrix with column names
#' @param plot Should the results be plotted?
#' @param type Type of trend to be plotted, actual or relative to series median
#' @param pval p-value for significance
#' @param ...  Further options to pass to plotting function
#' @importFrom ggplot2 geom_bar scale_fill_manual element_blank
#' @return A data frame with the following fields: \item{series}{series names}
#' \item{season}{season number} \item{sen.slope}{Sen slope in original units
#' per year} \item{sen.slope.rel}{Sen slope divided by median for that specific
#' season and series} \item{p}{p-value for the trend according to the
#' Mann-Kendall test.} \item{missing}{Proportion of slopes joining first and
#' last fifths of the data that are missing}
#' @seealso \code{\link{mannKen}}, \code{\link{plotSeason}},
#' \code{\link[ggplot2]{facet_wrap}}
#' @keywords Graphics ts
#' @export
#' @examples
#' 
#' x <- sfbayChla
#' seasonTrend(x)
#' seasonTrend(x, plot = TRUE, ncol = 4)
#' 
seasonTrend <-
function(x, plot = FALSE, type = c("slope", "relative"), pval = .05, ...) {

  # validate args
  if (!is(x, "ts"))
    stop("x must be a 'ts'")
  type <- match.arg(type)

  # extend to full years
  first = start(x)[1]
  last = end(x)[1]
  fr <- frequency(x)
  x <- window(x, start = first, end = c(last, fr), extend = TRUE)

  # function for single vector
  st <- function(x) {
    x1 <- matrix(x, ncol = fr, byrow = TRUE)
    mannKen(x1)[, c(1:3, 6)]
  }

  # construct a data frame of all trends
  if (!is.matrix(x)) {
    ans <- data.frame(season = as.factor(1:fr), st(x), row.names = 1:fr)
  } else {
    nc <- ncol(x)
    colx <- colnames(x)
    series <- factor(rep(colx, each = fr), levels = colx, ordered = TRUE)
    season <- as.factor(rep(1:fr, times = nc))
    ans0 <- do.call(rbind, lapply(1:nc, function(i) st(x[, i])))
    ans <- data.frame(series, season, ans0, row.names = 1:nrow(ans0))
  }

  if (!plot) return(ans)

  ans[ans$miss >= 0.5, c("sen.slope", "sen.slope.rel")] <- NA
  ans$sig <- ifelse(ans$p.value < pval, TRUE, FALSE)
  v1 <- switch(type, slope = "sen.slope", relative = "sen.slope.rel")
  ylb <- switch(type,
                slope = expression(paste("Trend, units ", yr^{-1})),
                relative = expression(paste("Relative trend, ", yr^{-1})))
  names(ans)[match(v1, names(ans))] <- "trend"
  plt <- ggplot(ans, aes_string(x="season", y="trend", fill="sig")) +
           geom_bar(stat = "identity") +
           scale_fill_manual(name = "", values = c(`FALSE` = "grey65",
             `TRUE` = "dodgerblue"),
             labels = c(bquote(italic(p)>=.(pval)),bquote(italic(p)<.(pval)))) +
           labs(x = "Season", y = ylb) +
           theme(panel.grid.minor = element_blank())
  if (!is.matrix(x)) return(plt)
  plt + facet_wrap(~ series, ...)
}
