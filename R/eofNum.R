#' Plot EOF percent variance
#' 
#' Plots the variances associated with empirical orthogonal functions (EOF).
#' Useful for deciding how many EOFs to retain for rotation.
#' 
#' Calculates the eigenvalues from an EOF analysis, as described in
#' \code{\link{eof}}. The eigenvalues are plotted against eigenvalue number
#' (sometimes called a \dQuote{scree plot}), and the cumulative variance as \%
#' of total is plotted over each eigenvalue. The approximate 0.95 confidence
#' limits are depicted for each eigenvalue using North et al.'s (1982)
#' rule-of-thumb, which ignores any autocorrelation in the data. If the
#' autocorrelation structure is assessed separately and can be expressed in
#' terms of effective sample size (e.g., Thiebaux and Zwiers 1984), then
#' \code{n} can be set equal to this number.
#' 
#' There is no universal rule for deciding how many of the EOFs should be
#' retained for rotation (Hannachi et al. 2007). In practice, the number is
#' chosen by requiring a minimum cumulative variance, looking for a sharp break
#' in the spectrum, requiring that confidence limits not overlap, various Monte
#' Carlo methods, or many other techniques. The plot produced here enables the
#' first three methods.
#' 
#' @param x a data frame or matrix, with no missing values
#' @param n effective sample size
#' @param scale.  logical indicating whether the (centered) variables should be
#' scaled to have unit variance
#' @importFrom ggplot2 geom_errorbar geom_text
#' @return A plot of the eigenvectors.
#' @seealso \code{\link{eof}}, \code{\link{interpTs}}, \code{\link{monthCor}},
#' \code{\link{eofPlot}}
#' @references Hannachi, A., Jolliffe, I.T., and Stephenson, D.B. (2007)
#' Empirical orthogonal functions and related techniques in atmospheric
#' science: A review. \emph{International Journal of Climatology} \bold{27,}
#' 1119--1152.
#' 
#' North, G., Bell, T., Cahalan, R., and Moeng, F. (1982) Sampling errors in
#' the estimation of empirical orthogonal functions. \emph{Monthly Weather
#' Review} \bold{110,} 699--706.
#' 
#' Thiebaux H.J. and Zwiers F.W. (1984) The interpretation and estimation of
#' effective sample sizes. \emph{Journal of Climate and Applied Meteorology}
#' \bold{23,} 800--811.
#' @keywords Graphics ts
#' @examples
#' 
#' # Create an annual time series data matrix from sfbay chlorophyll data
#' chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)  # average over each year
#' chla1 <- chla1[, 1:12]  # remove stations with missing years
#' eofNum(chla1)
#' # These stations appear to act as one with respect to chlorophyll
#' # variability on the annual scale because there's one dominant EOF.
#' 
eofNum <-
function(x, n = nrow(x), scale. = TRUE) {

  # eigenvectors
  eigs <- prcomp(x, scale.=scale.)[["sdev"]]^2
  eigs.pct <- 100 * eigs/sum(eigs)

  # 0.95 confidence limits
  eigs.lo <- eigs * (1 - sqrt(2/n))
  eigs.hi <- eigs * (1 + sqrt(2/n))

  # cum. variance
  cumvar <- round(cumsum(eigs.pct), 1)

  # plot
  p <- ncol(x)
  d <- data.frame(rank = factor(1:p), eigs, eigs.lo, eigs.hi, cumvar)
  d <- within(d, cumvar.line <- eigs.hi + 0.02 * max(eigs.hi))
  d <- d[1:min(p, 10), ]
  ggplot(data = d, aes(x = rank, y = eigs)) +
    geom_errorbar(aes(x = rank, ymin = eigs.lo, ymax = eigs.hi),
                  width = 0.3) +
    geom_point(size = 3) +
    geom_text(aes(x = rank, y = cumvar.line, label = cumvar),
              size = 3, vjust = 0) +
    labs(list(x = "Rank", y = "Eigenvalue")) +
    theme(panel.grid.minor = element_blank())
}
