eofNum <-
function (x, distr = c("normal", "lognormal"), n = nrow(x), reps =
	10000) {

  # Validate args
  distr <- match.arg(distr)
  
  # Eigenvectors
  eigs <- svd(cor(x))$d
  p <- ncol(x)
  eigs.per <- 100 * eigs/p
  
  # 0.95 confidence limits
  eigs.lo <- eigs * (1 - sqrt(2/n))
  eigs.hi <- eigs * (1 + sqrt(2/n))
  
  # rule N
  rulen.eigs <- ruleN(n, p, type = distr, reps = reps)
  rulen.ok <- eigs > rulen.eigs
  
  # cum. variance
  cumvar <- round(cumsum(eigs.per), 1)
  
  # Plot
  d <- data.frame(rank = 1:p, eigs, eigs.lo, eigs.hi, rulen.ok, 
    cumvar)
  d <- within(d, cumvar.line <- eigs.hi + 0.02 * max(eigs.hi))
  d <- d[1:min(p, 10), ]
  ggplot(data = d, aes(x = rank, y = eigs, colour = rulen.ok)) + 
    geom_errorbar(aes(x = rank, ymin = eigs.lo, ymax = eigs.hi), width =
    	0.5) +
    geom_point(size = 4) + 
    geom_text(aes(x = rank, y = cumvar.line, label = cumvar), size = 3,
    	vjust = 0) +
    scale_colour_discrete("rule N", breaks = c(TRUE, FALSE), labels =
      c(expression(p < 0.05), expression(p >= 0.05))) +
    labs(list(x = "Rank", y = "Eigenvalue")) + theme(panel.grid.minor =
    	element_blank()) +
    theme_bw()
}
