eofNum <-
function (x, distr = c("normal", "lognormal"), n = nrow(x), reps = 10000) 
{
	# aj 10/21/09 2:49 PM
	# Scree plot enabling scree test, North's rule-of-thumb, rule N
	# Args:
	#   x: data.frame or matrix
	#   distr: distribution type for rule N
	#   n: effective sample size (<= nrow(x))
	#   reps: repetitions for rule N
	# Returns:
	#   ggplot object
		
	require(ggplot2)
	
	# Validate args
	distr <- match.arg(distr)
	
	# Eigenvectors
	eigs <- svd(cor(x))$d
	p <- ncol(x)
	eigsPer <- 100 * eigs/p
	
	# 0.95 confidence limits
	eigsLo <- eigs * (1 - sqrt(2/n))
	eigsHi <- eigs * (1 + sqrt(2/n))
	
	# rule N
	ruleNeigs <- ruleN(n, p, type = distr, reps = reps)
	ruleNok <- eigs > ruleNeigs
	
	# cum. variance
	cumVar <- round(cumsum(eigsPer), 1)
	
	# Plot
	d <- data.frame(rank = 1:p, eigs, eigsLo, eigsHi, ruleNok, 
			cumVar)
	d <- transform(d, cumVarLine = eigsHi + 0.02 * max(eigsHi))
	d <- d[1:min(p, 10), ]
	ggplot(data = d, aes(x = rank, y = eigs, colour = ruleNok)) + 
		geom_errorbar(aes(x = rank, ymin = eigsLo, ymax = eigsHi), width = 0.5) + 
		geom_point(size = 4) + 
		geom_text(aes(x = rank, y = cumVarLine, label = cumVar), size = 3, vjust = 0) + 
		scale_colour_discrete("rule N", breaks = c(TRUE, FALSE), labels = c(expression(p < 0.05), expression(p >= 0.05))) +
		labs(list(x = "Rank", y = "Eigenvalue")) + opts(panel.grid.minor = theme_blank())
}
