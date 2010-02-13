ruleN <-
    function(n, p, type=c('normal','lognormal'), reps = 10000) {

  # aj 10/21/09 3:07 PM
  # Repeatedly compute singular values of the correlation matrix for
  # an n by p matrix of a random variable and return the 0.95 quantiles
  # Args:
  #  n: cases
  #  p: variables
  #  type: distribution function
  #  reps: repetitions
  # Returns:
  #  vector of p 0.95 quantiles
        
  #     Check validity and set distribution function
	type <- match.arg(type)
	distr <- function(x, type) 
            switch(type, 
                   normal = rnorm(x),
                   lognormal = rlnorm(x)
                   )
	
  #     Find eigenvalues
	xdat <- distr(n * p * reps, type)
	dim(xdat) <- c(n, p, reps)
	get.eigs <- function(x) svd(cor(x))$d
	eigs <- apply(xdat, 3, get.eigs)
	
  #     Return quantile
	q.95 <- apply(eigs, 1, quantile, probs = 0.95)
	round(q.95, 3)
    }
