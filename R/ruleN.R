ruleN <-
function(n, p, type=c('normal','lognormal'), reps = 10000) {

  # Check validity and set distribution function
  type <- match.arg(type)
  distr <- function(x, type) 
            switch(type, 
                   normal = rnorm(x),
                   lognormal = rlnorm(x)
                   )
  
  # Find eigenvalues
  xdat <- distr(n * p * reps, type)
  dim(xdat) <- c(n, p, reps)
  get.eigs <- function(x) svd(cor(x))$d
  eigs <- apply(xdat, 3, get.eigs)
  
  # Return quantile
  q.95 <- apply(eigs, 1, quantile, probs = 0.95)
  round(q.95, 3)
}
