decompTs <- 
function(x, startyr, endyr, event = TRUE, type = c('mult', 'add')) 
{   

### Decomposes time series into annual, seasonal, and event series
### Args: 
###   x, ts
###   startyr, starting year
###   endyr, ending year
###   event, whether or not an event series should be calculated
###   type, multiplicative or additive decomposition
### Returns: mts consisting of decomposition components as ts
    
  ## Validate input
  if(!is(x, 'ts') || is(x, 'mts') || !identical(frequency(x), 12))
    stop("x must be a monthly 'ts' vector")
  type = match.arg(type)
  
  ## Set the time window
  if (missing(startyr)) 
    startyr <- start(x)[1]
  if (missing(endyr)) 
    endyr <- end(x)[1]
  x <- window(x, start = c(startyr, 1), end = c(endyr, 12), extend=TRUE)
  
  ## Choose the arithmetic operations, depending on type
  `%/-%` <- function(x, y) 
    switch(type, 
           mult = x/y,
           add = x-y
           )
  `%*+%` <- function(x, y)
    switch(type, 
           mult = x*y,
           add = x+y
           )
  
  ## Long-term mean
  grandmean <- mean(x, na.rm=TRUE)

  ## Annual component
  annualmean <- aggregate(x, 1, mean, na.rm=TRUE)
  annualmeanReps <- as.vector(t(matrix(rep(annualmean, 12), ncol=12)))
  annualmeanTs <- ts(annualmeanReps, s=startyr, f=12) %/-% grandmean   

  ## Remaining components
  if(event) {
  ## Monthly component
    x2 <- matrix(x, nrow=12)
    monthdev <- sweep(x2, 2, annualmean, '%/-%')
    monthmean <- apply(monthdev, 1, mean, na.rm=TRUE)
    seasonal <- ts(rep(monthmean, endyr - startyr + 1), s=startyr, f=12)

  ## Events component
    resids <- sweep(monthdev, 1, monthmean, '%/-%')
    events <- ts(as.vector(resids), s=startyr, f=12)
  }
  else {
  ## Monthly component
    seasonal <- x/(grandmean %*+% annualmeanTs)
  } 
  
  ## Prepare output
  if (event) {
    dcomp <- ts.union(x, grandmean, annual=annualmeanTs, seasonal,
    	events)
  } else 
  dcomp <- ts.union(x, grandmean, annual=annualmeanTs, seasonal)
  colnames(dcomp)[1] <- 'original'
  dcomp
}
