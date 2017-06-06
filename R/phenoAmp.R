setOldClass("zoo")

setGeneric(
  name = 'phenoAmp',
  def = function(x, ...)
    standardGeneric("phenoAmp")
)

#' @export
setMethod(
  f = "phenoAmp",
  signature = "ts",
  definition = function(x, season.range = c(1, 12)) {

    # get series subset
    seasons <- season.range[1]:season.range[2]
    x1 <- tsSub(x, seas = seasons)

    # get statistics for each year
    range1 <- aggregate(x1, 1, max) - aggregate(x1, 1, min)
    var1 <- aggregate(x1, 1, var)
    mad1 <- aggregate(x1, 1, mad)
    mean1 <- aggregate(x1, 1, mean)
    median1 <- aggregate(x1, 1, median)

    # result
    cbind(range = range1, var = var1, mad = mad1, 
          mean = mean1, median = median1)
  }
)

#' @importFrom stats mad var
setMethod(
  f = "phenoAmp",
  signature = "zoo",
  definition = function(x, month.range = c(1, 12)) {

    # validate args
    if (match(class(index(x)), c("Date", "POSIXct"), nomatch=0) == 0)
      stop('time index must be a Date or POSIXct object')

    # get series subset
    months1 <- month.range[1]:month.range[2]
    x1 <- x[as.numeric(format(index(x), "%m")) %in% months1]

    # get statistics for each year
    range1 <- aggregate(x1, years, max) - aggregate(x1, years, min)
    var1 <- aggregate(x1, years, var)
    mad1 <- aggregate(x1, years, mad)
    mean1 <- aggregate(x1, years, mean)
    median1 <- aggregate(x1, years, median)
    n <- aggregate(x1, years, length)

    #
    cbind(range = range1, var = var1, mad = mad1, mean = mean1,
          median = median1, n)
  }
)
