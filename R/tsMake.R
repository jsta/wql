setGeneric(
  name = "tsMake",
  def = function(object, ...)
    standardGeneric("tsMake")
)

setMethod(
  f = "tsMake",
  signature = "WqData",
  definition = function(object, focus, layer, type = c("ts.mon", "zoo"),
    qprob = NULL)
  {
    require(reshape)
    require(zoo)

    ## Validate args
    d <- data.frame(object)
    if ( missing(focus) || length(focus) > 1 )
      stop("'focus' must be the name of a single site or       	variable.")
    if (match(focus, d$site, nomatch = 0) > 0) {
      d <- subset(d, site == focus)
      if (nrow(d) == 0) 
        stop("No data for this site.")
    } else {
      if (match(focus, d$variable, nomatch = 0) > 0) {
        d <- subset(d, variable == focus)
        if (nrow(d) == 0) 
          stop("No data for this variable.")
      } else {
        stop("'focus' does not match any sites or variables")
      }
    }
    type <- match.arg(type)

    ## Assemble all depths
    depths <- NULL
    if (missing(layer))
      layer <- list(c(-Inf, Inf))
    if (identical(layer, 'max.depths')) {
      ans <- aggregate(depth ~ time + site + variable, data = d, max, na.rm = TRUE)
      d <- merge(d, ans)
      d$depth <- depths <- max(d$depth, na.rm=TRUE) + 1
    } else {
      if (!is(layer, "list"))
        layer <- list(layer)
      for (el in layer) {
        if ( !is(el, "numeric") || length(el) > 2 )
          stop("layer list items must be numbers or numeric vectors of length 2")
        if (length(el) > 1) {
          depths1 <- unique(subset(d, depth >= el[1] & depth <= el[2])$depth)
          depths <- c(depths, depths1)
        } else {
          depths <- c(depths, el)
        }
      }
    }
    d <- subset(d, depth %in% depths)
    if (nrow(d) == 0) 
      stop("No data for this layer.")

    ## Define aggregation function
    if (is.null(qprob)) {
      f = mean
    } else {
      f = function(x, ...) quantile(x, probs = qprob, ...)
    }

    ## Reshape data
    if (match(focus, d$site, nomatch = 0) > 0) {
      c1 <- cast(d, time ~ variable, fun.aggregate = f, na.rm = TRUE)
    } else {
      c1 <- cast(d, time ~ site, fun.aggregate = f, na.rm = TRUE)
    } 

    ## Create zoo or ts object
    class(c1) <- "data.frame"  # otherwise colnames are dropped
    z1 <- zoo(c1[, -1], c1[, 1])
    if (type == 'ts.mon') {
      z1 <- aggregate(z1, as.yearmon, f, na.rm = TRUE)
      if (is.null(nrow(z1)) || nrow(z1) > 1) z1 <- as.ts(z1)
    }
    z1
  }
)
