setGeneric(
  name = "tsMake", 
  def = function(object, ...)
    standardGeneric("tsMake")
)

setMethod(
  f = "tsMake",
  signature = "WqData",
  definition = function(object, focus, layer, type = c('ts.mon', 'zoo'))
  {
    require(reshape)
    require(zoo)
    
    ## Validate args
    if (missing(focus) || length(focus) > 1)
      stop("'focus' must be the name of a single site or single
      	variable.")
    type <- match.arg(type)
    
    ## Assemble all depths
    d <- as.data.frame(object)
    if (missing(layer)) layer = list(c(-Inf, Inf))
    if (!is.list(layer)) layer = list(layer)
    depths <- NULL
    for (el in layer) {
        if (length(el) > 1) {
            depths1 <- subset(d, depth >= el[1] & depth <= el[2])$depth
            depths <- c(depths, depths1)
        } else { 
            depths <- c(depths, el)
        }
    }
    
    ## Reshape data
    if (match(focus, d$site, nomatch = 0) > 0) {
      d <- subset(d, depth %in% depths & site == focus)
      if (nrow(d) == 0) stop("No data for this site and layer.")
      c1 <- cast(d, time ~ variable, fun.aggregate = mean, na.rm = TRUE)
    } else {
      if (match(focus, d$variable, nomatch = 0) > 0) {
        d <- subset(d, depth %in% depths & variable == focus)
        if (nrow(d) == 0) stop("No data for this variable and layer.")
        c1 <- cast(d, time ~ site, fun.aggregate = mean, na.rm = TRUE)
      } else {
        stop("'focus' does not match any sites or variables")
      }
    }

    class(c1) <- "data.frame"  # necessitated by conflict with cast_df?
    z1 <- zoo(c1[, -1], c1[, 1])
    if (type == 'ts.mon') {
      z1 <- aggregate(z1, as.yearmon, mean, na.rm = TRUE)
      if (is.null(nrow(z1)) || nrow(z1) > 1) z1 <- as.ts(z1)
    } 
    z1
  }
)