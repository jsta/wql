setGeneric(
  name = "tsMake", 
  def = function(object, ...)
    standardGeneric("tsMake")
)


setMethod(
  f = "tsMake",
  signature = "WqData",
  definition = function(object, focus, layer, type = c('ts.mon', 'zoo')) {
  
    require(reshape)
    require(zoo)
    
    ## Validate args
    if (missing(focus))
      stop("'focus' must be the name of a single site or single variable")
    type <- match.arg(type)
    
    ## Reshape data
    if (match(focus, object$site, nomatch = 0) > 0) {
      x1 <- with(object, object[depth >= layer[1] & depth <= layer[2] & site == focus, ])
      c1 <- cast(x1, time ~ variable, fun.aggregate = mean, na.rm = TRUE)
    } else {
      if (match(focus, object$variable, nomatch = 0) > 0) {
        x1 <- with(object, object[depth >= layer[1] & depth <= layer[2] & variable == focus, ])
        c1 <- cast(x1, time ~ site, fun.aggregate = mean, na.rm = TRUE)
      } else {
        stop("'focus' does not match any sites or variables")
      }
    }
    class(c1) <- "data.frame"  # necessitated by conflict with cast_df?
    
    z1 <- zoo(c1[, -1], c1[, 1])
    if (type == 'ts.mon') {
      z1 <- aggregate(z1, as.yearmon, mean, na.rm = TRUE)
      z1 <- as.ts(z1)
    }
    
    z1
    
  }
  
)