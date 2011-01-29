setClass(
  Class = 'WqData', 
  contains = 'data.frame', 
  validity = function(object) {
    if (!identical(object@names[1:5], c("time", "site", "depth",
    	"variable", "value")))
      stop("columns are not all named correctly")
    if (!all(
        is(object$time, "DateTime"),
        is(object$site, "factor"),
        is(object$depth, "numeric"),
        is(object$variable, "factor"),
        is(object$value, "numeric")))
      stop("columns are not all of correct class")
  }
)


setMethod(
  f = "summary", 
  signature = "WqData", 
  definition = function(object, ...) {
    trange <- range(as.Date(object$time), na.rm = TRUE)
    cat("date range: ", paste(trange[1], "to", trange[2]), "\n\n")
    nums <- table(object$site, object$variable)
    quarts <- tapply(object$value, object$variable, summary)
    quarts1 <- matrix(unlist(quarts), byrow = TRUE,  ncol = 6)
    colnames(quarts1) <- names(quarts[[1]])
    rownames(quarts1) <- names(quarts)[seq_len(nrow(quarts1))]
    list(observations = nums, quartiles = quarts1)
  }
)


setMethod(
  f = "plot",
  signature = "WqData",
  definition =  function(x, y = "missing", vars, num.col = NULL) {
    if (missing(vars)) 
      vars = unique(x$variable)[1:10]
    require(ggplot2)
    d <- subset(as.data.frame(x), variable %in% vars)
    ggplot(d, aes(x = value, y = site, z = variable)) + 
      geom_point(colour = 'blue', shape = 1) +
      facet_wrap(~ variable, scales = "free_x", ncol = num.col)
  }
)
