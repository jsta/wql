wqData <-
function(data, locus, wqdata, site.order, time.format = "%Y-%m-%d",
  type = c("long", "wide")) {    

  require(reshape)

  ## Validate args
  cnames <- colnames(data)
  if (is(wqdata, "character"))
      wqdata <- match(wqdata, cnames, nomatch=0)
  if (any(wqdata == 0) || max(wqdata) > ncol(data))
      stop("wqdata not in data")
  type <- match.arg(type)

  ## Reshape data
  if (identical(type, "long")) {
      data <- data.frame(data[, locus], data[, wqdata])
      names(data) <- c("time", "site", "depth", "variable", "value")
  } else {
      if (identical(length(wqdata), 1L)) {		
          data <- data.frame(data[, locus], variable =
            rep(cnames[wqdata], nrow(data)), value = data[x, wqdata])
          names(data)[1:3] <- c("time", "site", "depth")
      } else {
          data <- data.frame(data[, locus], data[, wqdata])
          names(data)[1:3] <- c("time", "site", "depth")
          data <- melt(data, id.vars = 1:3)
      }
  }
  
  ## Change time to correct format and class if needed
  if(grepl('H', time.format)) {
    data <- transform(data, time = as.POSIXct(time, format =
      time.format))
  } else {
      data <- transform(data, time = as.Date(time, format =
        time.format))
  }
          
  ## Remove NAs
  data <- subset(data, !is.na(value))
  rownames(data) <- 1:nrow(data)
  
  ## Remove unneeded factor levels
  data <- transform(data, site = factor(site, ordered = site.order))
  levels(data$site) <- gsub('X','s', make.names(levels(data$site),
    unique = TRUE))
  
  ## Make sure variable is a factor
  data <- transform(data, variable = as.factor(variable))

  ##
  new(Class="WqData", data)

}
