#' Construct an object of class "WqData"
#' 
#' \code{wqData} is a constructor for the \code{"WqData"} class that is often
#' more convenient to use than \code{new}. It converts a data.frame containing
#' water quality data in \dQuote{long} or \dQuote{wide} format to a
#' \code{"WqData"} object. In \dQuote{long} format, observations are all in one
#' column and a second column is used to designate the variable being observed.
#' In \dQuote{wide} format, observations for each variable are in a separate
#' column.
#' 
#' If the data are already in long format, the function has little to do but
#' rename the data fields. If in wide format, the \pkg{reshape2} package is
#' called to \code{melt} the data. The function also removes \code{NA}
#' observations, converts \code{site} to (possibly ordered) factors with valid
#' variable names, and converts \code{time} to class \code{"Date"} or
#' \code{"POSIXct"} and ISO 8601 format, depending on \code{time.format}.
#' 
#' @param data Data frame containing water quality data.
#' @param locus Character or numeric vector designating column names or
#' numbers, respectively, in \code{data} that correspond to \code{time},
#' \code{site} and \code{depth}.
#' @param wqdata In the case of \dQuote{long} data, character or numeric vector
#' designating column names or numbers, respectively, in \code{data} that
#' correspond to \code{variable} and \code{value}. In the case of \dQuote{wide}
#' data, character or numeric vector designating column names or numbers,
#' respectively, in \code{data} that denote water quality variable data.
#' @param site.order If \code{TRUE}, \code{site} factor levels will be ordered
#' in alphanumeric order.
#' @param time.format Conversion specification for \code{time} defined by
#' \acronym{ISO C/POSIX} standard (see \code{\link{strptime}}).
#' @param type Either \dQuote{long} or \dQuote{wide} \code{data}.
#' @return An object of class \code{"WqData"}.
#' @seealso \code{\link{as.Date}}, \code{\link{strptime}},
#' \code{\link{WqData-class}}
#' @references International Organization for Standardization (2004) ISO 8601.
#' Data elements and interchange formats - Information interchange -
#' Representation of dates and times.
#' @keywords classes data
#' @importFrom methods new
#' @export
#' @examples
#' 
#' # Create new WqData object from sfbay data. First combine date and time
#' # into a single string after making sure that all times have 4 digits.
#' sfb <- within(sfbay, time <- substring(10000 + time, 2, 5))
#' sfb <- within(sfb, time <- paste(date, time, sep = ' '))
#' sfb <- wqData(sfb, 2:4, 5:12, site.order = TRUE, type = "wide",
#'     time.format = "%m/%d/%Y %H%M")
#' 
#' head(sfb)
#' tail(sfb)
#' 
#' # If time of day were not required, then the following would suffice:
#' sfb <- wqData(sfbay, c(1,3,4), 5:12, site.order = TRUE, type = "wide", 
#'   time.format = "%m/%d/%Y")
#' 
#' 
wqData <-
function(data, locus, wqdata, site.order, time.format = "%Y-%m-%d",
  type = c("long", "wide")) {    

  # Validate args
  if (length(locus) != 3)
      stop("locus must be of length 3")
  cnames <- colnames(data)
  if (is(wqdata, "character"))
      wqdata <- match(wqdata, cnames, nomatch=0)
  if (any(identical(wqdata, 0)) || max(wqdata) > ncol(data))
      stop("wqdata not in data")
  type <- match.arg(type)

  # Reshape data
  if (identical(type, "long")) {
      data <- data.frame(data[, locus], data[, wqdata])
      names(data) <- c("time", "site", "depth", "variable", "value")
  } else {
      if (identical(length(wqdata), 1L)) {		
          data <- data.frame(data[, locus], variable =
            rep(cnames[wqdata], nrow(data)), value = data[, wqdata])
          names(data)[1:3] <- c("time", "site", "depth")
      } else {
          # Avoid possible duplicate names
          wqd <- data[, wqdata]
          ind <- match(c("time", "site", "depth"), names(wqd), nomatch=0)
          names(wqd)[ind] <- paste(names(wqd)[ind], 1, sep="")
          # Assemble and reshape data
          data <- data.frame(data[, locus], wqd)
          names(data)[1:3] <- c("time", "site", "depth")
          data <- melt(data, id.vars = 1:3)
      }
  }
  
  # Change time to correct format and class if needed
  if (grepl('H', time.format)) {
    data <- within(data, time <- as.POSIXct(time, format =
      time.format))
  } else {
      data <- within(data, time <- as.Date(time, format =
        time.format))
  }
          
  # Remove NAs
  data <- data[!is.na(data$value), ]
  rownames(data) <- 1:nrow(data)
  
  # Remove unneeded factor levels
  data <- within(data, site <- factor(site, ordered = site.order))
  levels(data$site) <- gsub('X','s', make.names(levels(data$site),
    unique = TRUE))
  
  # Make sure variable is a factor
  data <- within(data, variable <- as.factor(variable))

  #
  new(Class="WqData", data)

}
