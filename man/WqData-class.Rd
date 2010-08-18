\name{WqData-class}
\Rdversion{1.1}
\docType{class}
\alias{WqData-class}
\alias{subset,WqData-method}
\alias{summary,WqData-method}
\alias{plot,WqData-method}

\title{Class "WqData"}
\description{A simple extension or subclass of the \code{"data.frame"} class for typical \dQuote{discrete} water quality monitoring programs that examine phenomena on a time scale of days or longer. It requires water quality data to be in a specific \dQuote{narrow} format, although a generating function \code{\link{wqData}} can be used for different forms of data.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("WqData", d)}, where \code{d} is a data.frame. \code{d} should have columns named \code{time, site, depth, variable, value} of class \code{"DateTime", "factor", "numeric", "factor", "numeric"}, respectively.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"data.frame"} as described above} 
    
    Automatically created slots:
    
    \item{\code{names}:}{Object of class \code{"character"} containing the names of the original data.frame.}
    \item{\code{row.names}:}{Object of class \code{"data.frameRowLabels"} containing the row names of the original data.frame.}
    \item{\code{.S3Class}:}{Object of class \code{"character"} describing which S3 class is extended by this class.}
  }
}
\section{Extends}{
  Class \code{"\linkS4class{data.frame}"}, directly.
  Class \code{"\linkS4class{list}"}, by class "data.frame", distance 2.
  Class \code{"\linkS4class{oldClass}"}, by class "data.frame", distance 2.
  Class \code{"\linkS4class{vector}"}, by class "data.frame", distance 3.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "WqData")}: Multi-page stripchart of observations, each page containing one plot of all sites for one variable. An argument \code{vars} can be given specifying a character string of variables to be plotted. Otherwise, only the first 10 variables will be plotted if the number of variables is more than 10. Further arguments will be passed to the underlying \code{stripplot} function.}
  }
    
  \describe{
    \item{summary}{\code{signature(x = "WqData")}: Date range, number of observations for each combination of site and variable, and quartiles of variable values.}
  }

  \describe{
    \item{tsMake}{\code{signature(x = "WqData")}: see \code{\link{tsMake,WqData-method}}}
  }
}

\seealso{
 \code{\link{DateTime-class}}, \code{\link{tsMake,WqData-method}}, \code{\link{wqData}}
}
\examples{
showClass("WqData")

### Make a WqData object from sfbay data ###

# Combine date and time into a POSIXct object called time.
# In this case, requires first making sure all sfbay times have 4
# digits. Also drop date, which is no longer needed.
sfb1 <- transform(sfbay, time = substring(10000 + time, 2, 5))
sfb1 <- transform(sfb1, time = paste(date, time, sep = " "))
sfb1 <- transform(sfb1, time = as.POSIXct(time, format = "\%m/\%d/\%Y \%H\%M"))
sfb1$date <- NULL

# If time of day were not needed, then the following would suffice:
# sfb1 <- transform(sfbay, time = as.Date(date, "\%m/\%d/\%Y"))
# sfb1$date <- NULL

# 'Melt' the data into the correct format
# which automatically gives required variable and value fields.
require(reshape)
m1 <- melt(sfb1, id.vars = c('time', 'stn', 'depth'))

# Make sure names of id.vars conform to WqData requirements
names(m1)[2] <- "site"

# Make sure all column classes conform to WqData requirements
m1 <- transform(m1, 
        site = factor(paste("s", site, sep = ""), ordered = TRUE)
      )

# Remove NAs
m1 <- subset(m1, !is.na(value))
rownames(m1) <- 1:nrow(m1)

# Create the new WqData object
sfb <- new('WqData', m1)
###

# Summarize the data
summary(sfb)

# Create multi-page pdf of strip chart summary of data
pdf()  # default is to create Rplots.pdf
plot(sfb, vars = c('chl', 'dox', 'spm'))
dev.off()
}

\keyword{classes}