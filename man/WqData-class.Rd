\name{WqData-class}
\Rdversion{1.1}
\docType{class}
\alias{WqData-class}
\alias{[,WqData-method}
\alias{summary,WqData-method}
\alias{plot,WqData-method}

\title{Class "WqData"}
\description{A simple extension or subclass of the \code{"data.frame"} class for typical \dQuote{discrete} water quality monitoring programs that examine phenomena on a time scale of days or longer. It requires water quality data to be in a specific \dQuote{long} format, although a generating function \code{\link{wqData}} can be used for different forms of data.
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
    \item{[}{\code{signature(x = "WqData")}:
    
    x[i, ]
    
    Extracts a subset of the data, based on either row numbers or a logical expression involving the columns (\code{time, site, depth, variable, value}). The returned value is also a \code{WqData} object.
    }
  }
    
  \describe{
    \item{plot}{\code{signature(x = "WqData")}:
    
    plot(x, vars, num.col = NULL)
    
    Multiple plots, each containing boxplots of data at each site for the same variable. An argument \code{vars} can be given specifying a character string of variables to be plotted. Otherwise, only the first 10 variables will be plotted if the number of variables is more than 10. The number of columns plotted is determined automatically unless \code{num.col} is given an integer value.
    }
  }
     
  \describe{
    \item{summary}{\code{signature(x = "WqData")}: 
    
    summary(x)
    
    Date range, number of observations for each combination of site and variable, and quartiles of variable values.
    }
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
# Construct the WqData object sfb as shown in the wqData examples.
sfb <- wqData(sfbay, c(1,3,4), 5:12, site.order = TRUE, type = "wide", 
              time.format = "\%m/\%d/\%Y")
# Summarize the data
summary(sfb)
# Create boxplot summary of data
plot(sfb, vars = c('chl', 'dox', 'spm'), num.col = 2)
# Extract some of the data as a WqData object
sfb[1:10,]  # first 10 observations
sfb[sfb$depth==20,]  # all observations at 20 m
}

\keyword{classes}
