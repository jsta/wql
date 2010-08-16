\name{decompTs}
\Rdversion{1.1}
\alias{decompTs}

\title{
Decompose a time series
}

\description{
The function decomposes a time series into a long-term mean, annual, seasonal and "events" component. The decomposition can be multiplicative or additive.
}

\usage{
decompTs(x, startyr, endyr, event = TRUE, type = c("mult", "add"))
}
\arguments{
  \item{x }{
a monthly time series vector
}
  \item{startyr }{
the desired starting year for the analysis
}
  \item{endyr }{
the ending year
}
  \item{event }{
whether or not an "events" component should be determined
}
  \item{type }{
the kind of decomposition, either multiplicative ("mult") or additive ("add")
}
}

\details{
A multiplicative decomposition will typically be useful for a biological community- or population-related variable (e.g., chlorophyll-a) that experiences exponential changes in time and is approximately lognormal, whereas an additive deomposition is more suitable for a normal variable.  Aside from the long-term mean, each component of a multiplicative decomposition will average 1, whereas each component of an additive decomposition will average 0.  The rationale for this simple approach to decomposing a time series, with examples of its application, is given by Cloern and Jassby (2009).  

If \code{event = TRUE}, the seasonal component represents a recurring monthly pattern and the events component a residual series.  Otherwise, the seasonal component becomes the residual series.  

Seasonal patterns that change systematically over time can be investigated by applying the function to a \dQuote{rolling} time window by changing \code{startyr} and \code{endyr}.  
}

\value{
A monthly time series matrix with the following individual time series:
\item{original }{original time series}
\item{grandmean }{constant series equal to the long-term mean}
\item{annual }{annual mean series}
\item{seasonal }{repeating seasonal component}
\item{events }{optionally, the residual or "events" series}
}

\references{
Cloern, J.E. and Jassby, A.D. (2009) Patterns and scales of phytoplankton variability in estuarine-coastal ecosystems. \emph{Estuaries and Coasts} DOI 10.1007/s12237-009-9195-3.
}

\examples{
# Apply the function to a matrix time series, producing a list of decompositions
ans <- vector('list', ncol(sfbayChla))
names(ans) <- colnames(sfbayChla)
for(i in seq(along = names(ans))) {
  ans[[i]] <- decompTs(sfbayChla[, i])
}

# A quick plot for a time series decomposiiton
plot(ans[[7]], nc = 1, main = paste(names(ans)[7], "Chl-a decomposition"))
}

\keyword{manip}
\keyword{ts}