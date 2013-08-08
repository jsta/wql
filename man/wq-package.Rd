\name{wq-package}
\alias{wq-package}
\alias{wq}
\docType{package}
\title{
Exploring water quality monitoring data
}
\description{
Functions to assist in the processing and exploration of data from
    environmental monitoring programs. The name "wq" stands for "water quality" 
    and reflects the original focus on time series data for physical and 
    chemical properties of water, as well as the plankton. The package is 
    intended for programs that sample approximately monthly at discrete 
    stations, a feature of many legacy data sets. Most of the functions should 
    be useful for analysis of similar-frequency time series regardless of the 
    subject matter.
}
\details{
\tabular{ll}{
Package: \tab wq\cr
Type: \tab Package\cr
Version: \tab 0.3-11\cr
Date: \tab 2013-08-01\cr
Depends: \tab 
methods,
R (>= 3.0.0),
zoo\cr
Suggests: \tab 
ggplot2 (>= 0.9),
grid,
reshape2\cr
License: \tab GPL-2\cr
LazyData: \tab yes\cr
Built: \tab R 3.0.1; ; 2013-08-06 20:40:04 UTC; unix\cr
}

Index:
\preformatted{
DateTime-class          Class "DateTime"
WqData-class            Class "WqData"
ammFrac                 Un-ionized ammonia
date2decyear            Miscellaneous utility functions
decompTs                Decompose a time series
ec2pss                  Convert conductivity to salinity
eof                     Empirical orthogonal functions
eofNum                  Assess significance of eigenvalues
eofPlot                 Plot results of an EOF analysis
interpTs                Interpolate or substitute missing time series values
layOut                  Arrange a group of saved plots
mannKen                 Mann-Kendall test and the Sen slope
mts2ts                  Converts matrix to vector time series
oxySol                  Dissolved oxygen at saturation
phenoAmp                Phenological amplitude
phenoAmp-methods        Methods for Function phenoAmp
phenoPhase              Phenological phase
phenoPhase-methods      Methods for Function phenoPhase
plotSeason              Plots seasonal patterns for a time series
plotTs                  Time series plot
plotTsAnom              Anomaly plot of time series
plotTsTile              Image plot of monthly time series
seaKen                  Seasonal and Regional Kendall test
seasonTrend             Determine seasonal trends
sfbay                   San Francisco Bay water quality data
trendHomog              Trend homogeneity test
ts2df                   Convert time series to data frame
tsMake                  Create time series from water quality data
tsMake-methods          Methods for Function tsMake
wq-package              Exploring water quality monitoring data
wqData                  Construct an object of class "WqData"
zoo-class               Class "zoo"
}

Further information is available in the following vignettes:
\tabular{ll}{
\code{wq-package} \tab wq: exploring water quality monitoring data (source, pdf)\cr
}
}
\author{
Alan D. Jassby and James E. Cloern

Maintainer: Alan Jassby <adjassby@ucdavis.edu>
}
\keyword{ package }
