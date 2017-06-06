---
output: github_document
---
# wql
<!-- rmarkdown v1 -->
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![Licence](https://img.shields.io/badge/licence-GPL--2-blue.svg)](https://www.gnu.org/licenses/old-licenses/gpl-2.0.html)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-3.0.0-6666ff.svg)](https://cran.r-project.org/)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/wql)](https://cran.r-project.org/package=wql)
[![packageversion](https://img.shields.io/badge/Package%20version-0.4.9-orange.svg?style=flat-square)](commits/master)
[![Last-changedate](https://img.shields.io/badge/last%20change-2017--06--06-yellowgreen.svg)](/commits/master)

<!-- README.md is generated from README.Rmd. Please edit that file -->



## Installation

You can install wql from github with:


```r
# install.packages("devtools")
devtools::install_github("jsta/wql")
```

## Motivation

The main purpose of wql is to explore seasonal time series through plots and nonparametric trend tests.  It was created originally to examine water quality data sets (hence, wql) but is suitable as a more general purpose set of tools for looking at annual or seasonal time series.

One of the more tedious tasks in exploring environmental data sets is creating usable time series from the original complex data sets, especially when you want many series at will that group data in different ways.  So wql also provides a way of transforming data sets to a common format that then allows a diversity of time series to be created quickly.  A few functions are specific to the fields of limnology and oceanography.

The plots are designed for easy use, not for publication-quality graphs.  Nonetheless, extensive customization is possible by passing options through ..., adding annotations in the case of base graphics, and adding layers in the case of ggplot2 objects.

Two functions are used mainly for preparing the times series:

* a function that transforms incoming data to a common data structure in the form of the WqData class
* a function that easily prepares time series objects from this class

The WqData class can be easily adapted to non-aquatic data.  Obviously, the depth field can be used for elevation in atmospheric studies.  But more generally, the site and depth fields can be used for many two-way classifications and don't need to refer to spatial location.

Some of the time series functions include:

* a variety of plots to examine changes in seasonal patterns
* nonparametric trend tests
* time series interpolation and related manipulations
* a simple decomposition of a series into different time scales
* phenological analyses
* the use of empirical orthogonal functions to detect multiple independent
mechanisms underlying temporal change

A few functions are specialized for the aquatic sciences:

* converting between oxygen concentrations and percent saturation
* converting between salinity and conductivity

## Usage

The capabilities of wql are more fully explained in the accompanying vignette:
wql: Exploring environmental monitoring data.

## References

This package is a fork of the archived CRAN package `wq`. To date, improvements have been focused on improving the documentation and code readability.


```
#> 
#> To cite wq in publications use:
#> 
#>   Alan D. Jassby and James E. Cloern (). wq: Some tools for
#>   exploring water quality monitoring data. R package version
#>   0.4.9. http://cran.r-project.org/package=wq
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{jassbywq,
#>     title = {wq: Exploring water quality monitoring data},
#>     author = {Alan D. Jassby and James E. Cloern},
#>     note = {R package version 0.4.9},
#>     url = {http://CRAN.R-project.org/package=wq},
#>   }
```
