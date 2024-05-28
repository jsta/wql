wql 1.0.1
=========
* CRAN maintenence release

wql 1.0.0
=========
* Update maintainer name and contact info
* Bump version to signify stable functioning
* Link rot fixes

wql 0.4.9
=========
* Fork and rebrand wq to wql
* Added roxygen documentation

wq 0.4.8
========
* eof: Shift id to row name in output.
* eofPlot: Fix for non-numeric row names in original data.
* mannKen: Use different definition of relative sen slope.
* pett: Add change.size. Get actual change.time for zoo objects.
* phenoAmp: Change to more versatile output statistics. Change argument name.
* phenoPhase: Change argument name.
* seaKen: Use different definition of relative sen slope.
* trendHomog: Ignore seasons with insufficient data.

wq 0.4.7
========
* Add back html vignette.
* Fix global variable notes.
* eof: Calculation method changed.
* eofNum: Remove ruleN-based colouring.
* eofPlot: Remove Monte Carlo lines.
* plotTsTile: Default to median instead of mean.
* ruleN: Remove.

wq 0.4.6
========
* decompTs: Remove startyr and endyr. Add median as default centering method.
* mannKen: Remove "tau" argument. Use median instead of mean for relative. Add exact p-values. Allow matrix and data.frame. Change plot.
* pett.R: Add new function with plot for Pettitt test.
* plotTs: Remove plot.order argument. Improve legend.
* plotTsAnom: Remove plot.order argument.
* seaKen: Allow matrix and data.frame. Add plot.
* seaRoll: Change arguments and plot.
* seasonTrend: Change arguments. Remove ls trend method. Improve legend.

wq 0.4.5
========
* eofPlot.R: Fix argument to geom_hline.

wq 0.4.4
========
* wq-internal.R: Removed.
* plot WqData-class: Added global variable definitions.

wq 0.4.3
========
* ammFrac: Remove.
* layOut: Remove.

wq 0.4-1
========
* mannKen: Preserve station order in data matrix.
* mannKen: Modify examples to be more useful.
* NAMESPACE: Add importFrom("methods").
* NAMESPACE: Remove all require(zoo) from functions.
* plotSeason: Enable plotting by arbitrary eras.
* plotSeason: Fixed error when num.era=1.
* vignette: Remove unnecessary commands from preamble.
* wqData: Require locus to be length 3.
* WqData-class: Change tz="UTC" default in as.Date for summary.

wq 0.3-11
=========
* `[`: method now exists for WqData objects.
* plotTsTile: has additional options and a corrected help file.
* subset, transform: no longer used within code.
* wqData: handles collision between locus and wq variables with same name.
* minor bug fixes.

wq 0.3-10
=========
* Vignette (re-)included.
* Minor changes in plotting functions.

wq 0.3-8
========
* Updated for ggplot2 0.9.2.

wq 0.3-7
========
* Minor changes to manual.

wq 0.3-6
========
* Minor changes to code, with no changes in behaviour.

wq 0.3-5
========
* Addresses issues introduced with version 0.9 of ggplot2.
* Uses reshape2 instead of reshape.
* plotTsAnom and seasonTrend: minor plotting changes.

wq 0.3-4
========
* date2decyear: Converts dates to numeric years.
* interpTs: More options for replacing NAs.
* plotTs: Time series line plot plus isolated points
* plotTsAnom: Now works for matrix time series.
* theme_wq: Simplified version of theme_grey in package "ggplot2".
* tsMake: No longer excruciatingly slow when layer = "max.depths"

wq 0.3-3
========
* plotTsAnom: time series anomaly plot.
* tsMake: Now aggregates depths (and days) by quantiles as well as means.
* plotSeason: Fixed problem with missing data treatment.
* tsSub: Fixed problem with incorrect starting month.

wq 0.3-1
========
* plot method for class "WqData": Boxplots instead of stripcharts. Also fixed behaviour when less than 10 unique variables.
* plotSeason: Changed "by.month" option to standardized anomalies.
* tsMake: Added option to create series for "bottom" of water column.
* Version requirement is R 10.0.
* Vignette file size fixed so it can be downloaded from HTML help.

wq 0.3
======
* All plot functions now produce objects of class "ggplot".
* layOut: Lays out graphs of class ggplot.
* mannKen: Added mts and plot options.
* meanSub: Utility function to average subsets.
* mts2ts: Prepare spatial time series for further analysis.
* plotSeason: Added another way of plotting.
* seaRoll: Plotting is an additional option.
* trendHomog: Test for homogeneity of seasonal trends.
* tsSub: Utility function to drop seasons in a ts.
* plotEof: Changed to eofPlot.

wq 0.2-9
========
* phenoPhase: Integration defaults can now be changed, if necessary.
* plotSeason: Added smoother when type='by.month'.
* seasonTrend: Changed appearance and some options.
* plotSeason: Fixed problem when type='by.month'.
* plotTsTile: Now plots correct colours when four=FALSE.
* tsMake: Fixed problem when layer of class "numeric".

wq 0.2-8
========
* wqData: Changed "narrow" to "long".
* plotSeas: Changed to plotSeason.
* ammFrac: New function for un-ionized ammonium.
* plotSeason: Added option to plot individual months for entire record.
* seasonTrend: New function for time series trends by season.
* tsMake: Now allows for more ways of specifying depths.
* mannKen, seaKen: Now gives correct sign when trend given as %.
* phenoAmp: Fixed problem when subsetting months leaves no data for year.
* phenoPhase: Fixed problem when subsetting months and x is a "zoo" object.
* plotSeason: Now handles num = 1.
* tsMake: Fixed problem when focus consisted of a single variable or site.

wq 0.2-5
========
* interpTs: Option to fill with long-term mean or median.
* plotSeas: Option to choose number of intervals.
* WqData plot method: Option to choose variables.
* phenoPhase: Fixed problem where max.time month number given relative to
  subset rather than whole year
* plotTsTile: More useful error message when not enough data.
* seaRoll: Fixed problem with frequencies other than 12.
* tsMake: Remove need to specify layer if no depth info.
* WqData subset method: Removed because of strategy that WqData objects	should not be changed.
* wqData: Argument value "narrow" changed to more conventional "long".
* wqData: Now requires time.format to be specified.
* wqData: Now ensures variable is a factor when type = "long".
