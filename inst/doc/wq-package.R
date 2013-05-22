### R code from vignette source 'wq-package.Rnw'

###################################################
### code chunk number 1: wq-package.Rnw:51-52
###################################################
library(wq)


###################################################
### code chunk number 2: wq-package.Rnw:59-65 (eval = FALSE)
###################################################
## sfbay <- read.csv("sfbay.csv", header = FALSE, as.is = TRUE,
##                   skip = 2)
## names(sfbay) <- c('date', 'time', 'stn', 'depth', 'chl', 'dox',
##                   'spm', 'ext', 'sal', 'temp', 'nox', 'nhx')
## sfbay <- subset(sfbay, stn %in% c(21, 24, 27, 30, 32, 36) &
##                 substring(date, 7, 10) %in% 1985:2004)


###################################################
### code chunk number 3: wq-package.Rnw:70-71
###################################################
head(sfbay)


###################################################
### code chunk number 4: wq-package.Rnw:76-81
###################################################
x <- sample(1:nrow(sfbay), 10)
sfbay[x, "dox"]
sfbay1 <- transform(sfbay,
                    dox = round(100 * dox/oxySol(sal, temp), 1))
sfbay1[x, "dox"]


###################################################
### code chunk number 5: wq-package.Rnw:100-103
###################################################
sfb <- wqData(sfbay, c(1, 3:4), 5:12, site.order = TRUE,
              type = "wide", time.format = "%m/%d/%Y")
head(sfb)


###################################################
### code chunk number 6: wq-package.Rnw:108-109
###################################################
summary(sfb)


###################################################
### code chunk number 7: wq-package.Rnw:114-115 (eval = FALSE)
###################################################
## plot(sfb, vars = c('dox', 'temp'), num.col = 2)


###################################################
### code chunk number 8: wq-package.Rnw:121-122
###################################################
print(plot(sfb, vars = c('dox', 'temp'), num.col = 2))


###################################################
### code chunk number 9: wq-package.Rnw:135-138
###################################################
y <- tsMake(sfb, focus = "chl", layer = c(0, 5))
y[1:6, ]
tsp(y)


###################################################
### code chunk number 10: wq-package.Rnw:140-141 (eval = FALSE)
###################################################
## plotTs(y, ylab = "Chlorophyll in San Francisco Bay", ncol = 2)


###################################################
### code chunk number 11: wq-package.Rnw:147-149
###################################################
print(plotTs(y, ylab = "Chlorophyll in San Francisco Bay",
             ncol = 2))


###################################################
### code chunk number 12: wq-package.Rnw:158-159
###################################################
head(tsMake(sfb, focus = "chl", layer = c(0, 5), type = 'zoo'))


###################################################
### code chunk number 13: wq-package.Rnw:164-168
###################################################
chl27 <- sfbayChla[, 's27']
tsp(chl27)
chl27 <- round(chl27, 1)
head(ts2df(chl27))


###################################################
### code chunk number 14: wq-package.Rnw:173-176
###################################################
y <- window(sfbayChla, start = 2005,
            end = c(2009, 12))  # 5 years, 16 sites
round(mts2ts(y, seas = 2:4), 1)  # focus on Feb-Apr spring bloom


###################################################
### code chunk number 15: wq-package.Rnw:185-186
###################################################
mannKen(Nile)


###################################################
### code chunk number 16: wq-package.Rnw:191-195
###################################################
y <- sfbayChla
y1 <- tsSub(y, seas = 2:4)  # focus on Feb-Apr spring bloom
y2 <- aggregate(y1, 1, mean, na.rm = FALSE)
signif(mannKen(y2), 3)


###################################################
### code chunk number 17: wq-package.Rnw:200-202
###################################################
chl27 <- sfbayChla[, "s27"]
seaKen(chl27)


###################################################
### code chunk number 18: wq-package.Rnw:207-208
###################################################
seaRoll(chl27, w = 10)


###################################################
### code chunk number 19: wq-package.Rnw:213-214
###################################################
x <- sfbayChla


###################################################
### code chunk number 20: wq-package.Rnw:216-217 (eval = FALSE)
###################################################
## seasonTrend(x, plot = TRUE, ncol = 2, scales = 'free_y')


###################################################
### code chunk number 21: wq-package.Rnw:223-224
###################################################
print(seasonTrend(x, plot = TRUE, ncol = 2, scales = 'free_y'))


###################################################
### code chunk number 22: wq-package.Rnw:233-235
###################################################
x <- sfbayChla[, 's27']
trendHomog(x)


###################################################
### code chunk number 23: wq-package.Rnw:240-242
###################################################
chl <-sfbayChla[, 1:12]  # first 12 stns have good data coverage
seaKen(mts2ts(chl, 2:4))  # regional trend in spring bloom


###################################################
### code chunk number 24: wq-package.Rnw:251-253
###################################################
chl27 <- sfbayChla[, "s27"]
chl27a <- interpTs(chl27, gap = 3)


###################################################
### code chunk number 25: wq-package.Rnw:255-257 (eval = FALSE)
###################################################
## plot(chl27a, col = "red", lwd = .5, xlab = "")
## lines(chl27, col = "blue", lwd = 1.5)


###################################################
### code chunk number 26: wq-package.Rnw:263-265
###################################################
plot(chl27a, col = "red", lwd = .5, xlab = "")
lines(chl27, col = "blue", lwd = 1.5)


###################################################
### code chunk number 27: wq-package.Rnw:274-276
###################################################
chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
chla1 <- chla1[, 1:12]


###################################################
### code chunk number 28: wq-package.Rnw:278-279 (eval = FALSE)
###################################################
## eofNum(chla1, distr = "lognormal", reps = 2000)


###################################################
### code chunk number 29: wq-package.Rnw:285-286
###################################################
print(eofNum(chla1, distr = "lognormal", reps = 2000))


###################################################
### code chunk number 30: wq-package.Rnw:295-297
###################################################
e1 <- eof(chla1, n = 1)
e1


###################################################
### code chunk number 31: wq-package.Rnw:302-303 (eval = FALSE)
###################################################
## eofPlot(e1, type = "amp")


###################################################
### code chunk number 32: wq-package.Rnw:309-310
###################################################
print(eofPlot(e1, type = "amp"))


###################################################
### code chunk number 33: wq-package.Rnw:319-322
###################################################
chl27b <- interpTs(sfbayChla[, "s27"], gap = 3)
chl27b <- ts2df(chl27b, mon1 = 10, addYr = TRUE, omit = TRUE)
head(round(chl27b, 1))


###################################################
### code chunk number 34: wq-package.Rnw:327-328
###################################################
e2 <- eof(chl27b, n = 2)


###################################################
### code chunk number 35: wq-package.Rnw:330-331 (eval = FALSE)
###################################################
## eofPlot(e2, type = "coef")


###################################################
### code chunk number 36: wq-package.Rnw:337-338
###################################################
print(eofPlot(e2, type = "coef"))


###################################################
### code chunk number 37: wq-package.Rnw:353-355
###################################################
chl27 <- sfbayChla[, "s27"]
d1 <- decompTs(chl27)


###################################################
### code chunk number 38: wq-package.Rnw:357-358 (eval = FALSE)
###################################################
## plot(d1, nc = 1, main = "Station 27 Chl-a decomposition")


###################################################
### code chunk number 39: wq-package.Rnw:364-365
###################################################
plot(d1, nc = 1, main = "Station 27 Chl-a decomposition")


###################################################
### code chunk number 40: wq-package.Rnw:374-376 (eval = FALSE)
###################################################
## plotSeason(chl27, num.era = 3, same.plot = FALSE,
##            ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 41: wq-package.Rnw:382-384
###################################################
print(plotSeason(chl27, num.era = 3, same.plot = FALSE,
                 ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 42: wq-package.Rnw:393-395 (eval = FALSE)
###################################################
## plotSeason(chl27, num.era = 3, same.plot = TRUE,
##            ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 43: wq-package.Rnw:401-403
###################################################
print(plotSeason(chl27, num.era = 3, same.plot = TRUE,
                 ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 44: wq-package.Rnw:412-413 (eval = FALSE)
###################################################
## plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 45: wq-package.Rnw:419-420
###################################################
print(plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 46: wq-package.Rnw:435-442
###################################################
chl27 <- sfbayChla[, 's27']
p1 <- phenoPhase(chl27)
head(p1)
p2 <- phenoPhase(chl27, c(1, 6))
head(p2)
p3 <- phenoAmp(chl27, c(1, 6))
head(p3)


###################################################
### code chunk number 47: wq-package.Rnw:447-453
###################################################
zchl <- tsMake(sfb, focus = "chl", layer = c(0, 5), type = 'zoo')
head(zchl)
zchl27 <- zchl[, 3]
head(phenoPhase(zchl27))
head(phenoPhase(zchl27, c(1, 6), out = 'doy'))
head(phenoPhase(zchl27, c(1, 6), out = 'julian'))


###################################################
### code chunk number 48: wq-package.Rnw:460-461
###################################################
chl <- aggregate(sfbayChla[, 1:6], 1, meanSub, 2:4, na.rm = TRUE)


###################################################
### code chunk number 49: wq-package.Rnw:463-465 (eval = FALSE)
###################################################
## plotTsAnom(chl, ylab = 'Chlorophyll-a', strip.labels =
##             paste('Station', substring(colnames(chl), 2, 3)))


###################################################
### code chunk number 50: wq-package.Rnw:471-474
###################################################
print(plotTsAnom(chl, ylab = 'Chlorophyll-a', strip.labels =
      paste('Station', substring(colnames(chl), 2, 3)))
)


###################################################
### code chunk number 51: wq-package.Rnw:484-485
###################################################
chl27 <- sfbayChla[, "s27"]


###################################################
### code chunk number 52: wq-package.Rnw:487-488 (eval = FALSE)
###################################################
## plotTsTile(chl27)


###################################################
### code chunk number 53: wq-package.Rnw:494-495
###################################################
print(plotTsTile(chl27))


###################################################
### code chunk number 54: wq-package.Rnw:506-515
###################################################
chl27 = sfbayChla[, 's27']
g1 <- plotTsTile(chl27, legend.title = 'Chl log-anomaly',
    square=FALSE)
g2 <- seasonTrend(chl27, plot = TRUE, legend = TRUE)
g3 <- plotSeason(chl27, num.era = 3,
                 ylab = expression(paste('Chl-', italic(a), ', ',
                                         mu*g~L^{-1})))
## quartz("", 10, 6)  # e.g., in mac os x, or:
## grid.newpage()  # to re-use existing plot window


###################################################
### code chunk number 55: wq-package.Rnw:517-518 (eval = FALSE)
###################################################
## layOut(list(g1, 1:2, 1:6), list(g2, 1:2, 7:10), list(g3, 3:5, 1:8))


###################################################
### code chunk number 56: wq-package.Rnw:524-526
###################################################
print(layOut(list(g1, 1:2, 1:6), list(g2, 1:2, 7:10),
             list(g3, 3:5, 1:8)))


