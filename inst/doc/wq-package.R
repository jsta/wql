### R code from vignette source 'wq-package.Rnw'

###################################################
### code chunk number 1: wq-package.Rnw:50-51
###################################################
library(wq)


###################################################
### code chunk number 2: wq-package.Rnw:58-64 (eval = FALSE)
###################################################
## sfbay <- read.csv("sfbay.csv", header = FALSE, as.is = TRUE,
##                   skip = 2)
## names(sfbay) <- c('date', 'time', 'stn', 'depth', 'chl', 'dox',
##                   'spm', 'ext', 'sal', 'temp', 'nox', 'nhx')
## sfbay <- subset(sfbay, stn %in% c(21, 24, 27, 30, 32, 36) &
##                 substring(date, 7, 10) %in% 1985:2004)


###################################################
### code chunk number 3: wq-package.Rnw:69-70
###################################################
head(sfbay)


###################################################
### code chunk number 4: wq-package.Rnw:75-80
###################################################
x <- sample(1:nrow(sfbay), 10)
sfbay[x, "dox"]
sfbay1 <- transform(sfbay,
                    dox = round(100 * dox/oxySol(sal, temp), 1))
sfbay1[x, "dox"]


###################################################
### code chunk number 5: wq-package.Rnw:97-100
###################################################
sfb <- wqData(sfbay, c(1, 3:4), 5:12, site.order = TRUE,
              type = "wide", time.format = "%m/%d/%Y")
head(sfb)


###################################################
### code chunk number 6: wq-package.Rnw:105-106
###################################################
summary(sfb)


###################################################
### code chunk number 7: wq-package.Rnw:111-112 (eval = FALSE)
###################################################
## plot(sfb, vars = c('dox', 'temp'), num.col = 2)


###################################################
### code chunk number 8: wq-package.Rnw:118-119
###################################################
print(plot(sfb, vars = c('dox', 'temp'), num.col = 2))


###################################################
### code chunk number 9: wq-package.Rnw:132-135
###################################################
y <- tsMake(sfb, focus = "chl", layer = c(0, 5))
y[1:6, ]
tsp(y)


###################################################
### code chunk number 10: wq-package.Rnw:137-138 (eval = FALSE)
###################################################
## plotTs(y, ylab = "Chlorophyll in San Francisco Bay", ncol = 2)


###################################################
### code chunk number 11: wq-package.Rnw:144-146
###################################################
print(plotTs(y, ylab = "Chlorophyll in San Francisco Bay",
             ncol = 2))


###################################################
### code chunk number 12: wq-package.Rnw:155-156
###################################################
head(tsMake(sfb, focus = "chl", layer = c(0, 5), type = 'zoo'))


###################################################
### code chunk number 13: wq-package.Rnw:161-165
###################################################
chl27 <- sfbayChla[, 's27']
tsp(chl27)
chl27 <- round(chl27, 1)
head(ts2df(chl27))


###################################################
### code chunk number 14: wq-package.Rnw:170-173
###################################################
y <- window(sfbayChla, start = 2005,
            end = c(2009, 12))  # 5 years, 16 sites
round(mts2ts(y, seas = 2:4), 1)  # focus on Feb-Apr spring bloom


###################################################
### code chunk number 15: wq-package.Rnw:182-183
###################################################
mannKen(Nile)


###################################################
### code chunk number 16: wq-package.Rnw:188-192
###################################################
y <- sfbayChla
y1 <- tsSub(y, seas = 2:4)  # focus on Feb-Apr spring bloom
y2 <- aggregate(y1, 1, mean, na.rm = FALSE)
signif(mannKen(y2), 3)


###################################################
### code chunk number 17: wq-package.Rnw:197-199
###################################################
chl27 <- sfbayChla[, "s27"]
seaKen(chl27)


###################################################
### code chunk number 18: wq-package.Rnw:204-205
###################################################
seaRoll(chl27, w = 10)


###################################################
### code chunk number 19: wq-package.Rnw:210-211
###################################################
x <- sfbayChla


###################################################
### code chunk number 20: wq-package.Rnw:213-214 (eval = FALSE)
###################################################
## seasonTrend(x, plot = TRUE, ncol = 2, scales = 'free_y')


###################################################
### code chunk number 21: wq-package.Rnw:220-221
###################################################
print(seasonTrend(x, plot = TRUE, ncol = 2, scales = 'free_y'))


###################################################
### code chunk number 22: wq-package.Rnw:230-232
###################################################
x <- sfbayChla[, 's27']
trendHomog(x)


###################################################
### code chunk number 23: wq-package.Rnw:237-239
###################################################
chl <-sfbayChla[, 1:12]  # first 12 stns have good data coverage
seaKen(mts2ts(chl, 2:4))  # regional trend in spring bloom


###################################################
### code chunk number 24: wq-package.Rnw:248-250
###################################################
chl27 <- sfbayChla[, "s27"]
chl27a <- interpTs(chl27, gap = 3)


###################################################
### code chunk number 25: wq-package.Rnw:252-254 (eval = FALSE)
###################################################
## plot(chl27a, col = "red", lwd = .5, xlab = "")
## lines(chl27, col = "blue", lwd = 1.5)


###################################################
### code chunk number 26: wq-package.Rnw:260-262
###################################################
plot(chl27a, col = "red", lwd = .5, xlab = "")
lines(chl27, col = "blue", lwd = 1.5)


###################################################
### code chunk number 27: wq-package.Rnw:271-273
###################################################
chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
chla1 <- chla1[, 1:12]


###################################################
### code chunk number 28: wq-package.Rnw:275-276 (eval = FALSE)
###################################################
## eofNum(chla1, distr = "lognormal", reps = 2000)


###################################################
### code chunk number 29: wq-package.Rnw:282-283
###################################################
print(eofNum(chla1, distr = "lognormal", reps = 2000))


###################################################
### code chunk number 30: wq-package.Rnw:292-294
###################################################
e1 <- eof(chla1, n = 1)
e1


###################################################
### code chunk number 31: wq-package.Rnw:299-300 (eval = FALSE)
###################################################
## eofPlot(e1, type = "amp")


###################################################
### code chunk number 32: wq-package.Rnw:306-307
###################################################
print(eofPlot(e1, type = "amp"))


###################################################
### code chunk number 33: wq-package.Rnw:316-319
###################################################
chl27b <- interpTs(sfbayChla[, "s27"], gap = 3)
chl27b <- ts2df(chl27b, mon1 = 10, addYr = TRUE, omit = TRUE)
head(round(chl27b, 1))


###################################################
### code chunk number 34: wq-package.Rnw:324-325
###################################################
e2 <- eof(chl27b, n = 2)


###################################################
### code chunk number 35: wq-package.Rnw:327-328 (eval = FALSE)
###################################################
## eofPlot(e2, type = "coef")


###################################################
### code chunk number 36: wq-package.Rnw:334-335
###################################################
print(eofPlot(e2, type = "coef"))


###################################################
### code chunk number 37: wq-package.Rnw:350-352
###################################################
chl27 <- sfbayChla[, "s27"]
d1 <- decompTs(chl27)


###################################################
### code chunk number 38: wq-package.Rnw:354-355 (eval = FALSE)
###################################################
## plot(d1, nc = 1, main = "Station 27 Chl-a decomposition")


###################################################
### code chunk number 39: wq-package.Rnw:361-362
###################################################
plot(d1, nc = 1, main = "Station 27 Chl-a decomposition")


###################################################
### code chunk number 40: wq-package.Rnw:371-373 (eval = FALSE)
###################################################
## plotSeason(chl27, num.era = 3, same.plot = FALSE,
##            ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 41: wq-package.Rnw:379-381
###################################################
print(plotSeason(chl27, num.era = 3, same.plot = FALSE,
                 ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 42: wq-package.Rnw:390-392 (eval = FALSE)
###################################################
## plotSeason(chl27, num.era = 3, same.plot = TRUE,
##            ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 43: wq-package.Rnw:398-400
###################################################
print(plotSeason(chl27, num.era = 3, same.plot = TRUE,
                 ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 44: wq-package.Rnw:409-410 (eval = FALSE)
###################################################
## plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 45: wq-package.Rnw:416-417
###################################################
print(plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 46: wq-package.Rnw:432-439
###################################################
chl27 <- sfbayChla[, 's27']
p1 <- phenoPhase(chl27)
head(p1)
p2 <- phenoPhase(chl27, c(1, 6))
head(p2)
p3 <- phenoAmp(chl27, c(1, 6))
head(p3)


###################################################
### code chunk number 47: wq-package.Rnw:444-450
###################################################
zchl <- tsMake(sfb, focus = "chl", layer = c(0, 5), type = 'zoo')
head(zchl)
zchl27 <- zchl[, 3]
head(phenoPhase(zchl27))
head(phenoPhase(zchl27, c(1, 6), out = 'doy'))
head(phenoPhase(zchl27, c(1, 6), out = 'julian'))


###################################################
### code chunk number 48: wq-package.Rnw:457-458
###################################################
chl <- aggregate(sfbayChla[, 1:6], 1, meanSub, 2:4, na.rm = TRUE)


###################################################
### code chunk number 49: wq-package.Rnw:460-462 (eval = FALSE)
###################################################
## plotTsAnom(chl, ylab = 'Chlorophyll-a', strip.labels =
##             paste('Station', substring(colnames(chl), 2, 3)))


###################################################
### code chunk number 50: wq-package.Rnw:468-471
###################################################
print(plotTsAnom(chl, ylab = 'Chlorophyll-a', strip.labels =
      paste('Station', substring(colnames(chl), 2, 3)))
)


###################################################
### code chunk number 51: wq-package.Rnw:481-482
###################################################
chl27 <- sfbayChla[, "s27"]


###################################################
### code chunk number 52: wq-package.Rnw:484-485 (eval = FALSE)
###################################################
## plotTsTile(chl27)


###################################################
### code chunk number 53: wq-package.Rnw:491-492
###################################################
print(plotTsTile(chl27))


