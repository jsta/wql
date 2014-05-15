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
### code chunk number 5: wq-package.Rnw:99-102
###################################################
sfb <- wqData(sfbay, c(1, 3:4), 5:12, site.order = TRUE,
              type = "wide", time.format = "%m/%d/%Y")
head(sfb)


###################################################
### code chunk number 6: wq-package.Rnw:107-108
###################################################
summary(sfb)


###################################################
### code chunk number 7: wq-package.Rnw:113-114 (eval = FALSE)
###################################################
## plot(sfb, vars = c('dox', 'temp'), num.col = 2)


###################################################
### code chunk number 8: wq-package.Rnw:120-121
###################################################
print(plot(sfb, vars = c('dox', 'temp'), num.col = 2))


###################################################
### code chunk number 9: wq-package.Rnw:134-137
###################################################
y <- tsMake(sfb, focus = "chl", layer = c(0, 5))
y[1:6, ]
tsp(y)


###################################################
### code chunk number 10: wq-package.Rnw:139-140 (eval = FALSE)
###################################################
## plotTs(y, ylab = "Chlorophyll in San Francisco Bay", ncol = 2)


###################################################
### code chunk number 11: wq-package.Rnw:146-148
###################################################
print(plotTs(y, ylab = "Chlorophyll in San Francisco Bay",
             ncol = 2))


###################################################
### code chunk number 12: wq-package.Rnw:157-158
###################################################
head(tsMake(sfb, focus = "chl", layer = c(0, 5), type = 'zoo'))


###################################################
### code chunk number 13: wq-package.Rnw:163-167
###################################################
chl27 <- sfbayChla[, 's27']
tsp(chl27)
chl27 <- round(chl27, 1)
head(ts2df(chl27))


###################################################
### code chunk number 14: wq-package.Rnw:172-175
###################################################
y <- window(sfbayChla, start = 2005,
            end = c(2009, 12))  # 5 years, 16 sites
round(mts2ts(y, seas = 2:4), 1)  # focus on Feb-Apr spring bloom


###################################################
### code chunk number 15: wq-package.Rnw:184-185
###################################################
mannKen(Nile)


###################################################
### code chunk number 16: wq-package.Rnw:190-194
###################################################
y <- sfbayChla
y1 <- tsSub(y, seas = 2:4)  # focus on Feb-Apr spring bloom
y2 <- aggregate(y1, 1, mean, na.rm = FALSE)
signif(mannKen(y2), 3)


###################################################
### code chunk number 17: wq-package.Rnw:199-201
###################################################
chl27 <- sfbayChla[, "s27"]
seaKen(chl27)


###################################################
### code chunk number 18: wq-package.Rnw:206-207
###################################################
seaRoll(chl27, w = 10)


###################################################
### code chunk number 19: wq-package.Rnw:212-213
###################################################
x <- sfbayChla


###################################################
### code chunk number 20: wq-package.Rnw:215-216 (eval = FALSE)
###################################################
## seasonTrend(x, plot = TRUE, ncol = 2, scales = 'free_y')


###################################################
### code chunk number 21: wq-package.Rnw:222-223
###################################################
print(seasonTrend(x, plot = TRUE, ncol = 2, scales = 'free_y'))


###################################################
### code chunk number 22: wq-package.Rnw:232-234
###################################################
x <- sfbayChla[, 's27']
trendHomog(x)


###################################################
### code chunk number 23: wq-package.Rnw:239-241
###################################################
chl <-sfbayChla[, 1:12]  # first 12 stns have good data coverage
seaKen(mts2ts(chl, 2:4))  # regional trend in spring bloom


###################################################
### code chunk number 24: wq-package.Rnw:250-252
###################################################
chl27 <- sfbayChla[, "s27"]
chl27a <- interpTs(chl27, gap = 3)


###################################################
### code chunk number 25: wq-package.Rnw:254-256 (eval = FALSE)
###################################################
## plot(chl27a, col = "red", lwd = .5, xlab = "")
## lines(chl27, col = "blue", lwd = 1.5)


###################################################
### code chunk number 26: wq-package.Rnw:262-264
###################################################
plot(chl27a, col = "red", lwd = .5, xlab = "")
lines(chl27, col = "blue", lwd = 1.5)


###################################################
### code chunk number 27: wq-package.Rnw:273-275
###################################################
chla1 <- aggregate(sfbayChla, 1, mean, na.rm = TRUE)
chla1 <- chla1[, 1:12]


###################################################
### code chunk number 28: wq-package.Rnw:277-278 (eval = FALSE)
###################################################
## eofNum(chla1, distr = "lognormal", reps = 2000)


###################################################
### code chunk number 29: wq-package.Rnw:284-285
###################################################
print(eofNum(chla1, distr = "lognormal", reps = 2000))


###################################################
### code chunk number 30: wq-package.Rnw:294-296
###################################################
e1 <- eof(chla1, n = 1)
e1


###################################################
### code chunk number 31: wq-package.Rnw:301-302 (eval = FALSE)
###################################################
## eofPlot(e1, type = "amp")


###################################################
### code chunk number 32: wq-package.Rnw:308-309
###################################################
print(eofPlot(e1, type = "amp"))


###################################################
### code chunk number 33: wq-package.Rnw:318-321
###################################################
chl27b <- interpTs(sfbayChla[, "s27"], gap = 3)
chl27b <- ts2df(chl27b, mon1 = 10, addYr = TRUE, omit = TRUE)
head(round(chl27b, 1))


###################################################
### code chunk number 34: wq-package.Rnw:326-327
###################################################
e2 <- eof(chl27b, n = 2)


###################################################
### code chunk number 35: wq-package.Rnw:329-330 (eval = FALSE)
###################################################
## eofPlot(e2, type = "coef")


###################################################
### code chunk number 36: wq-package.Rnw:336-337
###################################################
print(eofPlot(e2, type = "coef"))


###################################################
### code chunk number 37: wq-package.Rnw:352-354
###################################################
chl27 <- sfbayChla[, "s27"]
d1 <- decompTs(chl27)


###################################################
### code chunk number 38: wq-package.Rnw:356-357 (eval = FALSE)
###################################################
## plot(d1, nc = 1, main = "Station 27 Chl-a decomposition")


###################################################
### code chunk number 39: wq-package.Rnw:363-364
###################################################
plot(d1, nc = 1, main = "Station 27 Chl-a decomposition")


###################################################
### code chunk number 40: wq-package.Rnw:373-375 (eval = FALSE)
###################################################
## plotSeason(chl27, num.era = 3, same.plot = FALSE,
##            ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 41: wq-package.Rnw:381-383
###################################################
print(plotSeason(chl27, num.era = 3, same.plot = FALSE,
                 ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 42: wq-package.Rnw:392-394 (eval = FALSE)
###################################################
## plotSeason(chl27, num.era = 3, same.plot = TRUE,
##            ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 43: wq-package.Rnw:400-402
###################################################
print(plotSeason(chl27, num.era = 3, same.plot = TRUE,
                 ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 44: wq-package.Rnw:411-412 (eval = FALSE)
###################################################
## plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a')


###################################################
### code chunk number 45: wq-package.Rnw:418-419
###################################################
print(plotSeason(chl27, "by.month", ylab = 'Stn 27 Chl-a'))


###################################################
### code chunk number 46: wq-package.Rnw:434-441
###################################################
chl27 <- sfbayChla[, 's27']
p1 <- phenoPhase(chl27)
head(p1)
p2 <- phenoPhase(chl27, c(1, 6))
head(p2)
p3 <- phenoAmp(chl27, c(1, 6))
head(p3)


###################################################
### code chunk number 47: wq-package.Rnw:446-452
###################################################
zchl <- tsMake(sfb, focus = "chl", layer = c(0, 5), type = 'zoo')
head(zchl)
zchl27 <- zchl[, 3]
head(phenoPhase(zchl27))
head(phenoPhase(zchl27, c(1, 6), out = 'doy'))
head(phenoPhase(zchl27, c(1, 6), out = 'julian'))


###################################################
### code chunk number 48: wq-package.Rnw:459-460
###################################################
chl <- aggregate(sfbayChla[, 1:6], 1, meanSub, 2:4, na.rm = TRUE)


###################################################
### code chunk number 49: wq-package.Rnw:462-464 (eval = FALSE)
###################################################
## plotTsAnom(chl, ylab = 'Chlorophyll-a', strip.labels =
##             paste('Station', substring(colnames(chl), 2, 3)))


###################################################
### code chunk number 50: wq-package.Rnw:470-473
###################################################
print(plotTsAnom(chl, ylab = 'Chlorophyll-a', strip.labels =
      paste('Station', substring(colnames(chl), 2, 3)))
)


###################################################
### code chunk number 51: wq-package.Rnw:483-484
###################################################
chl27 <- sfbayChla[, "s27"]


###################################################
### code chunk number 52: wq-package.Rnw:486-487 (eval = FALSE)
###################################################
## plotTsTile(chl27)


###################################################
### code chunk number 53: wq-package.Rnw:493-494
###################################################
print(plotTsTile(chl27))


###################################################
### code chunk number 54: wq-package.Rnw:505-514
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
### code chunk number 55: wq-package.Rnw:516-517 (eval = FALSE)
###################################################
## layOut(list(g1, 1:2, 1:6), list(g2, 1:2, 7:10), list(g3, 3:5, 1:8))


###################################################
### code chunk number 56: wq-package.Rnw:523-525
###################################################
print(layOut(list(g1, 1:2, 1:6), list(g2, 1:2, 7:10),
             list(g3, 3:5, 1:8)))


