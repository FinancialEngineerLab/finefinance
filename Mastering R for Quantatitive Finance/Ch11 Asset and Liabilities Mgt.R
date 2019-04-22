##### Ch11 : Asset and Liabilities Management #####

## 1. Data set ##
## data ##
portfolio <- read.csv("c:/users/shinhyunjin/dropbox/data/portfolio.csv")
market <- read.csv("c:/users/shinhyunjin/dropbox/data/market.csv") ## libor rate

## data cleaning ##

portfolio$issue <- as.Date(portfolio$issue, format = "%m/%d/%Y")
portfolio$maturity <- as.Date(portfolio$maturity, format = "%m/%d/%Y")
market$date <- as.Date(market$date, format = "%m/%d/%Y")

head(portfolio)
levels(portfolio$account_name)
head(market)

NOW <- as.Date("09/30/2014", format = "%m/%d/%Y")

#### 2. Cash flow function ####

source("C:/users/shinhyunjin/dropbox/data/bankALM.R")

# basic cashflow
cf(rate = 0.1, maturity = 3, volume=100, type = "BULLET")

# fitted spot yield curve #

test.date <- seq(from = as.Date("09/30/2015", format = "%m/%d/%Y"), to = as.Date("09/30/2035",
                                                                                 format = "%m/%d/%Y"),
                 by = "1 year")

get.yieldcurve.spot(market, test.date, type ="EUR01", now = NOW, showplot = TRUE)

# repricing #

test.reprice.date <- test.date[seq(from =1,to=20, by=2)]
test.forward <- get.yieldcurve.forward(market, test.reprice.date, type="EUR01", now = NOW)
test.floating <- get.floating(market, test.date, test.reprice.date, type="EUR01",
                              now = NOW, showplot = TRUE)
# 변동금리예측 : step별

#### 현금흐름 준비  ####

cashflow.table <- do.call(rbind, lapply(1:NROW(portfolio),
                                        function(i) cf.table(portfolio, market, now = NOW, id = i)))
head(cashflow.table)

## PV table ##

presentvalue.table <- do.call(rbind, lapply(1:NROW(portfolio),
                                            function(i) pv.table(cashflow.table[cashflow.table$id
                                                                                == portfolio$id[i],],
                                                                 market, now = NOW)))
head(presentvalue.table)
sum(presentvalue.table$presentvalue)



##### 3. 금리리스크관리 #####

nii <- nii.table(cashflow.table, now = NOW)
round(nii[,1:7], 2)

### visualization : density graph ###
barplot(nii, density = 5*(1:(NROW(nii)-1)), xlab = "Mat",
        cex.names = 0.8, ylab = "EUR", cex.axis = 0.8,
        args.legend = list(x="right"))

# sub 제목 
title(main = "Net interest income table", cex =0.8, sub = paste("Actual date : ",
                                                                as.character(as.Date(NOW))))

par(fig=c(0,1,0,1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

legend("right", legend = row.names(nii[1:NROW(nii-1),]), density = 5*(1:(NROW(nii)-1)),
bty = "n", cex =1) #NROW(nii-1) : total 제외


### repricing gap table ###
(repgap <- repricing.gap.table(portfolio, now = NOW))
#

barplot(repgap, col = "gray", xlab = "Months", ylab = "EUR")
title(main = "Repricing gap table", cex = 0.8,
      sub = paste("Actual Date: ", as.character(as.Date(NOW))))


##### 4. 유동성 리스크관리 #####

lq <- lq.table(cashflow.table, now = NOW)
round(lq[,1:5], 2)

barplot(lq, density = 5*(1:(NROW(nii)-1)), xlab = "Mat",
        cex.names = 0.8, ylab = "EUR", cex.axis = 0.8,
        args.legend = list(x="right"))

# sub 제목 

title(main = "Liquidity Gap table", cex =0.8, sub = paste("Actual date : ",
                                                                as.character(as.Date(NOW))))

par(fig=c(0,1,0,1), oma = c(0,0,0,0), mar = c(0,0,0,0), new = TRUE)
plot(0,0, type = "n", bty = "n", xaxt = "n", yaxt = "n")

legend("right", legend = row.names(nii[1:NROW(nii-2),]), density = 5*(1:(NROW(nii)-2)),
       bty = "n", cex =1) #NROW(nii-1) : total 제외



##### Non maturity deposit NMD 비만기예금 모델 #####
#install.packages("urca")

library(car)
library(urca)
library(dynlm)

nmd <- read.csv("c:/users/shinhyunjin/dropbox/data/ecb_nmd_data.csv")
nmd$date <- as.Date(nmd$date, format = "%m/%d/%Y")


#Plotting deposit coupon and 1 month Euribor
plot(nmd$eur1m ~ nmd$date, type="l", xlab="Time", ylab="Interest rate")
lines(nmd$cpn~ nmd$date, type="l", col="red")
title(main="Time series", cex=0.8 )
legend("topright", legend = c("Coupon","EUR 1M"), 
       fill =  c("red","black"), bty = "n", cex=1)

attach(nmd)

#Unit root test (ADF)
cpn.ur <- ur.df(cpn, type="none", lags=2)
dcpn.ur <- ur.df(diff(cpn), type="none", lags=1)
eur1m.ur <- ur.df(eur1m, type="none", lags=2)
deur1m.ur <- ur.df(diff(eur1m), type="none", lags=1)
sumtbl <- matrix(cbind(cpn.ur@teststat, cpn.ur@cval, dcpn.ur@teststat, dcpn.ur@cval, eur1m.ur@teststat, eur1m.ur@cval, deur1m.ur@teststat, deur1m.ur@cval), nrow=4)
colnames(sumtbl) <- c("cpn", "diff(cpn)", "eur1m", "diff(eur1m)")
rownames(sumtbl) <- c("Test stat", "1pct CV", "5pct CV", "10pct CV")

#Stationarty test (KPSS)
cpn.kpss <- ur.kpss(cpn, type="mu")
eur1m.kpss <- ur.kpss(eur1m, type="mu")

sumtbl <- matrix(cbind( cpn.kpss@teststat, cpn.kpss@cval, eur1m.kpss@teststat, eur1m.kpss@cval), nrow=5)
colnames(sumtbl) <- c("cpn", "eur1m")
rownames(sumtbl) <- c("Test stat", "10pct CV", "5pct CV", "2.5pct CV", "1pct CV")

print(cpn.ur@test.name)
print(sumtbl)
print(cpn.kpss@test.name)
print(sumtbl)

#Residual test of cointegrating equation
lr <- lm(cpn ~ eur1m)
res <- resid(lr)
lr$coefficients
res.ur <- ur.df(res, type="none", lags=1)
summary(res.ur)

library(dynlm)
res <- resid(lr)[2:length(cpn)]
dy <- diff(cpn)
dx <- diff(eur1m)
detach(nmd)
ecmdata <- c(dy, dx, res)
ecm <- dynlm(dy ~ L(dx, 1) + L(res, 1), data = ecmdata)
summary(ecm)


#### Static Replication ####

ecb.yc <- read.csv("C:/users/shinhyunjin/dropbox/data/ecb_yc_data.csv")
ecb.yc$date <- as.Date(ecb.yc$date, format = "%d/%m/%Y")

#

matplot(ecb.yc$date, ecb.yc[,2:6], type = "l", lty = (1:5), lwd = 2,
        col =1, xlab= "Time", ylab = "Yiled", ylim = c(0,6), xaxt = "n")
legend("topright", cex = 0.8, bty ="n", lty = c(1:5), lwd =2, legend = colnames(ecb.yc[,2:6]))
title(main  = "ECB yield curve", cex = 0.8)
axis.Date(1, ecb.yc$date)

## Optimization ##
install.packages("quadprog")
library(quadprog)

b <- nmd$cpn[21:135]
b
A<- cbind(ecb.yc$EUR1M, ecb.yc$EUR3M, ecb.yc$EUR1Y, ecb.yc$EUR5Y, ecb.yc$EUR10Y)
A

m <- c(1,3,12,60,120)
l <- 60

stat.opt <- solve.QP(t(A)%*% A, t(b)%*% A,
                     cbind(matrix(1, nr = 5, nc = 1),
                           matrix(m, nr = 5, nc = 1),
                           diag(5)),
                     c(1, l, 0,0,0,0,0), meq =2)
sumtbl <- matrix(round(stat.opt$solution * 100, digits = 1), nr = 1)
colnames(sumtbl) <- c("1M", "3M", "1Y", "5Y", "10Y")
cat("Portfolio weights in%")
print(sumtbl) ## portfolio replicate weights#

#

mrg <- nmd$cpn[21:135] - stat.opt$solution[2]*ecb.yc$EUR3M +
  stat.opt$solution[5]*ecb.yc$EUR10Y
plot(mrg ~ ecb.yc$date, type = "l", col = "black", xlab = "Time", ylab = "%")
title(main = "Margin of static replication", cex =0.8)
