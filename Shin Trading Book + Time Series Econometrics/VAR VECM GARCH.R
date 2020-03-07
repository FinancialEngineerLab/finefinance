### Shin's Trading ###

## Author : Shin Hyunjin ##

## Time Series Econometris for forecasting and Trading ##

#------------------------------------------------------------------------------------------#
#install.packages("vars")
#install.packages("tseries")
#install.packages("moments")
#install.packages("rugarch")
library(rugarch)
library(moments)
library(Quandl)
library(vars)
library(urca)
library(quantmod)
library(tseries)

#------------------------------------------------------------------------------------------#

## Basic Cointegration Simulation ##

set.seed(1)
N <- 10000
x <- cumsum(rnorm(N))
gamma <- 0.7 # initial param
y <- gamma * x + rnorm(N)
plot(x, type='l', col = "blue")
lines(y, col = "red")
legend(50, 30, c("x", "y"), lty =c(1,1), lwd=c(2.5, 2.5), col = c("blue", "red"))

# 

summary(ur.df(x, type = "none"))
summary(ur.df(y, type = "none"))

# new linear combination model : z #

z = y - gamma*x # 
plot(z, type='l') # white noise

summary(ur.df(z, type = "none"))

### Vector Autoregressive Model ###

### Gold, Copper, 3Year Bond ##
getSymbols('DTB3', src ='FRED')
getSymbols('SNP', from = '2018-01-02')
getSymbols('DX-Y.NYB', from = '2018-01-02')

dollar <- `DX-Y.NYB`
#dollar
dollar <- dollar$`DX-Y.NYB.Adjusted`
colnames(dollar) <- c("dollar")
dollar
#dollar
SNP <- SNP$SNP.Adjusted
DTB3.sub <- DTB3['2008-01-02/2020-03-07']

dollar.ret <- diff(log(dollar))
SNP.ret <- diff(log(SNP))

dataDaily <- na.omit(merge(SNP.ret, dollar.ret, DTB3.sub), join = 'inner')

var1 <- VAR(dataDaily, lag.max =4, ic = "AIC")
VARselect(dataDaily, lag.max= 4)
summary(var1)
var1

var.pred <- predict(var1, n.ahead =10, ci=0.95)

var.irf <- irf(var1) # impulse response
plot(var.irf)

#SVAR #

amat <- diag(3)
amat[2,1] <- NA
amat[2,3] <- NA
amat[3,1] <- NA
svar1 <- SVAR(var1, estmethod = 'direct', Amat = amat)
irf.svar1 <- irf(svar1)
plot(irf.svar1)

#"LBMA/GOLD", 런던 금시장협회 (LBMA) 금 가격
#"LBMA/SILVER", 런던 금시장협회 (LBMA) 은 가격

#국제 구리 가격
#런던 금속 거래소(London Metal Exchange), 전 세계 비철금속거래의 지표
#Cash(현금가격), 3Months(3개월 선물가격), 15Months(15개월 선물가격) 등



#------------------------------------------------------------------------------------------#

#### Var and VECM Cointegraton ####


getSymbols('DTB3', src = 'FRED')
getSymbols('DTB6', src = 'FRED')

DTB3.sub = DTB3['1984-01-02/2020-03-07']
DTB6.sub = DTB6['1984-01-02/2020-03-07']
plot(DTB3.sub)
lines(DTB6.sub, col = 'red')

# regression

x1 = as.numeric(na.omit(DTB3.sub))
x2 = as.numeric(na.omit(DTB6.sub))
y = cbind(x1, x2)
cregr <- lm(x1 ~ x2)
r = cregr$residuals
r

# Phillips and Ouliaris Methods #
po.coint <- po.test(y, demean = TRUE, lshort = TRUE)
po.coint # pvalue low : cointegration, pvaleu high : no cointegration (h0)

# Johansen - Procedure
yJoTest = ca.jo(y, type = c("trace"), ecdet = c("none"), K=2)
summary(yJoTest) #  r<=1(cointegration) vs r = 0 (no cointegration)

yJoRegr = cajorls(yJoTest, r =1)
yJoRegr

#------------------------------------------------------------------------------------------#


##### Volatility Modeling #####

# dollar #
getSymbols('DX-Y.NYB', from = '2018-01-02')

dollar <- `DX-Y.NYB`
#dollar
dollar <- dollar$`DX-Y.NYB.Adjusted`
colnames(dollar) <- c("dollar")
dollar

ret <- dailyReturn(dollar, type = 'log')
ret

par(mfrow = c(2,2))
acf(ret, main = "Return ACF")
pacf(ret, main = "Return PACF")

acf(ret^2, main = "Squared return ACF")
pacf(ret^2, main = "Squared return PACF")
par(mfrow  =c(1,1))

# plotting #

m = mean(ret)
s = sd(ret)
#tail, normal vs empirical
hist(ret, nclass= 40, freq= FALSE, main = 'Return histogram')
curve(dnorm(x, mean = m, sd=s), from = -0.3, to = 0.2, add =TRUE, col = "red") #add : adding existing graph
plot(density(ret), main = 'Return empirical distribution') # empirical distribution : density
curve(dnorm(x, mean = m, sd = s), from = -0.3, to = 0.2, add =TRUE, col = "red")
par(mfrow=c(1,1))

# skewness

kurtosis(ret)

#tail zooming

plot(density(ret), main = "Return DEF - upper tail")#, xlim = c(0.1,0.2), ylim = c(0,2))
curve(dnorm(x, mean = m, sd= s), from = -0.3, to = 0.2, add= TRUE, col = "red")

# log Scale

plot(density(ret), xlim = c(-5*s, 5*s), log = 'y', main = "Density on log-scale")
curve(dnorm(x, mean = m, sd= s), from = -5*s, to = 5*s, log = "y", add =TRUE, col = "red")

# QQ plot
qqnorm(ret)
qqline(ret)
#-----------------------------------------------------------------------------------------------------#
### GARCH Modeling ###

chartSeries(ret)

# Garch (1,1) #

garch11.spec = ugarchspec(variance.model = list(model = "sGARCH",
                                                garchorder = c(1,1)),
                          mean.model = list(armaOrder=c(0,0)))
garch11.spec

# MLE fitting
dollar.garch11.fit = ugarchfit(spec = garch11.spec, data = ret)
coef(dollar.garch11.fit) #estimation
vcov(dollar.garch11.fit) # cov-var matrix
infocriteria(dollar.garch11.fit)
newsimpact(dollar.garch11.fit) # news impact
signbias(dollar.garch11.fit) # Engle - Ng Bias Test
fitted(dollar.garch11.fit) # fittied data
residuals(dollar.garch11.fit) # residuals
uncvariance(dollar.garch11.fit) # no condition var
uncmean(dollar.garch11.fit) # no condition mean

# NewsImpact-GARCH(1,1)

ni.garch11 <- newsimpact(dollar.garch11.fit)
plot(ni.garch11$zx, ni.garch11$zy, type = "l", lwd = 2, col = "blue", main  ="GARCH(1,1)-News Impact", ylab=ni.garch11$yexpr, xlab = ni.garch11$xexpr)
# symmetic -> no assymetric impact

### Exponential GARCH ###

egarch11.spec = ugarchspec(variance.model  = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
dollar.egarch11.fit = ugarchfit(spec = egarch11.spec, data = ret)
coef(dollar.egarch11.fit)

ni.egarch11 <- newsimpact(dollar.egarch11.fit)
plot(ni.egarch11$zx, ni.egarch11$zy, type = "l", lwd = 2, col = "blue", main = "EGARCH(1,1)- News Impact",
     ylab = ni.egarch11$yexpr, xlab = ni.egarch11$xexpr)

### Threshold GARCH (TGARCH) Model ###

tgarch11.spec= ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
dollar.tgarch11.fit = ugarchfit(spec = tgarch11.spec, data = ret)
coef(dollar.tgarch11.fit)

ni.tgarch11 <- newsimpact(dollar.tgarch11.fit)
plot(ni.tgarch11$zx, ni.tgarch11$zy, type = "l", lwd = 2, col = "blue", main = "TGARCH(1,1) - News Impact",
     ylab = ni.tgarch11$yexpr, xlab = ni.tgarch11$xexpr) # 0 point : turning point

#### Simulation and Forecasting ####

garch11sim.spec = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)),
                             fixed.pars=list(mu= 0, omega = 0.1, alpha1 = 0.1, beta1 = 0.7))
garch11.sim = ugarchpath(garch11sim.spec, n.sim = 1000)
dollar.garch11.fit = ugarchfit(spec = garch11.spec, data = ret, out.sample = 20)
dollar.garch11.fcst = ugarchforecast(dollar.garch11.fit, n.ahead = 10, n.roll = 10)
plot(dollar.garch11.fcst, which = 'all')     
