#### Var and VECM Cointegraton ####

install.packages("quantmod")
library(quantmod)


getSymbols('DTB3', src = 'FRED')
getSymbols('DTB6', src = 'FRED')

DTB3.sub = DTB3['1984-01-02/2019-03-21']
DTB6.sub = DTB6['1984-01-02/2019-03-21']
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
install.packages("tseries")
library(tseries)

po.coint <- po.test(y, demean = TRUE, lshort = TRUE)
po.coint # pvalue low : cointegration, pvaleu high : no cointegration (h0)

# Johansen - Procedure
install.packages("urca")
library(urca)
yJoTest = ca.jo(y, type = c("trace"), ecdet = c("none"), K=2)
summary(yJoTest) #  r<=1(cointegration) vs r = 0 (no cointegration)

yJoRegr = cajorls(yJoTest, r =1)
yJoRegr


##### Volatility Modeling #####

# S&P 500 Analysis #
getSymbols("SNP", from = "2015-01-01", to  = Sys.Date())
chartSeries(Cl(SNP))

ret <- dailyReturn(Cl(SNP), type = 'log')
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
install.packages("moments")
library(moments)
kurtosis(ret)

#tail zooming

plot(density(ret), main = "Return DEF - upper tail", xlim = c(0.1,0.2), ylim = c(0,2))
curve(dnorm(x, mean = m, sd= s), from = -0.3, to = 0.2, add= TRUE, col = "red")

# log Scale

plot(density(ret), xlim = c(-5*s, 5*s), log = 'y', main = "Density on log-scale")
curve(dnorm(x, mean = m, sd= s), from = -5*s, to = 5*s, log = "y", add =TRUE, col = "red")

# QQ plot
qqnorm(ret)
qqline(ret)

### GARCH Modeling ###

install.packages("rugarch")
library(rugarch)

getSymbols("AAPL", from = "2013-01-01", to = "2019-03-22")
ret.aapl <- dailyReturn(Cl(AAPL), type = 'log')
ret.aapl
chartSeries(ret.aapl)

# Garch (1,1) #

garch11.spec = ugarchspec(variance.model = list(model = "sGARCH", garchorder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
garch11.spec

# MLE fitting
aapl.garch11.fit = ugarchfit(spec = garch11.spec, data = ret.aapl)
coef(aapl.garch11.fit) #estimation
vcov(aapl.garch11.fit) # cov-var matrix
infocriteria(aapl.garch11.fit)
newsimpact(aapl.garch11.fit) # news impact
signbias(aapl.garch11.fit) # Engle - Ng Bias Test
fitted(aapl.garch11.fit) # fittied data
residuals(aapl.garch11.fit) # residuals
uncvariance(aapl.garch11.fit) # no condition var
uncmean(aapl.garch11.fit) # no condition mean

# NewsImpact-GARCH(1,1)

ni.garch11 <- newsimpact(aapl.garch11.fit)
plot(ni.garch11$zx, ni.garch11$zy, type = "l", lwd = 2, col = "blue", main  ="GARCH(1,1)-News Impact", ylab=ni.garch11$yexpr, xlab = ni.garch11$xexpr)
# symmetic -> no assymetric impact

### Exponential GARCH ###

egarch11.spec = ugarchspec(variance.model  = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
aapl.egarch11.fit = ugarchfit(spec = egarch11.spec, data = ret.aapl)
coef(aapl.egarch11.fit)

ni.egarch11 <- newsimpact(aapl.egarch11.fit)
plot(ni.egarch11$zx, ni.egarch11$zy, type = "l", lwd = 2, col = "blue", main = "EGARCH(1,1)- News Impact",
     ylab = ni.egarch11$yexpr, xlab = ni.egarch11$xexpr)

### Threshold GARCH (TGARCH) Model ###

tgarch11.spec= ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
aapl.tgarch11.fit = ugarchfit(spec = tgarch11.spec, data = ret.aapl)
coef(aapl.tgarch11.fit)

ni.tgarch11 <- newsimpact(aapl.tagrch11.fit)
plot(ni.tgarch11$zx, ni.tgarch11$zy, type = "l", lwd = 2, col = "blue", main = "TGARCH(1,1) - News Impact",
     ylab = ni.tgarch11$yexpr, xlab = ni.tgarch11$xexpr) # 0 point : turning point

#### Simulation and Forecasting ####

garch11sim.spec = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)),
                             fixed.pars=list(mu= 0, omega = 0.1, alpha1 = 0.1, beta1 = 0.7))
garch11.sim = ugarchpath(garch11sim.spec, n.sim = 1000)
aapl.garch11.fit = ugarchfit(spec = garch11.spec, data = ret.aapl, out.sample = 20)
aapl.garch11.fcst = ugarchforecast(aapl.garch11.fit, n.ahead = 10, n.roll = 10)
plot(aapl.garch11.fcst, which = 'all')     
