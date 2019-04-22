
##### Volatility Modeling #####

# KOSPI200 Analysis #
getSymbols("KOSPI200.KS", from = "2014-01-01", to  = Sys.Date())
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
library(quantmod)
getSymbols("^KS11", from = "2013-01-01", to = Sys.Date())
ret.kospi <- dailyReturn(Cl(KS11), type = 'log')
ret.kospi
chartSeries(ret.kospi)

# Garch (1,1) #

garch11.spec = ugarchspec(variance.model = list(model = "sGARCH", garchorder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
garch11.spec

# MLE fitting
kospi.garch11.fit = ugarchfit(spec = garch11.spec, data = ret.kospi)
coef(kospi.garch11.fit) #estimation
vcov(kospi.garch11.fit) # cov-var matrix
infocriteria(kospi.garch11.fit)
newsimpact(kospi.garch11.fit) # news impact
signbias(kospi.garch11.fit) # Engle - Ng Bias Test
fitted(kospi.garch11.fit) # fittied data
residuals(kospi.garch11.fit) # residuals
uncvariance(kospi.garch11.fit) # no condition var
uncmean(kospi.garch11.fit) # no condition mean

# NewsImpact-GARCH(1,1)

ni.garch11 <- newsimpact(kospi.garch11.fit)
plot(ni.garch11$zx, ni.garch11$zy, type = "l", lwd = 2, col = "blue", main  ="GARCH(1,1)-News Impact", ylab=ni.garch11$yexpr, xlab = ni.garch11$xexpr)
# symmetic -> no assymetric impact

### Exponential GARCH ###

egarch11.spec = ugarchspec(variance.model  = list(model = "eGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
kospi.egarch11.fit = ugarchfit(spec = egarch11.spec, data = ret.kospi)
coef(kospi.egarch11.fit)

ni.egarch11 <- newsimpact(kospi.egarch11.fit)
plot(ni.egarch11$zx, ni.egarch11$zy, type = "l", lwd = 2, col = "blue", main = "EGARCH(1,1)- News Impact",
     ylab = ni.egarch11$yexpr, xlab = ni.egarch11$xexpr)

### Threshold GARCH (TGARCH) Model ###

tgarch11.spec= ugarchspec(variance.model = list(model = "fGARCH", submodel = "TGARCH", garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)))
kospi.tgarch11.fit = ugarchfit(spec = tgarch11.spec, data = ret.kospi)
coef(kospi.tgarch11.fit)

ni.tgarch11 <- newsimpact(kospi.tgarch11.fit)
plot(ni.tgarch11$zx, ni.tgarch11$zy, type = "l", lwd = 2, col = "blue", main = "TGARCH(1,1) - News Impact",
     ylab = ni.tgarch11$yexpr, xlab = ni.tgarch11$xexpr) # 0 point : turning point

#### Simulation and Forecasting ####

garch11sim.spec = ugarchspec(variance.model = list(garchOrder = c(1,1)), mean.model = list(armaOrder=c(0,0)),
                             fixed.pars=list(mu= 0, omega = 0.1, alpha1 = 0.1, beta1 = 0.7))
garch11.sim = ugarchpath(garch11sim.spec, n.sim = 1000)
kospi.garch11.fit = ugarchfit(spec = garch11.spec, data = ret.kospi, out.sample = 20)
kospi.garch11.fcst = ugarchforecast(kospi.garch11.fit, n.ahead = 10, n.roll = 10)
plot(kospi.garch11.fcst, which = 'all')     
