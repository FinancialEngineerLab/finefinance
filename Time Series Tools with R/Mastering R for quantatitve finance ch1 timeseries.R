##### 금융공학 R로 마스터하기 #####

install.packages('urca')
install.packages('vars')
install.packages('quantmod')
install.packages('tseries')
install.packages('moments')

library('quantmod')
library('urca')
library('vars')
library('tseries')
library('moments')

### Ch1. Time Series ###
## Lists :
#Multivariate time series analysis 
#Cointegration
#Vector autoregressive models
#VAR implementation example
#Cointegrated VAR and VECM
#Volatility modeling
#GARCH modeling with the rugarch package
#The standard GARCH model
#The Exponential GARCH model (EGARCH)
#The Threshold GARCH model (TGARCH)
#Simulation and forecasting
## Cointegration ##

## Multivariate time series analysis ##
# Generate two time series of length 100

set.seed(20140623) #fix the random seed
N <- 1000 #define length
x <- cumsum(rnorm(N)) #normal randomwalk
gamma <- 0.7 #initial value
y <- gamma*x + rnorm(N) #cointegration series equation
plot(x, type='l')
lines(y, col = "red")

# statistical tests -> ADF test (Augmented Dickey-Fuller test)
summary(ur.df(x, type="none"))
summary(ur.df(y, type="none"))

#linear combiation of multiple time series
z = y-gamma*x # linear combitation of time series, y (above)
plot(z, type='l')
summary(ur.df(z, type="none"))
#results is whitenoise

#Engle-Granger method -> conintegration test#
#estimate conintegration relationship
coin <- lm(y~x-1) # regression without intercept
coin$resid #residual gotten
summary(ur.df(coin$resid)) #ADF test of residulas


## Vector Autoregressive Models ##
#VAR implemenation example
getSymbols('^GSPC', from = '2017-11-22', to = '2018-11-22')
getSymbols('^IXIC', from = '2017-11-22', to = '2018-11-22')
getSymbols('DTB3', src = 'FRED')

# Elementes 추출 
Cl(GSPC)
Op(GSPC)
Hi(GSPC)
Lo(GSPC)
ClCl(GSPC) # close to close return 
Ad(GSPC)

chartSeries(ClCl(GSPC)) # plotting shortcuts
  
# interest rate
DTB3.sub <- DTB3['2017-11-22/2018-11-22']
GSPC.ret <- diff(log(Ad(GSPC)))
IXIC.ret <- diff(log(Ad(IXIC)))

dataDaily  <- na.omit(merge(GSPC.ret, IXIC.ret, DTB3.sub), join='inner')

GSPC.M <- to.monthly(GSPC.ret)$GSPC.ret.Close
MSFT.M <- to.monthly(MSFT.ret)$MSFT.ret.Close
DTB3.M <- to.monthly(DTB3.sub)$DTB3.sub.close

# vAR modeling
var1 <- VAR(dataDaily, lag.max=4, ic = "AIC")
VARselect(dataDaily, lag.max=4)
summary(var1)
var1

plot(var1)
coef(var1)
residuals(var1)
fitted(var1)
Phi(var1)

var.pred <- predict(var1, n.ahead=10, ci=0.95) #VAR predction

var.irf <- irf(var1)
plot(var.irf)

# SVAR
amat <- diag(3)
amat[2,1] <- NA
amat[2,3] <- NA
amat[3,1] <- NA

svar1 <- SVAR(var1, estmethod = 'direct', Amat = amat)
irf.svar1 <- irf(svar1)
plot(irf.svar1)

#### Cointegrated VAR and VCEM ####
## VCEM : Vector  Error Correlation Model ##

getSymbols('DTB3', src = 'FRED')
getSymbols('DTB6', src = 'FRED')
DTB3.sub = DTB3['1988-01-02/2018-11-22']
DTB6.sub = DTB6['1988-01-02/2018-11-22']
plot(DTB3.sub)
lines(DTB6.sub, col='red')

x1 = as.numeric(na.omit(DTB3.sub))
x2 = as.numeric(na.omit(DTB6.sub))
y = cbind(x1,x2)
cregr <- lm(x1 ~ x2)
r = cregr$residuals

#Phillips & Ouliaries Test
po.coint <- po.test(y, demean =TRUE, lshort = TRUE)

# Johansen procedure
yJoTest = ca.jo(y, type = c("trace"), ecdet = c("none"), K=2)
yJoRegr = cajorls(yJoTest, r=1) # r은 rank, OLS
yJoRegr

#### Volatility Modeling ####

getSymbols("KRW=X", from = "2013-11-22", to=Sys.Date())
chartSeries(Cl(`KRW=X`))

ret <- dailyReturn(Cl(`KRW=X`), type = 'log')

#Autocorrelation
par(mfrow = c(2,2))
acf(ret, main = "Return ACF")
pacf(ret, main = "Return PACF")
acf(ret^2, main = "Squared return ACF")
pacf(ret^2, main = "Squared return PACF")
par(mfrow = c(1,1))

#Histogram and Distribution
m = mean(ret)
s = sd(ret)
par(mfrow=c(1,2))
hist(ret, nclass=40, freq = FALSE, main='Return histogram')
curve(dnorm(x, mean=m, sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")

plot(density(ret), main='Return empirical distribution')
curve(dnorm(x, mean=m, sd=s), from = -0.3, to = 0.2, add=TRUE, col="blue")
par(mfrow=c(1,1))

kurtosis(ret)

#