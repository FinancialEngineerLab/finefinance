rm(list=ls())


### data ###
raw.data <- read.csv('C:/Users/Shinhyunjin/Dropbox/data/KOSPI_indices.csv')
data <- raw.data

##### Econometrics Class Codes #####

#Fgarch install
install.packages("fGarch")
library("fGarch")

#ARMA GARCH install
install.packages("rugarch")
library(rugarch)

#### 1. ARMA ####

#Close price

Price=log(data$Close)
return=diff(Price)
return.abs=abs(return)

par(mfrow=c(2,2))
plot(Price,type="l")
plot(diff(Price), type="l")
acf(Price)
pacf(Price)

par(mfrow=c(2,2))
plot(return,type="l")
acf(return)
pacf(return)

par(mfrow=c(2,2))
acf(return.abs)
pacf(return.abs)
acf(return^2)
pacf(return^2)


#Fitting
obj1=arima(Price, order = c(1,0,1))
tsdiag(obj1)

obj2=arima(Price, order = c(0,0,1))
tsdiag(obj2)

obj3=arima(Price, order = c(1,0,0))
tsdiag(obj3)

BIC(obj1, obj2, obj3)


#predict
predict(obj1, 100)


#### ARCH GARCH ####

#Close price

Price=log(data$kospi200)
return=diff(Price)
return.abs=abs(return)

par(mfrow=c(2,2))
plot(Price,type="l")
plot(diff(Price), type="l")
acf(Price)
pacf(Price)

par(mfrow=c(2,2))
plot(return,type="l")
acf(return)
pacf(return)

par(mfrow=c(2,2))
acf(return.abs, main="absolute of return (ACF)")
pacf(return.abs, main="absolute of return (PACF)")
acf(return^2)
pacf(return^2)


install.packages("fGarch")
library("fGarch")


#different distributions

obj1=garchFit(formula = ~ garch(1, 1), data=return,
cond.dist =  "norm")

obj2=garchFit(formula = ~ garch(1, 1), data=return,
cond.dist =  "QMLE")

obj3=garchFit(formula = ~ garch(1, 1), data=return,
cond.dist =  "std")

coef(obj1)
coef(obj2)
coef(obj3)

summary(obj2)


#Residuals
Res=obj2@residuals
par(mfrow=c(3,2))
plot(Res)
qqnorm(Res)
acf(Res)
pacf(Res)
acf(Res^2)
pacf(Res^2)

par(mfrow=c(1,1))
plot(Price)

mean(return==0)


### ARMA-GARCH ###
install.packages("rugarch")
library(rugarch)
 

spec = ugarchspec(variance.model = list(model = "sGARCH", 
                                         garchOrder = c(1, 1), 
                                         submodel = NULL, 
                                         external.regressors = NULL, 
                                         variance.targeting = FALSE), 

                   mean.model     = list(armaOrder = c(1, 1), 
                                         external.regressors = NULL, 
                                         distribution.model = "norm", 
                                         start.pars = list(), 
                                         fixed.pars = list()))

obj4= ugarchfit(spec = spec, data = return, solver.control = list(trace=0))

Res=return-fitted(obj4) 
?fitted

#Residuals
par(mfrow=c(3,2))
plot(Res)
qqnorm(Res)
acf(Res)
pacf(Res)
acf(Res^2)
pacf(Res^2)

