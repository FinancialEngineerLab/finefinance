### Ch1. Time Series Analysis ###

library(quantmod)
library(zoo)
library(forecast)

##### introduction ###

## Time Series DATA ##
appl <- read.csv("C:/users/shinhyunjin/dropbox/data/aapl.csv", index = "Date")
appl <- read.zoo("C:/users/shinhyunjin/dropbox/data/aapl.csv",sep=",", header=TRUE, format="%Y-%m-%d")

head(appl)
tail(appl)
#appl[,1]
#index(appl) <- appl[,1]
appl[which.max(appl)]
which.max(appl) 

plot(appl, main = "APPLE Closing prices on NASDAQ", ylab = "PRICE in USD", xlab = "DATE")
appl

#
ret_simple <- diff(appl) /lag(appl, k=-1)*100 #discrete
ret_cont <- diff(log(appl))*100 #continous
summary(coredata(ret_simple))
quantile(ret_simple, probs = 0.01)
#
ret_simple[which.min(ret_simple)]
ret_simple[which.max(ret_simple)]
#
hist(ret_simple, breaks=100, main = "Histogram of simple returns", xlab = "%")
#
appl_2013 <- window(appl, start = '2013-01-01', end = '2013-12-31')
appl_2013[which.max(appl_2013)]

#### UK house price analysis ####

## Linear time series modeling and forecasting ##

hp <- read.zoo("C:/users/shinhyunjin/dropbox/data/UKHP.csv", sep =",", header =TRUE, format="%Y-%m", FUN =as.yearmon)
frequency(hp)
hp_ret <- diff(hp) / lag(hp, k=-1)*100
hp_ret

## Model identification and estimation ##

mod <- auto.arima(hp_ret, stationary = TRUE, seasonal = FALSE, ic = "aic")
mod # arima 2,0,0 estimation by that function

confint(mod)

## Model diagnostic checking ##

tsdiag(mod)

plot(mod$x, lty = 1,  main="UK house prices : raw data vs. fitted values", ylab ="Return in percent", xlab = "Date")
lines(fitted(mod), lty = 2, lwd = 2, col = "red")

accuracy(mod)

## Forecasting ##

predict(mod, n.ahead = 3)
plot(forecast(mod))

### volatitty forecasting for risk managment ###

intc <- read.zoo("C:/users/shinhyunjin/dropbox/data/UKHP.csv", header=TRUE,
                 sep = ",", format = "%Y-%m", FUN = as.yearmon)

plot(intc, main = "Monthly returns of Intel Corporation", xlab  = "date", ylab = "return in percent")

Box.test(coredata(intc^2), type = "Ljung-Box", lag =12)

#
install.packages("FinTS")
library(FinTS)

Box.test(coredata(intc^2), type = "Ljung-Box", lag=12)

ArchTest <- function (x, lags=12, demean = FALSE) 
{
  # Capture name of x for documentation in the output  
  xName <- deparse(substitute(x))
  # 
  x <- as.vector(x)
  if(demean) x <- scale(x, center = TRUE, scale = FALSE)
  #  
  lags <- lags + 1
  mat <- embed(x^2, lags)
  arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
  STATISTIC <- arch.lm$r.squared * length(resid(arch.lm))
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- lags - 1
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "ARCH LM-test;  Null hypothesis:  no ARCH effects"
  result <- list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name =
                   xName)
  class(result) <- "htest"
  return(result)
}

ArchTest(coredata(intc))

## GARCH model specification ##

library(rugarch)

intc_garch11_spec <- ugarchspec(variance.model = list(
  garchOrder = c(1,1)),
  mean.model = list(armaOrder = c(0,0)))

intc_garch11_spec

## GARCH Model estimation ##

intc_garch11_fit <- ugarchfit(spec = intc_garch11_spec, data = intc)
intc_garch11_fit

## Forecasting ###

intc_garch11_fcst <- ugarchforecast(intc_garch11_fit, n.ahead=12)
intc_garch11_fcst


