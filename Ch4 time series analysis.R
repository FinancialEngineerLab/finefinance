### Ch4 Time Series Analysis ###
  
### 3. ARCH GARCH 시리즈 ###

## 3-1 GARCH BAsic ##
install.packages("rugarch")
library(rugarch)

gspec.ru <- ugarchspec(mean.model = list(armaOrder = c(0,0)),distribution = "std")
gfit.ru <- ugarchfit(gspec.ru, price)
coef(gfit.ru)

#forecast
FutureForecast = ugarchforecast(gfit.ru, n.ahead=5)
FutureForecast

## 3-2 EGARCH -> 지수형 GARCH로서 시장시나리오에 적합함 ##
# return form + no N/A data
return <- na.omit(return)
egarchetf.spec = ugarchspec(variance.model = list(model = "eGARCH", garchOrder=c(1,1)),mean.model = list(armaOrder = c(0,0)))
egarchetf.fit = ugarchfit(egarchetf.spec, return)
egarchetf.fit
coef(egarchetf.fit)

FutureForecast = ugarchforecast(egarchetf.fit, n.ahead =5)
FutureForecast

## 3-3 VGARCH -> 벡터 GARCH 및 다변량 GARCH라고 한다.##

install.packages("rmgarch")
install.packages("PerformanceAnalytics")
library(rmgarch)
library(PerformanceAnalytics)

getSymbols("114800.KS", from = "2018-01-01", to = "2018-12-31")
data1 <- `114800.KS`
price1 <- data1$`114800.KS.Close`
return1 <- Delt(price1)
return1 <- na.omit(return1)

getSymbols("122630.KS", from = "2018-01-01", to = "2018-12-31")
data2 <- `122630.KS`
price2 <- data2$`122630.KS.Close`
return2 <- Delt(price2)
return2 <- na.omit(return2)

data <- cbind(return1, return2)

garch_spec = ugarchspec(mean.model = list(armaOrder = c(2,1)), variance.model = list(garchOrder = c(1,1),model = "sGARCH"),distribution.model = "norm")
dcc.garch_spec = dccspec(uspec = multispec(replicate(2, garch_spec)), dccOrder = c(1,1),distribution = "mvnorm")
dcc_fit = dccfit(dcc.garch_spec, data = data)
fcst = dccforecast(dcc_fit, n.ahead =5)
fcst

## 3-4 DCC 동적조건부 상관관계-> 간결하고 설명력 좋음.##

garch_spec2 = ugarchspec(mean.model = list(armaOrder = c(0,0)), variance.model = list(garchOrder = c(1,1),model = "sGARCH"),distribution.model = "norm")
dcc.garch_spec2 = dccspec(uspec = multispec(replicate(2, garch_spec2)), dccOrder = c(1,1),distribution = "mvnorm")
dcc_fit2 = dccfit(dcc.garch_spec2, data = data, fit.control = list(scale=TRUE))
dcc_fit2
