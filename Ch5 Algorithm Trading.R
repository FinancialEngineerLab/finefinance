### Ch5 Algorithm Trading ###

library(quantmod)
library(PerformanceAnalytics)
library(tseries)
library(fPortfolio)

## 5-1 모멘텀 / 방향성 트레이딩 ##

getSymbols("226490.KS", from = "2015-09-09", to= "2018-12-31")
getSymbols("169950.KS", from = "2015-09-09", to = "2018-12-31")

kospietf <- `226490.KS`
kospietf_p <- kospietf$`226490.KS.Close`
kospietf_p <- na.omit(kospietf_p)

kospietf_r <- Delt(kospietf_p, k = 1)
kospietf_r <- na.omit(kospietf_r)

chinaetf <- `169950.KS`
chinaetf_p <- chinaetf$`169950.KS.Close`
chinaetf_p <- na.omit(chinaetf_p)

chinaetf_r <- Delt(chinaetf_p, k = 1)
chinaetf_r <- na.omit(chinaetf_r)

#추세분석
par(mfrow = c(2,1))
plot(kospietf_p,type="l")
plot(kospietf_r, type = "l")

#날짜 나누기
in_sd <- "2015-09-09"
in_ed <- "2017-12-31"
out_sd <- "2018-01-01"
out_ed <- Sys.Date()

in_kospi <- kospietf_p[(index(kospietf_p) >= in_sd & index(kospietf_p) <= in_ed),]
in_kospi <- na.omit(in_kospi)
in_ret_kospit <- kospietf_r[(index(kospietf_r) >= in_sd & index(kospietf_r) <= in_ed),]
out_kospi <- kospietf_p[(index(kospietf_p) >= out_sd & index(kospietf_p) <= out_ed),]
out_kospi <- na.omit(out_kospi)
out_ret_kospit <- kospietf_r[(index(kospietf_r) >= out_sd & index(kospietf_r) <= out_ed),]


#Variables

macd <- MACD(in_kospi, nFast=7, nSlow = 12, nSig = 15, maType="SMA", percent = FALSE)
#macd : 이동평균선 사이의 관계 -> 추세방향과 주가 움직임 분석에 용이
bb <- BBands(in_kospi, n=20, maType="SMA", sd = 2)
#bb : 상하한선의 폭 분석, 변동성에 비례함, 과매수매도 분석
signal <- NULL
signal <- ifelse(in_kospi > bb[,'up'] & macd[,'macd'] > macd[,'signal'],1,ifelse(in_kospi<bb[,'dn']&macd[,'macd']<macd[,'signal'],-1,0))

#Investment Results
trade_return <- in_ret_kospit*lag(signal)
cumm_ret <- Return.cumulative(trade_return)
annual_ret <- Return.annualized(trade_return)

charts.PerformanceSummary(trade_return)
summary(as.ts(trade_return))

#Financial Results
maxDrawdown(trade_return)
StdDev(trade_return)
StdDev.annualized(trade_return)
VaR(trade_return, p = 0.95)
SharpeRatio(as.ts(trade_return), Rf = 0.018, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(trade_return, Rf = 0.018)

## 표본외 데이터 계산 #
#Variables
macd_o <- MACD(out_kospi, nFast=7, nSlow = 12, nSig = 15, maType="SMA", percent = FALSE)
#macd : 이동평균선 사이의 관계 -> 추세방향과 주가 움직임 분석에 용이
bb_o <- BBands(out_kospi, n=20, maType="SMA", sd = 2)
#bb : 상하한선의 폭 분석, 변동성에 비례함, 과매수매도 분석
signal_o <- NULL
signal_o <- ifelse(out_kospi > bb_o[,'up'] & macd_o[,'macd'] > macd_o[,'signal'],1,ifelse(out_kospi<bb_o[,'dn']&macd_o[,'macd']<macd_o[,'signal'],-1,0))

#Investment Results
trade_return_o <- out_ret_kospit*lag(signal_o)
cumm_ret_o <- Return.cumulative(trade_return_o)
annual_ret_o <- Return.annualized(trade_return_o)

charts.PerformanceSummary(trade_return_o)
summary(as.ts(trade_return_o))

#Financial Results
maxDrawdown(trade_return_o)
StdDev(trade_return_o)
StdDev.annualized(trade_return_o)
VaR(trade_return_o, p = 0.95)
SharpeRatio(as.ts(trade_return_o), Rf = 0.018, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(trade_return_o, Rf = 0.018)

## var-cov matrix ##

var(kospietf_r, na.rm = T)
var(chinaetf_r, na.rm = T)
var(kospietf_r + chinaetf_r, na.rm =T) #cov때문에 둘이 다를 수밖에 없다.

sd(kospietf_r, na.rm = T)
sd(chinaetf_rm na.rm = T)
cor(kospietf_r[!is.na(kospietf_r)], chinaetf_r[!is.na(chinaetf_r)])

pf_ret <- data.frame(matrix(NA, dim(kospietf_r)[1],2))
pf_ret[,1] <- kospietf_r
pf_ret[,2] <- chinaetf_r
pf_ret <- pf_ret[!is.na(pf_ret[,1]),]
cor(pf_ret)


### 5-2 Pair Trading ###

kospietf_r[1] <- 1
chinaetf_r[1] <- 1

norm_kospi <- apply(kospietf_r, 2, cumsum)
norm_china <- apply(chinaetf_r, 2, cumsum)

#Basic Results 
plot(norm_kospi, type="l", ylim = c(0.75,1.35),ylab="Normalized_Price",main="KODEX Kospi ETF & China ETF")
lines(norm_china, col = "red")
legend('topright', c("KOSPI_ETF", "CHINA_ETF"), lty = 1, col=c('black', 'red'),bty = 'o', cex=1)

#Advanced Results
norm_kospi <- xts(norm_kospi, index(kospietf_r))
norm_china <- xts(norm_china, index(chinaetf_r))

par(mfrow  = c(3,1))
#plot(norm_kospi, type="l", ylim = c(0.75,1.35),ylab="Normalized_Price",main="KODEX Kospi ETF & China ETF")
lines(norm_china, col = "red")
legend('topright', c("KOSPI_ETF", "CHINA_ETF"), lty = 1, col=c('black', 'red'),bty = 'o', cex=1)

diff = norm_kospi - norm_china
plot(diff, type = "l", ylab = "Normalized_Price_difference")

me <- mean(diff)
std <- sd(diff)
n<-1

ub <- me + n*std
lb <- me - n*std
signal <- ifelse(diff>ub, 1, ifelse(diff<lb, -1,0))
me_dynamic <- rollapply(diff, 10, mean)
std_dynamic <- rollapply(diff, 10, sd)
plot(signal, type = "l")

cost <- 0
spread_return <- kospietf_r - chinaetf_r
trade_return <- spread_return * lag(signal) - cost
summary(trade_return)

#Final Results
cumm_ret <- Return.cumulative(trade_return)
annual_ret <- Return.annualized(trade_return)
charts.PerformanceSummary(trade_return)
maxxdd <- maxDrawdown(trade_return)
sd <- StdDev(trade_return)
sda <- StdDev.annualized(trade_return)
VaR(trade_return, p = 0.95)
SharpeRatio(as.ts(trade_return), Rf = 0, p = 0.95, FUN = "StdDev")
SharpeRatio.annualized(trade_return, Rf = 0)

### 5-3 상관관게 기반 페어트레이딩 ###

data <- data.frame(matrix(NA, dim(kospietf_r)[1],2))
data[,1] <- kospietf_r
data[,2] <- chinaetf_r

data <- xts(data, index(kospietf_r))
correlation <- function(x){
  result <- cor(x[,1], x[,2])
  return(result)
}
corr <- rollapply(data,252,correlation, by.column= FALSE)
plot(corr)
#
hedge_ratio <- kospietf_p / chinaetf_p

roll_me <- rollapply(hedge_ratio, 14, mean)
roll_std <- rollapply(hedge_ratio, 14, sd)
n <-1
roll_ub <- roll_me + n*roll_std
roll_lb <- roll_me - n*roll_std

signal <- NULL
signal <- ifelse(hedge_ratio > roll_ub, -1, ifelse(hedge_ratio < roll_lb, 1, 0))
lagsignal <- Lag(signal, 1)
signal <- ifelse(lagsignal == -1 & hedge_ratio > roll_me,-1,ifelse(lagsignal ==1 & hedge_ratio < roll_me, 1,0))

spread_return <- kospietf_r - chinaetf_r
trade_return <- spread_return * lag(signal) - cost

### 5-4 공적분 기반 페어 트레이딩 ###
adf.test(kospietf_p)

diff <- kospietf_p - Lag(kospietf_p, 1) #1차차분 
adf.test(diff[!is.na(diff)])

model <- lm(kospietf_p ~ chinaetf_p +0)
model
summary(model)
adf.test(as.ts(model$residuals))

plot(kospietf_p, type = "l", main = "KODEX KOSPI ETF & CHINA ETF")
par(mfrow = c(2,1))
lines(chinaetf_p * model$coefficients, col = "red")
plot(as.xts(model$residuals), type = "l")

roll_me <- rollapply(model$residuals, 14, mean)
roll_std <- rollapply(model$residuals, 14, sd)
n <- 1
roll_ub <- roll_me + n * roll_std
roll_lb <- roll_me - n * roll_std
signal <- NULL
signal <- ifelse(model$residuals > roll_ub, -1, ifelse(model$residuals < roll_lb, 1, 0))
lagsignal <- Lag(signal, 1)
signal <- ifelse(lagsignal ==-1 & model$residuals > roll_me, -1, ifelse(lagsignal ==1 & model$residuals < roll_me, 1, 0))

#### 5-4 CAPM ####
pfdata <- cbind(kospietf_r, chinaetf_r)

rf <- rep(0, dim(pfdata)[1])
model <- lm((pfdata[,2] -rf ) ~ (pfdata[,1]- rf))
model

CAPM.beta(pfdata[,2], pfdata[,1])
CAPM.alpha(pfdata[,2], pfdata[,1])
plot(as.ts(chinaetf_r), as.ts(kospietf_r), xlim = c(0,0.04), ylim = c(0,0.04),xlab = "CHINA ETF Return", ylab="KOSPI ETF Return")
abline(model, col = "red")

#### 5-5 Multi Factor Model -> do not Run ####

con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

dow.jones.components <- function(){
  url = 'http://money.cnn.com/data/dow30/'
  txt = join(readLines(url))
  temp = gsub(pattern = '">', replacement = '<td>', txt, perl =TRUE)
  temp = gsub(pattern = '</a>', replacement = '</td>', temp, perl = TRUE)
  temp = extract.table.from.webpage(temp, 'Volume', has.header=T)
  trim(temp[,'Company'])
}
tickers=dow.jones.components()

#ticker 크로울링 
data.fund <- new.env()
temp = paste(iif(nchar(tickers) <=3, 'NYSE:', 'NASDAQ:'), tickers, sep = '')
for(i in 1:len(tickers)) data.fund[[tickers[i]]] = fund.data(temp[i], 80)
save(data.fund, file = 'data.fund.Rdata')
#가격 크로울링
data <- new.env()
getSymbols(tickers, src = 'yahoo', from = '1970-01-01', env = data, auto.assign = T)
for(i in ls(data)) data[[i]] = adjustOHLC(data[[i]], use.Adusted=T)
save(data,file='data.Rdata')
#날짜 변수
date.fund.data <- function(data){
  quarter.end.date = as.Date(paste(data['quarter end date',],'/1',sep=''),'%Y/%m/%d')
  quarterly.indicator = data['quarterly indicator',]
  date.preliminary.data.loaded = as.Date(data['date preliminary data loaded',],],'%Y-%m-%d')+1
  months = seq(quarter.end.date[1], tail(quarter.end.date, 1)+365, by = '1 month')
  index = match(quarter.end.date, months)
  quarter.end.date = months[iif(quarterly.indicator =='4', index +3, index+2)+1]-1
  fund.date = date.preliminary.data.loaded
  fund.date[is.na(fund.date)] = quarter.end.date[is.na(fund.date)]
  return(fund.date)
}

## fundamental analysis ##

for(i in tickers){
  fund = data.fund[[i]]
  fund.date = date.fund.data(fund)
  EPS = get.fund.data('Diluted EPS from Total Operations', fund, fund.date, is.12m.rolling= T)
  CSHO = get.fund.data('total common shares out', fund, fund.date) # common shares outstanding
  CEQ = get.fund.data('total equity', fund, fund.date) #common equity
  data[[i]] = merge(data[[i]], EPS, CSHO, CEQ)
}
#
bt.prep(data, align = 'keep.all', dates='1995::2011')
prices= data$prices
prices = bt.apply.matrix(prices, function(x) ifna.prev(x))
#
factors = list()
#재무비율
factors$TV = list()
#시가총액
CSHO = bt.apply(data, function(x), ifna.prev(x[,'CSHO']))
MKVAL = prices * CSHO
# EPS
EPS = bt.apply(data, function(x) ifna.prev(x[,'EPS']))
factors$TV$EP = EPS / prices
# 자기자본대 주가비율
CEQ = bt.apply(data, function(x) ifna.prev(x[,'CEQ']))
factors$TV$BP = CEQ / MKVAL

#횡단면 Z점수로 변환
for(i in names(factors$TV)){
  factors$TV[[i]] = (factors$TV[[i]] - cap.wewighted.mean(factors$TV[[i]], MKVAL))/apply(factors$TV[[i]], 1, sd, na.rm =T)
}

load.packages("abind")
temp  = abind(factors$TV, along = 3)

factors$TV$AVG = factors$TV[[1]]
factors$TV$AVG[] = apply(temp, c(1,2), mean, na.rm= T)

#월말 찾기
month.ends = endpoints(prices, 'months')
prices = prices[month.ends, ]
n = ncol (prices)
nperiods = nrow(prices)

ret = prices / mlag(prices) -1
next.month.ret = mlag(ret,-1)
MKVAL = MKVAL[month.ends,]
for(j in 1:len(factors)){
  for(i in 1:len(factors[[j]])){
    factors[[j]][[i]] = factors[[j]][[i]][month.ends,]
  }
}

out = compute.quantiles(factors$TV$AVG, next.month.ret, plot = F)
models=list()
for(i in 1:5){
  data$weight[] = NA
  data$weight[month.ends,] = iif(out$qunatiles ==i, out$weights,0)
  capital = 100000
  data$weight[] = (capital/prices) * (data$weight)
  models[[paste('Q',i,sep='')]] = bt.run(data,type='share', captial = capital)
}

#스프레드 
data$weight[] = NA
data$weight[month.ends,] = iif(out$qunatiles ==5, out$weights, iif(out$qunatiles==1, -out$weights, 0))
capital = 10000
data$weight[]=(capital/prices)*(data$weight)
models$Q5_Q1 = bt.run(data, type = 'share', capital = capital)

factors.avg = list()
for(j in names(factors)) factors.avg[[j]] = factors[[j]]$AVG
factors.avg = add.avg.factor(factors.avg)
nperiods = nrow(nex.month.ret)
n = ncol(next.month.ret)

#각 요인에 대한 행렬 생성
factors.matrix = abind(factors.avg, along = 3)
all.data = factors.matrix

#베타
beta = all.data[,1,] * NA

# all.data에 next.month.ret추가
all.data = abind(next.month.ret, all.data, along = 3)
dimnames(all.data)[[3]][1] = 'Ret'
all.data[is.na(all.data)] <- 0
#베타예측(요인수익률)
for(t in 30:(nperiods-1)){
  temp = all.data[t:t,,]
  x = temp[,-1]
  y = temp[,1]
  beta[(t+1),]  = lm(y~x-1)$coefficients
}

#알파수익률 예측 생성
alpha = next.month.ret * NA
for(t in 40:(nperiods-1)){
  coef = colMeans(beta[(t-5):t,], na.rm=T)
  alpha[t,] = rowSums(all.data[t,,-1] * t(repmat(coef, 1, n)), na.rm = T)
}

#### 5-5 Portfolio Theory ####

stockData <- new.env()
symbols <- c("278530.KS","069500.KS","114800.KS","122630.KS")
start_date <- as.Date("2018-01-01")
getSymbols(symbols, src= "yahoo", env = stockData, from =start_date)
x<-list()
#
for (i in 1:length(symbols)){
  x[[i]] <- get(symbols[i], pos = stockData)
  x[[i]]$gl <- ((Cl(x[[i]]) - Op(x[[i]])) / Op(x[[i]]))*100
  if(i==1)
    data<-Cl(x[[i]])
  else
    data <- cbind(data, Cl(x[[i]]))
}
#
data_ret <- apply(data,2,Delt)
data_ret <- na.omit(data_ret)
napos <- which(apply(data_ret,2,is.na))
avg_ret <- apply(data_ret,2,mean)
covariance_mat <- cov(data_ret, use = 'na')
weights <- c(0.2, 0.3,0.35,0.15)
#
source("C:/Users/Shinhyunjin/Dropbox/data/portfolio.R")
# Normal
weightedport = getPortfolio(er=avg_ret, cov.mat = covariance_mat, weights =weights)
weightedport
# MVP
minvar_port <- globalMin.portfolio(avg_ret, covariance_mat)
minvar_port
# Efficient
rf <-0
efficient_port <- efficient.portfolio(avg_ret, covariance_mat, rf)
efficient_port
# Tangent
tangency_port <- tangency.portfolio(avg_ret, covariance_mat, rf)
tangency_port

## EF
efficient_frontier <- efficient.frontier(avg_ret, covariance_mat, alpha.min =-2,alpha.max=2, nport=50)
plot(efficient_frontier, plot.assets =T)
points(minvar_port$sd, minvar_port$er, col = "blue")
points(tangency_port$sd, tangency_port$er, col = "red")
tangent_sharpe_ratio = (tangency_port$er - rf) / tangency_port$sd
abline(a=rf, b=tangent_sharpe_ratio)