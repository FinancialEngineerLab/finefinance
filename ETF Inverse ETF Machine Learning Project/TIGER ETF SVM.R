#SUpport Vector machine

rm(list=ls())

library(quantmod)
library(class)
library(e1071)
library(PerformanceAnalytics)

library(xts)
library(quantmod)
library(caret)
library(nnet)
library(caret)
library(PerformanceAnalytics)
library(deepnet)
library(h2o)
library(caret)

library(TTR)
library(xts)
library(quantmod)
library(caret)
library(nnet)
library(caret)
library(PerformanceAnalytics)
library(deepnet)
library(h2o)

getSymbols("102110.KS") #
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)

avg10 <- rollapply(ETFKOSPI, 10, mean)
avg20 <- rollapply(ETFKOSPI, 20, mean)
std10 <- rollapply(ETFKOSPI,10,sd)
std20 <- rollapply(ETFKOSPI,20,sd)
rsi5 <- RSI(ETFKOSPI, 5, "SMA")
rsi14 <- RSI(ETFKOSPI, 14, "SMA")
macd12269 <- MACD(ETFKOSPI, 12, 26, 9, "SMA")
macd7205 <- MACD(ETFKOSPI, 7, 20, 5, "SMA")
bbands <- BBands(ETFKOSPI, 20, "SMA", 2)

direction <- NULL
direction[ETFKOSPI > Lag(ETFKOSPI, 20)] <- 1
direction[ETFKOSPI < Lag(ETFKOSPI, 20)] <- 0
ETFKOSPI <- cbind(ETFKOSPI, avg10, avg20, std10, std20, rsi5, rsi14, macd12269, macd7205, bbands, direction)

dm <- dim(ETFKOSPI)
dm
colnames(ETFKOSPI)[dm[2]]
colnames(ETFKOSPI)[dm[2]] <- "Direction"
colnames(ETFKOSPI)

osed <- "2019-02-22"
ossd <- as.Date(osed)- 365
ised <- ossd - 1
issd <- ised - 1825

isrow <- which(index(ETFKOSPI) >= issd & index(ETFKOSPI) <= ised)
osrow <- which(index(ETFKOSPI) >= ossd & index(ETFKOSPI) <= osed)

isETFKOSPI <- ETFKOSPI[isrow,]
osETFKOSPI <- ETFKOSPI[osrow,]

#Train Normalization
isme <- apply(isETFKOSPI, 2, mean)
isstd <- apply(isETFKOSPI, 2, sd)

isidn <- matrix(1, dim(isETFKOSPI)[1], dim(isETFKOSPI)[2])
norm_isETFKOSPI <- (isETFKOSPI - t(isme * t(isidn))) / t(isstd * t(isidn))

dm<-dim(isETFKOSPI)
norm_isETFKOSPI[,dm[2]] <- direction[isrow]
norm_isETFKOSPI <- na.omit(norm_isETFKOSPI)
#Test Normalization

osidn <- matrix(1,dim(osETFKOSPI)[1], dim(osETFKOSPI)[2])
norm_osETFKOSPI <- (osETFKOSPI - t(isme*t(osidn))) / t(isstd*t(osidn))
dm <- dim(osETFKOSPI)
norm_osETFKOSPI[,dm[2]] <- direction[osrow]
norm_osETFKOSPI <- na.omit(norm_osETFKOSPI)


#방향설정

lagret <- (ETFKOSPI[,1] - Lag(ETFKOSPI[,1],20))/Lag(ETFKOSPI[,1],20)
ETFKOSPI[,16][lagret>0.02]<-"up"
ETFKOSPI[,16][lagret >0.02]<-"Up"
ETFKOSPI[,16][lagret < -0.02]<-"Down"
ETFKOSPI[,16][lagret < 0.02 & lagret > -0.02]<-"Nowhere"

isdir <- ETFKOSPI[,16][index(norm_isETFKOSPI)]
osdir <- ETFKOSPI[,16][index(norm_osETFKOSPI)]
dim(norm_isETFKOSPI)
length(isdir)

model <- svm(norm_isETFKOSPI, as.factor(isdir))
model

pred <- predict(model, norm_osETFKOSPI)
head(pred)
table(pred,osdir)
confusionMatrix(table(pred,osdir))
model

#적중률
sum(diag(table(pred,osdir)))/sum(table(pred,osdir))

####서포트벡터 트레이딩 후 Cummulative return 구하는 코딩

signal <- ifelse(pred == "Up", 1, ifelse(pred == "Down",-1,0))
#테스트할 날짜 데이터 집합
testrow <- which(index(ETFKOSPI) >= ossd & index(ETFKOSPI) <= osed)
testrow
#리턴 구하는 공식과 그 공식 활용 테스트 날짜별 리턴
getSymbols("102110.KS")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
getSymbols("123310.KS")
inverse <-`123310.KS`
inverse <- inverse[,"123310.KS.Close"]
inverse <- na.omit(inverse)
ret <- ETFKOSPI/lag(ETFKOSPI) -1
ret <- ret[osrow]
ret
inv <- inverse/lag(inverse)-1
inv <- inv[index(ret)]
cost <- 0
length(signal)
dim(ret)

if (signal ==1){
  trade_ret = ret*signal - cost
}
if (signal ==-1){
  trade_ret = inv*(-signal) - cost
}
trade_ret <- ret * signal - cost
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)

