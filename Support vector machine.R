#SUpport Vector machine

rm(list=ls())

library(quantmod)
library(class)
library(e1071)
library(PerformanceAnalytics)


getSymbols("GOLD.AX", src = "yahoo")
GAX <- GOLD.AX
GAX <- GAX[,"GOLD.AX.Close"]
GAX <- na.omit(GAX)

avg10 <- rollapply(GAX, 10, mean)
avg20 <- rollapply(GAX, 20, mean)
std10 <- rollapply(GAX,10,sd)
std20 <- rollapply(GAX,20,sd)
rsi5 <- RSI(GAX, 5, "SMA")
rsi14 <- RSI(GAX, 14, "SMA")
macd12269 <- MACD(GAX, 12, 26, 9, "SMA")
macd7205 <- MACD(GAX, 7, 20, 5, "SMA")
bbands <- BBands(GAX, 20, "SMA", 2)

direction <- NULL
direction[GAX > Lag(GAX, 20)] <- 1
direction[GAX < Lag(GAX, 20)] <- 0

GAX <- cbind(GAX, avg10, avg20, std10, std20, rsi5, rsi14, macd12269, macd7205, bbands, direction)

dm <- dim(GAX)
dm
colnames(GAX)[dm[2]]
colnames(GAX)[dm[2]] <- "Direction"
colnames(GAX)
issd <- "2013-01-01"
ised <- "2017-12-31"
ossd <- "2018-01-01"
osed <- "2018-12-31" 

isrow <- which(index(GAX) >= issd & index(GAX) <= ised)
osrow <- which(index(GAX) >= ossd & index(GAX) <= osed)

isGAX <- GAX[isrow,]
osGAX <- GAX[osrow,]

#표준화
isme <- apply(isGAX, 2, mean)
isstd <- apply(isGAX, 2, sd)

isidn <- matrix(1, dim(isGAX)[1], dim(isGAX)[2])
norm_isGAX <- (isGAX - t(isme * t(isidn))) / t(isstd * t(isidn))

norm_isGAX
dm<-dim(isGAX)
norm_isGAX[,dm[2]] <- direction[isrow]

#정규화
osidn <- matrix(1,dim(osGAX)[1], dim(osGAX)[2])
norm_osGAX <- (osGAX - t(isme*t(osidn))) / t(isstd*t(osidn))
dm <- dim(osGAX)
norm_osGAX[,dm[2]] <- direction[osrow]

#방향설정
GAX <- GOLD.AX
GAX <- GAX[,"GOLD.AX.Close"] #다시 설정 
GAX <- na.omit(GAX)
lagret <- (GAX - Lag(GAX,20))/Lag(GAX,20)
direction[lagret >0.02]<-"Up"
direction[lagret < -0.02]<-"Down"
direction[lagret < 0.02 & lagret > -0.02]<-"Nowhere"
isdir <- direction[isrow]
osdir <- direction[osrow]

model <- svm(norm_isGAX, as.factor(isdir))
model

pred <- predict(model, norm_osGAX)
head(pred)
table(pred,osdir)

model
#적중률
sum(diag(table(pred,osdir)))/sum(table(pred,osdir))

####서포트벡터 트레이딩 후 Cummulative return 구하는 코딩

signal <- ifelse(pred == "Up", 1, ifelse(pred == "Down",-1,0))
#테스트할 날짜 데이터 집합
testrow <- which(index(GAX) >= ossd & index(GAX) <= osed)
testrow
#리턴 구하는 공식과 그 공식 활용 테스트 날짜별 리턴
ret <- GAX/lag(GAX) -1
ret <- ret[testrow]
ret
cost <- 0

trade_ret <- ret * Lag(signal) - cost
length(signal)
dim(ret)
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
