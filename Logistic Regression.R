###Logistic Regression ###

rm(list=ls())

library(TTR)
library(xts)
library(quantmod)
library(caret)
library(nnet)
library(caret)
library(PerformanceAnalytics)
library(deepnet)
library(h2o)
library(caret)


### Data ###
getSymbols("GOLD.AX", src = "yahoo")
GAX <- GOLD.AX
GAX <- GAX[,"GOLD.AX.Close"]
GAX <- na.omit(GAX)

#Varaible #
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
osed <- "2018-11-20" 

isrow <- which(index(GAX) >= issd & index(GAX) <= ised)
osrow <- which(index(GAX) >= ossd & index(GAX) <= osed)

isGAX <- GAX[isrow,]
osGAX <- GAX[osrow,]


#표준화#
isme <- apply(isGAX, 2, mean)
isstd <- apply(isGAX, 2, sd)
isidn <- matrix(1, dim(isGAX)[1], dim(isGAX)[2])

norm_isGAX <- (isGAX - t(isme * t(isGAX))) / t(isstd * t(isidn))

dm<-dim(isGAX)
norm_isGAX[,dm[2]] <- direction[isrow]

formula <- paste("Direction ~ .", sep ="")
model <-glm(formula, family = "binomial", norm_isGAX)
summary(model)

pred <- predict(model, norm_isGAX)

prob <- 1/(1+exp(-(pred)))


par(mflow = c(2,1))
plot(pred,type = "l")
plot(prob, type = "l")

head(prob)
tail(prob)

pred_direction <- NULL
pred_direction[prob > 0.5] <- 1
pred_direction[prob <= 0.5] <- 0

pred_direction


#오차행렬에 집어넣을 때, pred_direction이랑 norm_isdji 간 객체 차 존재. Table로 집어넣어야 함, e1071패키지 설치 후 실행
matrix <- confusionMatrix(table(pred_direction,norm_isGAX$Direction))
matrix
#예측 정확도 94%

#정규화
osidn <- matrix(1,dim(osGAX)[1], dim(osGAX)[2])
norm_osGAX <- (osGAX - t(isme*t(osidn))) / t(isstd*t(osidn))
dm <- dim(osGAX)
norm_osGAX[,dm[2]] <- direction[osrow]

#표본 외 데이터 값, 확률
ospred <- predict(model, norm_osGAX)
osprob <- 1/(1+exp(-(ospred)))

ospred_direction <- NULL
ospred_direction[osprob >0.5]<-1
ospred_direction[osprob<=0.5]<-0
osmatrix <- confusionMatrix(table(ospred_direction, norm_osGAX$Direction))
osmatrix
#84% accuracy, 트레이딩 비용, 시장 슬리피지 고려X, 다른 전략 X 오로지 예측


#### 트레이딩 후 Cummulative return 구하는 코딩

#로지스틱 CUM RET 구하는 법

signal <- ifelse(ospred_direction == 1, 1, ifelse(ospred_direction == 0,-1,0))
testrow <- which(index(GAX) >= ossd & index(GAX) <= osed)
GAX <- GOLD.AX$GOLD.AX.Close
ret <- GAX/lag(GAX) -1
ret <- ret[testrow]
ret
cost <- 0
length(signal)
length(ret)
dim(ret)
length(testrow)

trade_ret <- ret * Lag(signal) - cost
dim(ret)
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)