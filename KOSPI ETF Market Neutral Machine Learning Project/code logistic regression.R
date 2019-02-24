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

### Data ###

getSymbols("102110.KS", to ="2019-02-22")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
#Varaible #
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
ossd <- as.Date(osed)-365
ised <- ossd -1
issd <- ised - 1825




isrow <- which(index(ETFKOSPI) >= issd & index(ETFKOSPI) <= ised)
osrow <- which(index(ETFKOSPI) >= ossd & index(ETFKOSPI) <= osed)

isETFKOSPI <- ETFKOSPI[isrow,]
osETFKOSPI <- ETFKOSPI[osrow,]


# 표준화작업 #
isme <- apply(isETFKOSPI, 2, mean)
isstd <- apply(isETFKOSPI, 2, sd)
isidn <- matrix(1, dim(isETFKOSPI)[1], dim(isETFKOSPI)[2]) #표본내 동일한 차원의 단위행렬
#표준화
norm_isETFKOSPI <- (isETFKOSPI - t(isme * t(isETFKOSPI))) / t(isstd * t(isidn)) 
dm<-dim(isETFKOSPI)
norm_isETFKOSPI[,dm[2]] <- direction[isrow]

#일반화 선형모델 함수 
formula <- paste("Direction ~ .", sep ="")
model <-glm(formula, family = "binomial", norm_isETFKOSPI)
summary(model)

pred <- predict(model, norm_isETFKOSPI) #적합값
prob <- 1/(1+exp(-(pred))) #적합값의 확률ㅎ

#표현 
#par(mfrow = c(2,1))
plot(pred,type = "l")
plot(prob, type = "l")

head(prob)
tail(prob)

#방향성 반영 
pred_direction <- NULL
pred_direction[prob > 0.5] <- 1
pred_direction[prob <= 0.5] <- 0

pred_direction


#오차행렬에 집어넣을 때, pred_direction이랑 norm_isdji 간 객체 차 존재. Table로 집어넣어야 함, e1071패키지 설치 후 실행
matrix <- confusionMatrix(table(pred_direction,norm_isETFKOSPI$Direction))
matrix
#예측 정확도 94%

#정규화
osidn <- matrix(1,dim(osETFKOSPI)[1], dim(osETFKOSPI)[2])
norm_osETFKOSPI <- (osETFKOSPI - t(isme*t(osidn))) / t(isstd*t(osidn))
dm <- dim(osETFKOSPI)
norm_osETFKOSPI[,dm[2]] <- direction[osrow]

#표본 외 데이터 값, 확률
ospred <- predict(model, norm_osETFKOSPI)
osprob <- 1/(1+exp(-(ospred)))

#최종 Confusion Matrix
ospred_direction <- NULL
ospred_direction[osprob >0.5]<-1
ospred_direction[osprob<=0.5]<-0
osmatrix <- confusionMatrix(table(ospred_direction, norm_osETFKOSPI$Direction))
osmatrix
#84% accuracy, 트레이딩 비용, 시장 슬리피지 고려X, 다른 전략 X 오로지 예측


### 트레이딩 반영한  Cummulative return ###

#로지스틱 CUM RET 구하는 법

signal <- ifelse(ospred_direction == 1, 1, ifelse(ospred_direction == 0,-1,0))
testrow <- which(index(ETFKOSPI) >= ossd & index(ETFKOSPI) <= osed)
#
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

cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)
cumm_ret

par(mfrow = c(1,1))
charts.PerformanceSummary(trade_ret)

