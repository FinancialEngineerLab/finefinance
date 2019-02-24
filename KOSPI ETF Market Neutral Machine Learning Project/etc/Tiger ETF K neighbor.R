### K-clustering ###
rm(list=ls())

# library
library(quantmod)
library(clue)
library(class)
library(caret)
library(FNN)
library(xts)
library(quantmod)
library(caret)
library(nnet)
library(caret)
library(PerformanceAnalytics)
library(deepnet)
library(h2o)
library(caret)
library(RQuantLib)


#Data
getSymbols("102110.KS")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
#variable 
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


osed <- Sys.Date()
ossd <- Sys.Date()-365
ised <- ossd -1
issd <- ised - 1825



isrow <- which(index(ETFKOSPI) >= issd & index(ETFKOSPI) <= ised)
osrow <- which(index(ETFKOSPI) >= ossd & index(ETFKOSPI) <= osed)

isETFKOSPI <- ETFKOSPI[isrow,]
osETFKOSPI <- ETFKOSPI[osrow,]

#표준화
isme <- apply(isETFKOSPI, 2, mean)
isstd <- apply(isETFKOSPI, 2, sd)

isidn <- matrix(1, dim(isETFKOSPI)[1], dim(isETFKOSPI)[2])
norm_isETFKOSPI <- (isETFKOSPI - t(isme * t(isidn))) / t(isstd * t(isidn))


dm<-dim(isETFKOSPI)
norm_isETFKOSPI[,dm[2]] <- direction[isrow]

osidn <- matrix(1,dim(osETFKOSPI)[1], dim(osETFKOSPI)[2])
norm_osETFKOSPI <- (osETFKOSPI - t(isme*t(osidn))) / t(isstd*t(osidn))
dm <- dim(osETFKOSPI)
norm_osETFKOSPI[,dm[2]] <- direction[osrow]

clusters <- 3
set.seed(1)

#디렉션 제거
dm<- dim(isETFKOSPI)
isETFKOSPI[,-dm[2]]
isETFKOSPI <- isETFKOSPI[,-dm[2]]
norm_isETFKOSPI <- norm_isETFKOSPI[,-dm[2]]
dm<-dim(osETFKOSPI)
osETFKOSPI<-osETFKOSPI[,-dm[2]]
norm_osETFKOSPI<-norm_osETFKOSPI[,-dm[2]]

#isdji 바탕으로 클러스터화 모델
model <- kmeans(norm_isETFKOSPI, clusters)

head(model$cluster)
model$cluster
model$center

model$size

#클러스터 내제곱합과 총제곱합 비율 최소화
model$tot.withinss
model$totss
model$tot.withinss/model$totss

ospredict <- cl_predict(model, norm_osETFKOSPI)
norm_osETFKOSPI
head(ospredict)
ospredict

getSymbols("102110.KS")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
lagret <- (ETFKOSPI - Lag(ETFKOSPI,20))/Lag(ETFKOSPI,20)
direction[lagret >0.02]<-"Up"
direction[lagret < -0.02]<-"Down"
direction[lagret < 0.02 & lagret > -0.02]<-"Nowhere"
isdir <- direction[isrow]
osdir <- direction[osrow]

neighborhood <- 24
set.seed(1)
dim(norm_isETFKOSPI)
dim(norm_osETFKOSPI)
model <- knn(norm_isETFKOSPI, norm_osETFKOSPI, isdir, neighborhood)
model
head(model)

summary(model)

matrix <- confusionMatrix(table(model, osdir))
matrix

diag(matrix$table)
#for 사용해 confusionmatrix의 행렬 계산, 총 대각선 요소-총대각선 요소 수/총데이터요소수
accuracy<- NULL

for(i in c(1:100)){
  model <- knn(isETFKOSPI, osETFKOSPI, isdir, i)
  matrix <- confusionMatrix(table(model,osdir))
  diag <- sum(diag(matrix$table))
  total <- sum(matrix$table)
  accuracy[i] <- diag/total
}
accuracy
plot(accuracy ,type = "l")

### ##K Cluster 모형 CumRet 구하는법

signal <- ifelse( model== "Up", 1, ifelse( model== "Down",-1,0))
testrow <- which(index(ETFKOSPI) >= ossd & index(ETFKOSPI) <= osed)
getSymbols("102110.KS")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
getSymbols("123310.KS")
inverse <-`123310.KS`
inverse <- inverse[,"123310.KS.Close"]
inverse <- na.omit(inverse)
ret <- ETFKOSPI/lag(ETFKOSPI) -1
ret <- ret[index(osETFKOSPI)]
ret
inv <- inverse/lag(inverse)-1
inv <- inv[index(ret)]
cost <- 0
length(signal)
dim(ret)

if (signal ==1|signal==0){
  trade_ret = ret*signal - cost
}
if (signal ==-1){
  trade_ret = inv*(-signal) - cost
}

cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
