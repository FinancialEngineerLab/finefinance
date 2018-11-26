### K-clustering ###
rm(list=ls())

# library
library(quantmod)
library(clue)
library(class)
library(caret)
library(FNN)


#Data
getSymbols("GOLD.AX", src = "yahoo")
GAX <- GOLD.AX
GAX <- GAX[,"GOLD.AX.Close"]
GAX <- na.omit(GAX)
#variable 
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


dm<-dim(isGAX)
norm_isGAX[,dm[2]] <- direction[isrow]

osidn <- matrix(1,dim(osGAX)[1], dim(osGAX)[2])
norm_osGAX <- (osGAX - t(isme*t(osidn))) / t(isstd*t(osidn))
dm <- dim(osGAX)
norm_osGAX[,dm[2]] <- direction[osrow]

clusters <- 3
set.seed(1)

#디렉션 제거
dm<- dim(isGAX)
isGAX[,-dm[2]]
isGAX <- isGAX[,-dm[2]]
norm_isGAX <- norm_isGAX[,-dm[2]]
dm<-dim(osGAX)
osGAX<-osGAX[,-dm[2]]
norm_osGAX<-norm_osGAX[,-dm[2]]

#isdji 바탕으로 클러스터화 모델
model <- kmeans(norm_isGAX, clusters)

head(model$cluster)
model$cluster
model$center

model$size

#클러스터 내제곱합과 총제곱합 비율 최소화
model$tot.withinss
model$totss
model$tot.withinss/model$totss

ospredict <- cl_predict(model, norm_osGAX)
norm_osGAX
head(ospredict)
ospredict

GAX <- GOLD.AX
GAX <- GAX[,"GOLD.AX.Close"]
GAX <- na.omit(GAX)
lagret <- (GAX - Lag(GAX,20))/Lag(GAX,20)
direction[lagret >0.02]<-"Up"
direction[lagret < -0.02]<-"Down"
direction[lagret < 0.02 & lagret > -0.02]<-"Nowhere"
isdir <- direction[isrow]
osdir <- direction[osrow]

neighborhood <- 3
set.seed(1)
dim(norm_isGAX)
dim(norm_osGAX)
model <- knn(norm_isGAX, norm_osGAX, isdir, neighborhood)
model
head(model)

summary(model)

matrix <- confusionMatrix(table(model, osdir))
matrix

diag(matrix$table)
#for 사용해 confusionmatrix의 행렬 계산, 총 대각선 요소-총대각선 요소 수/총데이터요소수
accuracy<- NULL

for(i in c(1:100)){
  model <- knn(isGAX, osGAX, isdir, i)
  matrix <- confusionMatrix(table(model,osdir))
  diag <- sum(diag(matrix$table))
  total <- sum(matrix$table)
  accuracy[i] <- diag/total
}
accuracy
plot(accuracy ,type = "l")

### ##K Cluster 모형 CumRet 구하는법
GAX <- GOLD.AX
GAX <- GOLD.AX[,"GOLD.AX.Close"]
GAX <- na.omit(GAX)


signal <- ifelse( model== "Up", 1, ifelse( model== "Down",-1,0))
testrow <- which(index(GAX) >= ossd & index(GAX) <= osed)
ret <- GAX/lag(GAX,1) -1
ret <- ret[testrow]
ret
cost <- 0
length(signal)
length(ret)
length(testrow)

trade_ret <- ret * Lag(signal) - cost
signal
length(signal)
dim(ret)
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
