### Neural Network Machine Learning ###
rm(list=ls())

library(xts)
library(quantmod)
library(caret)
library(nnet)
library(caret)
library(PerformanceAnalytics)
library(deepnet)
library(h2o)
library(caret)

getSymbols("GOLD.AX", src = "yahoo")
GAX <- GOLD.AX[,"GOLD.AX.Close"]
GAX <- na.omit(GAX)

chartSeries(ClCl(GAX))
plot(GAX)

#수익률 계산, 델타 함수가 안되어요 ㅜㅜ
ret <- GAX/lag(GAX) -1 # 중요함 

avg10 <- rollapply(GAX,10,mean)
avg20 <- rollapply(GAX,20,mean)
std10<-rollapply(GAX,10,sd)
std20<-rollapply(GAX,20,sd)
rsi5 <- RSI(GAX,5,"SMA")
rsi14<-RSI(GAX,14,"SMA")
macd12269 <- MACD(GAX,12,26,9,"SMA")
macd7205 <- MACD(GAX,7,20,5,"SMA")
bbands<-BBands(GAX,20,"SMA",2)

#지난 20일간 수익률 2% 이상이면 up, -2% down, 그사이 nowhere

direction <- data.frame(matrix(NA,dim(GAX)[1],1))

#20일 수익률
lagret <- (GAX - Lag(GAX,20)) / Lag(GAX,20)
direction[lagret > 0.02] <- "Up"
direction[lagret < -0.02] <- "Down"
direction[lagret < 0.02 & lagret > -0.02] <- "Nowhere"

GAX <- cbind(GAX,avg10,avg20, std10, std20,rsi5,rsi14,macd12269,macd7205,bbands)

#훈련용, 검증용, 평가용 데이터집합
train_sdate <- "2013-01-01"
train_edate <- "2016-12-31"
vali_sdate <- "2017-01-01"
vali_edate <- "2017-12-31"
test_sdate <- "2018-01-01"
test_edate <- "2018-12-31"

trainrow <- which(index(GAX) >= train_sdate & index(GAX) <= train_edate)
valirow <- which(index(GAX) >= vali_sdate & index(GAX) <= vali_edate)
testrow <- which(index(GAX) >= test_sdate & index(GAX) <= test_edate)
trainGAX <- GAX[trainrow,]
valiGAX <- GAX[valirow,]
testGAX <- GAX[testrow,]

trainme <- apply(trainGAX,2,mean)
trainstd <- apply(trainGAX,2,sd)

#정규화
trainidn <- (matrix(1,dim(trainGAX)[1],dim(trainGAX)[2]))
valiidn <- (matrix(1,dim(valiGAX)[1],dim(valiGAX)[2]))
testidn <- (matrix(1,dim(testGAX)[1],dim(testGAX)[2]))
norm_trainGAX <- (trainGAX - t(trainme*t(trainidn)))/t(trainstd*t(trainidn))
norm_valiGAX <- (valiGAX - t(trainme*t(valiidn)))/t(trainstd*t(valiidn))
norm_testGAX <- (testGAX - t(trainme*t(testidn)))/t(trainstd*t(testidn))

traindir <- direction[trainrow,1]
validir <- direction[valirow,1]
testdir<-direction[testrow,1]



#신경망 적합(정규화열,날짜별 방향, 신경수, 트레이스 출력 여부)
set.seed(1)
model <- nnet(norm_trainGAX, class.ind(traindir),size = 4, trace = F)
model

dim(norm_trainGAX)

vali_pred <- predict(model, norm_valiGAX)
head(vali_pred)

vali_pred_class <- data.frame(matrix(NA,dim(vali_pred)[1],1))
vali_pred_class[vali_pred[,"Down"]>0.5, 1]<- "Down"
vali_pred_class[vali_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
vali_pred_class[vali_pred[,"Up"]>0.5, 1] <- "Up"

vali_pred_class
matrix<- confusionMatrix(table(vali_pred_class[,1],validir))
matrix                         
#87.65% 

test_pred <- predict(model,norm_testGAX)
test_pred_class <- data.frame(matrix(NA,dim(test_pred)[1],1))
test_pred_class[test_pred[,"Down"]>0.5, 1]<- "Down"
test_pred_class[test_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
test_pred_class[test_pred[,"Up"]>0.5, 1] <- "Up"

test_matrix <- confusionMatrix(table(test_pred_class[,1],testdir))
test_matrix
#82pro

#Signal generator
signal <- ifelse(test_pred_class == "Up", 1, ifelse(test_pred_class == "Down",-1,0))
ret <- ret[testrow]
ret
cost <- 0
trade_ret <- ret * Lag(signal) - cost

cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
#charts.PerformanceSummary(cumm_ret)
#plot(cumm_ret)

#Deep Neural Network

set.seed(1)
model <- dbn.dnn.train(norm_testGAX,class.ind(traindir),hidden=c(3,4,6))
nn.predict(model,norm_valiGAX)

nn.test(model,norm_valiGAX,class.ind(validir),t=0.4)

data <- cbind(as.data.frame(norm_trainGAX),traindir)
class(norm_trainGAX)
class(traindir)
h2o.init()
datah2o <- as.h2o(data,"h2o")
class(datah2o)

dim(datah2o)

#은닉층이 4개, 각 뉴런수 4,5,2,7
model <- h2o.deeplearning(1:15, 16,training_frame = datah2o,hidden = c(4,5,2,7))
vali_pred <- predict(model, as.h2o(norm_valiGAX,"h2o"))
vali_pred <- as.data.frame(vali_pred)
vali_pred_class <- data.frame(matrix(NA,dim(vali_pred)[1],1))
vali_pred_class[vali_pred[,"Down"]>0.5, 1]<- "Down"
vali_pred_class[vali_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
vali_pred_class[vali_pred[,"Up"]>0.5, 1] <- "Up"

vali_matrix <- confusionMatrix(table(vali_pred_class[,1],validir))
vali_matrix


## Deep Graph ##
signal <- ifelse(vali_pred_class == "Up", 1, ifelse(vali_pred_class == "Down",-1,0))
ret <- GAX/lag(GAX) -1 # 중요함 
ret <- ret[valirow]
ret <- ret[,1]
cost <- 0
trade_ret <- ret * Lag(signal) - cost

cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
