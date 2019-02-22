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

getSymbols("102110.KS") #
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)

#수익률 계산, 델타 함수가 안되어요 ㅜㅜ
ret <- ETFKOSPI/lag(ETFKOSPI) -1 # 중요함 

avg10 <- rollapply(ETFKOSPI,10,mean)
avg20 <- rollapply(ETFKOSPI,20,mean)
std10<-rollapply(ETFKOSPI,10,sd)
std20<-rollapply(ETFKOSPI,20,sd)
rsi5 <- RSI(ETFKOSPI,5,"SMA")
rsi14<-RSI(ETFKOSPI,14,"SMA")
macd12269 <- MACD(ETFKOSPI,12,26,9,"SMA")
macd7205 <- MACD(ETFKOSPI,7,20,5,"SMA")
bbands<-BBands(ETFKOSPI,20,"SMA",2)

#지난 20일간 수익률 2% 이상이면 up, -2% down, 그사이 nowhere

direction <- data.frame(matrix(NA,dim(ETFKOSPI)[1],1))

#20일 수익률
lagret <- (ETFKOSPI - Lag(ETFKOSPI,20)) / Lag(ETFKOSPI,20)
direction[lagret > 0.02] <- "Up"
direction[lagret < -0.02] <- "Down"
direction[lagret < 0.02 & lagret > -0.02] <- "Nowhere"

ETFKOSPI <- cbind(ETFKOSPI,avg10,avg20, std10, std20,rsi5,rsi14,macd12269,macd7205,bbands)

#훈련용, 검증용, 평가용 데이터집합
test_edate <- "2019-02-22"
test_sdate <- as.Date(test_edate)-365
vali_edate <- test_sdate- 1
vali_sdate <- vali_edate - 365
train_edate <- vali_sdate-1
train_sdate <- train_edate - 1825


trainrow <- which(index(ETFKOSPI) >= train_sdate & index(ETFKOSPI) <= train_edate)
valirow <- which(index(ETFKOSPI) >= vali_sdate & index(ETFKOSPI) <= vali_edate)
testrow <- which(index(ETFKOSPI) >= test_sdate & index(ETFKOSPI) <= test_edate)

trainETFKOSPI <- ETFKOSPI[trainrow,]
valiETFKOSPI <- ETFKOSPI[valirow,]
testETFKOSPI <- ETFKOSPI[testrow,]

trainme <- apply(trainETFKOSPI,2,mean)
trainstd <- apply(trainETFKOSPI,2,sd)

#정규화
trainidn <- (matrix(1,dim(trainETFKOSPI)[1],dim(trainETFKOSPI)[2]))
valiidn <- (matrix(1,dim(valiETFKOSPI)[1],dim(valiETFKOSPI)[2]))
testidn <- (matrix(1,dim(testETFKOSPI)[1],dim(testETFKOSPI)[2]))
norm_trainETFKOSPI <- (trainETFKOSPI - t(trainme*t(trainidn)))/t(trainstd*t(trainidn))
norm_valiETFKOSPI <- (valiETFKOSPI - t(trainme*t(valiidn)))/t(trainstd*t(valiidn))
norm_testETFKOSPI <- (testETFKOSPI - t(trainme*t(testidn)))/t(trainstd*t(testidn))

traindir <- direction[trainrow,1]
validir <- direction[valirow,1]
testdir<-direction[testrow,1]



#신경망 적합(정규화열,날짜별 방향, 신경수, 트레이스 출력 여부)
set.seed(1)
model <- nnet(norm_trainETFKOSPI, class.ind(traindir),size = 3, trace = F)
model

dim(norm_trainETFKOSPI)

vali_pred <- predict(model, norm_valiETFKOSPI)
head(vali_pred)

vali_pred_class <- data.frame(matrix(NA,dim(vali_pred)[1],1))
vali_pred_class[vali_pred[,"Down"]>0.5, 1]<- "Down"
vali_pred_class[vali_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
vali_pred_class[vali_pred[,"Up"]>0.5, 1] <- "Up"

vali_pred_class
matrix<- confusionMatrix(table(vali_pred_class[,1],validir))
matrix                         
#87.65% 

test_pred <- predict(model,norm_testETFKOSPI)
test_pred_class <- data.frame(matrix(NA,dim(test_pred)[1],1))
test_pred_class[test_pred[,"Down"]>0.5, 1]<- "Down"
test_pred_class[test_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
test_pred_class[test_pred[,"Up"]>0.5, 1] <- "Up"

test_matrix <- confusionMatrix(table(test_pred_class[,1],testdir))
test_matrix
#82pro

#Signal generator
signal <- ifelse(test_pred_class == "Up", 1, ifelse(test_pred_class == "Down",-1,0))

getSymbols("102110.KS")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
getSymbols("123310.KS")
inverse <-`123310.KS`
inverse <- inverse[,"123310.KS.Close"]
inverse <- na.omit(inverse)
ret <- ETFKOSPI/lag(ETFKOSPI) -1
ret <- ret[index(testETFKOSPI)]
ret
inv <- inverse/lag(inverse)-1
inv <- inv[index(testETFKOSPI)]
cost <- 0
length(signal)
dim(ret)

if (signal ==1 | signal ==0){
  trade_ret = ret*signal - cost
}
if (signal ==-1){
  trade_ret = inv*(-signal) - cost
}
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
#charts.PerformanceSummary(cumm_ret)
#plot(cumm_ret)



##### Deep Neural Network ####

set.seed(1)
model <- dbn.dnn.train(norm_testETFKOSPI,class.ind(traindir),hidden=c(3,4,6))
nn.predict(model,norm_valiETFKOSPI)

nn.test(model,norm_valiETFKOSPI,class.ind(validir),t=0.4)

data <- cbind(as.data.frame(norm_trainETFKOSPI),traindir)
class(norm_trainETFKOSPI)
class(traindir)
h2o.init()
datah2o <- as.h2o(data,"h2o")
class(datah2o)

dim(datah2o)

#은닉층이 4개, 각 뉴런수 4,5,2,7
model <- h2o.deeplearning(1:15, 16,training_frame = datah2o,hidden = c(4,5,2,7))
vali_pred <- predict(model, as.h2o(norm_valiETFKOSPI,"h2o"))
vali_pred <- as.data.frame(vali_pred)
model
vali_pred_class <- data.frame(matrix(NA,dim(vali_pred)[1],1))
vali_pred_class[vali_pred[,"Down"]>0.5, 1]<- "Down"
vali_pred_class[vali_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
vali_pred_class[vali_pred[,"Up"]>0.5, 1] <- "Up"

vali_matrix <- confusionMatrix(table(vali_pred_class[,1],validir))
vali_matrix

test_pred <- predict(model, as.h2o(norm_testETFKOSPI,"h2o"))
test_pred <- as.data.frame(test_pred)
test_pred_class <- data.frame(matrix(NA,dim(test_pred)[1],1))
test_pred_class[test_pred[,"Down"]>0.5, 1]<- "Down"
test_pred_class[test_pred[,"Nowhere"]>0.5,1] <- "Nowhere"
test_pred_class[test_pred[,"Up"]>0.5, 1] <- "Up"

test_matrix <- confusionMatrix(table(test_pred_class[,1],testdir))
test_matrix


## Deep Graph ##
signal <- ifelse(test_pred_class == "Up", 1, ifelse(test_pred_class == "Down",-1,0))
getSymbols("102110.KS")
ETFKOSPI <-`102110.KS`
ETFKOSPI <- ETFKOSPI[,"102110.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)
getSymbols("123310.KS")
inverse <-`123310.KS`
inverse <- inverse[,"123310.KS.Close"]
inverse <- na.omit(inverse)
ret <- ETFKOSPI/lag(ETFKOSPI) -1
ret <- ret[index(testETFKOSPI)]
ret
inv <- inverse/lag(inverse)-1
inv <- inv[index(testETFKOSPI)]
cost <- 0
length(signal)
dim(ret)

if (signal ==1 | signal ==0){
  trade_ret = ret*signal - cost
}
if (signal ==-1){
  trade_ret = inv*(-signal) - cost
}
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)

charts.PerformanceSummary(trade_ret)
