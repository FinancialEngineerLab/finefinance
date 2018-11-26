### library
install.packages('caret', dependencies = TRUE)
install.packages('nnet', dependencies = TRUE)
library(quantmod)
library(TTR)
library(caret)
library(nnet)

### Data ###

getSymbols('GC=F', from='2016-01-01', to = '2018-11-23')
data1 <- `GC=F`
data1 <- na.omit(data1)

###define variable
open1 = data1$`GC=F.Open`
close1 = data1$`GC=F.Close`
low1 = data1$`GC=F.Low`
high1 = data1$`GC=F.High`
volume1 = data1$`GC=F.Volume`
adclose1 = data1$`GC=F.Adjusted`
pricelog1 <- diff(log(close1))

HLC <- matrix(c(high1, low1, close1), nrow=length(high1), byrow=T)

rsi <- RSI(close1)
rsi <- matrix(rsi)
rsiMACD <- MACD(close1)
macd <- MACD[,1]
macd <- matrix(macd)
will <- williamsAD(HLC)
cci <- CCI(HLC)
STOCH <- stoch(HLC)
stochK <- STOCH[,1]
stochD <- STOCH[,1]

Input <- matrix(c(rsi[600:833], cci[600:833], macd[600:833], will[600:833], stochK[600:833], stochD[600:833]), nrow=234)
Target <- matrix(c(pricelog1[601:834]), nrow=234)
trainingdata <- cbind(Input, Target)
colnames(trainingdata) <- c("RSI", "CCI", "MACD", "WILL", "STOCHK", "STOCHD", "Return")

trainIndex <- createDataPartition(pricelog1[601:834], p=0.9, list= FALSE)
logprice1.train <- trainingdata[trainIndex,]
logprice1.test <- trainingdata[-trainIndex,]

best.network <- matrix(c(5,0.5))
best.rmse <- 1
for(i in 5:15)
  for(j in 1:3){
    price.fit <- nnet(Return ~ RSI + CCI + MACD + WiLL + STOCHK + STOCHD, data= logprice1.train, maxit = 1000, size = i, decay = 0.01 * j, linout=1)
    price.predict <- predict(price.fit, newdata = logprice1.test)
    price.rmse <- sqrt(mean((price.predict - pricelog1[])))
  }