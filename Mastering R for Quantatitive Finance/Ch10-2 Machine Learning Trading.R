

#### Neural Network : Machine Learning ####

library(quantmod)
library(TTR)
library(nnet)
library(caret)

## data crawling ##
getSymbols("005610.KS", from = "2013-01-01") ## SPC 삼립 
spc <- `005610.KS`
spc <- na.omit(spc)

# data cleaning #
spc <- spc[,1:4]
price <- spc$`005610.KS.Close`
HLC <- matrix(c(as.numeric(spc$`005610.KS.High`),as.numeric(spc$`005610.KS.Low`),
                as.numeric(spc$`005610.KS.Close`)), nrow=length(as.numeric(spc$`005610.KS.High`)))
spc.lr <- diff(log(price))

# Technical variables #
rsi <- RSI(price)
MACD <- MACD(price)
macd <- MACD[,1]
will <- williamsAD(HLC)
cci <- CCI(HLC)
STOCH <- stoch(HLC)
stochK <- STOCH[,1]
stochD <- STOCH[,2]

Input <- matrix(c(as.numeric(rsi[1018:1457]), cci[1018:1457], as.numeric(macd[1018:1457]),
                  will[1018:1457], stochK[1018:1457], stochD[1018:1457]), nrow = 440)
Target <- matrix(c(spc.lr[1019:1458]), nrow  =440)
trainingdata <- cbind(Input, Target)
colnames(trainingdata) <- c("RSI", "CCI", "MACD", "WILL", "STOCHK", "STOCHD", "Return")

### Data split ###
## 학습 : 9:1(연습, 유효성검사)

trainIndex <- createDataPartition(spc.lr[1018:1457], p = 0.9, list = FALSE)
spc.train <- trainingdata[trainIndex,]
spc.test <- trainingdata[-trainIndex,]

# 학습 Training

best.network <- matrix(c(5,0.5))
best.rmse <-1
for(i in 5:15){
  for(j in 1:3){
    spc.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD,
                    data = spc.train, maxit = 1000, size = i,
                    decay = 0.01*j, linout = 1)
    spc.predict <- predict(spc.fit, newdata = spc.test)
    spc.rmse <- sqrt(mean((spc.predict - as.numeric(spc.lr[1436:1457]))^2))
    if(spc.rmse < best.rmse){
      best.network[1,1] <- i
      best.network[2,1] <- j
      best.rmse <- spc.rmse
    }
}}

# test data cleaning
InputTest <- matrix(c(as.numeric(rsi[1457:1486]), cci[1457:1486],as.numeric(macd[1457:1486]),
                        will[1457:1486], stochK[1457:1486], stochD[1457:1486]), nrow = 30)
  
TargetTest <- matrix(c(spc.lr[1458:1487]), nrow = 30)
Testdata <- cbind(InputTest, TargetTest)
colnames(Testdata) <- c("RSI", "CCI", "MACD", "WILL", "STOCHK", "STOCHD", "Return")
  

## test data -> neural network Fit
spc.fit <- nnet(Return ~ RSI+CCI+MACD+WILL+STOCHK+STOCHD, data = trainingdata, maxit=1000,
                  size = best.network[1,1], decay = 0.1*best.network[2,1], linout = 1)
spc.predict1 <- predict(spc.fit, newdata = Testdata)
  
for(i in 1:20){
  spc.fit <- nnet(Return ~ RSI + CCI + MACD + WILL + STOCHK + STOCHD, data=trainingdata,
                    maxit = 1000,
                    size = best.network[1,1], decay = 0.1*best.network[2,1], linout = 1)
    
  spc.predict <- predict(spc.fit, newdata = Testdata)
  spc.predict1 <-(spc.predict1 + spc.predict) /2
}
  
money <- money2 <- matrix(0,31)
money[1,1] <- money2[1,1] <- 100
  
for(i in 2:31){
    direction1 <- ifelse(spc.predict1[i-1] < 0, -1,1)
    direction2 <- ifelse(TargetTest[i-1] < 0, -1,1)
    money[i,1] <- ifelse((direction1 - direction2) == 0,
    money[i-1,1]*(1+abs(TargetTest[i-1])),
    money[i-1,1]*(1-abs(TargetTest[i-1])))
    money2[i,1] <- 100*(as.numeric(price[1457+i-1])/as.numeric(price[1457]))
}
  
#Visualization
x <- 1:31
matplot(cbind(money, money2), type = "l", xaxt = "n", ylab = "", col = c("black","grey"),lty=1)
legend("topleft", legend = c("Neural Network", "Benchmark"), pch = 19, col = c("black","grey"))
axis(1, at = c(1,10,20,30),lab = c("2018-12-11", "2018-12-22", "2019-01-01", "2018-01-11"))
box()
mtext(side =1, "Test dataset", line=2)
mtext(side =2, "Investment value", line=2)


#### Log Optimal Portfolio ####

all_files <- list.files("data")

d <- read.table(file.path("data", all_files[[1]]), sep = ",", header = FALSE)
