###Real Final LSTM
#### LSTM ####
rm(list=ls())
library(keras)
library(devtools)
library(quantmod)
library(dummies)
library(tensorflow)
library(PerformanceAnalytics)
library(caret)
library(xts)
library(quantmod)
library(caret)
library(nnet)
library(caret)
library(deepnet)
library(h2o)
library(caret)

## Load and prep data!
getSymbols("123310.KS") #ETFKOSPI
ETFKOSPI <-`123310.KS`
ETFKOSPI <- ETFKOSPI[,"123310.KS.Close"]
ETFKOSPI <- na.omit(ETFKOSPI)




# dummies for categorical varibles

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
ETFKOSPI <- na.omit(ETFKOSPI)
N = nrow(ETFKOSPI)
p = ncol(ETFKOSPI)

## assuming target variable is in the last column

X = dummy.data.frame(ETFKOSPI[, -p])
Y = ETFKOSPI[, p]
## For a multi-class target variable add the following code, here:
# Y = to_categorical(Y)

data = cbind(X, Y)

## split data, training & testing, 80:20, AND convert dataframe to an array
train_sdate <- "2011-02-17"
train_edate <- "2017-02-17"
vali_sdate <- "2017-02-18"
vali_edate <- "2018-02-18"
test_sdate <- "2018-02-19"
test_edate <- Sys.Date()

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

p = ncol(data)
Y_train = data.matrix(data[trainrow, p])
X_train  = data.matrix(data[trainrow, -p])

Y_test = data.matrix(data[valirow, p])
X_test  = data.matrix(data[valirow, -p])

k = ncol(X_train)


## create your model,and add layers 
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 60, activation = 'relu', input_shape = k) %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 50, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 1, activation = 'sigmoid')

## see your model structure
summary(model)


## compile the model


model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam( lr= 0.0001 , decay = 1e-6 ),  
  metrics = c('accuracy')
  
)
## fit the model

track = model %>% fit(X_train, Y_train, epochs = 150, batch_size = 16,
                      callbacks = callback_early_stopping(patience = 20, monitor = 'acc'),
                      validation_split = 0.3
)
plot(track)

##prediction 
pred <- model %>% predict(testETFKOSPI[,-16], batch_size = 128)
Y_pred = round(pred)

# Confusion matrix
CM = confusionMatrix(table(Y_pred, testETFKOSPI[,16]))
CM

signal <- ifelse(Y_pred == 1, 1, ifelse(Y_pred == 0,-1,0))

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

