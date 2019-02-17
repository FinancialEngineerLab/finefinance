#### LSTM ####
rm(list=ls())

library(keras)
library(devtools)
library(quantmod)
library(dummies)
library(tensorflow)

## Load and prep data!
getSymbols('GOLD.AX')
GAX <- GOLD.AX
GAX <- GAX$GOLD.AX.Close
GAX <- na.omit(GAX)

# dummies for categorical varibles
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

## assuming target variable is in the last column


## For a multi-class target variable add the following code, here:
# Y = to_categorical(Y)

## split data, training & testing, 80:20, AND convert dataframe to an array

Y_train = isGAX[,16]
X_train  = isGAX[,1:15]

Y_test = osGAX[,16]
X_test  = osGAX[,1:15]

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

track = model %>% fit(X_train, Y_train, epochs = 150, batch_size = 10,
                      callbacks = callback_early_stopping(patience = 2, monitor = 'acc'),
                      validation_split = 0.3
)
plot(track)

##prediction 
pred <- model %>% predict(X_test, batch_size = 128)
Y_pred = round(pred)
# Confusion matrix
CM = table(Y_pred, Y_test)

# evaluate the model
evals <- model %>% evaluate(X_test, Y_test, batch_size = 10)

accuracy = evals[2][[1]]* 100
accuracy

## Graph ##
pred_direction <- NULL
pred_direction[Y_pred ==1] <- "Up"
pred_direction[Y_pred ==0] <- "Down"
# direction
signal <- ifelse(pred_direction == "Up", 1, ifelse(pred_direction == "Down",-1,0))

GAX <- GOLD.AX
GAX <- na.omit(GOLD.AX$GOLD.AX.Close)
GAX <- na.omit(GAX)
ret <- GAX/lag(GAX)-1
ret <- ret[osrow]
cost <- 0

length(ret)
length(signal)
trade_ret <- ret * Lag(signal) - cost
dim(ret)
cumm_ret <- Return.cumulative(trade_ret)
anual_ret <- Return.annualized(trade_ret)
charts.PerformanceSummary(trade_ret)
