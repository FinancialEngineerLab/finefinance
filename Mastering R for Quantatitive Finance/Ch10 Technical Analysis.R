#### Ch 10. Technical Analysis and Machine Learning ####

library(quantmod)
library(TTR)
library(caret)
library(nnet)

### Data ###

getSymbols("005610.KS", from = "2015-02-01") ## SPC 삼립 
spc <- `005610.KS`
spc <- na.omit(spc)
#colnames(spc) <- c("O","H","L","C")


### Moving Average와 RSI ###

spc <- tail(spc, 150)
spc <- as.xts(spc)
#dev.new(width= 20, height = 10)
chartSeries(spc, dn.col = "red", TA= "addRSI(10);addEMA(10)")

### MACD ###
#dev.new(width = 20, height = 10)
chartSeries(spc, dn.col = "red", TA = "addMACD();addSMA(10)")

### CandleChart ###

OHLC <- function(d){
  windows(20,10)
  chartSeries(d, dn.col = "red")
}

is.trend <- function(ohlc, i,j){
  avg1 = mean(ohlc[c((i-25):i), 4])
  avg2 = mean(ohlc[c((j-25):j), 4])
  if(avg1 >= avg2)return(FALSE)
  
  ohlc <- ohlc[c(i:j), ]
    n <- nrow(ohlc)
    candle_l <- pmin(ohlc[,1], ohlc[,4])
    valley <- rep(FALSE, n)
    for(k in 2:(n-1))
      valley[k] <- (as.numeric(candle_l[k-1]) >= as.numeric(candle_l[k]) 
                    & (as.numeric(candle_l[k+1]) >= as.numeric(candle_l[k])))
    z <- as.numeric(candle_l[valley])
    if(all(z==cummax(z)))return(TRUE)
    return(FALSE)
}

is.trend.rev <- function(ohlc, i,j){
  if(is.trend(ohlc,i,j) == FALSE) return(FALSE)
  last_candle <- ohlc[j+1,]
  reverse_candle <- ohlc[j+2,]
  ohlc <- ohlc[c(i:j),]
  
  if(as.numeric(last_candle[,4]) < as.numeric(last_candle[,1])) return(FALSE)
  if(as.numeric(last_candle[,4]) < max(as.numeric(ohlc[,c(1,4)]))) return(FALSE)
  if(as.numeric(reverse_candle[,1]) < as.numeric(last_candle[,4]) 
     | as.numeric(reverse_candle[,4]) >= as.numeric(last_candle[,1])) return(FALSE)
  return(TRUE)
}


getSymbols("005610.KS", from = "2015-02-01", auto.assign = F)
spc <- `005610.KS`
spc <- na.omit(spc)
spc <- spc[,1:4]

n <- nrow(spc)
n

result <- c(0,0)

for(a in 26:726){
  for(b in (a+3):min(n-3, a+100)){
    if(is.trend.rev(spc, a, b) & b-a>10)
      result <- rbind(result, c(a,b))
    if(b ==n)
      break
  }
}

z <- aggregate(result, by = list(result[,2]), FUN = min)[-1, 2:3]

for(h in 1:nrow(z)){
  OHLC(spc[z[h,1]:z[h,2]+2,])
  title(main = z[h,])
}
