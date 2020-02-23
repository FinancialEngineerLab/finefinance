##### Global Macro Trading Analysis #####

## Author : Shin Hyunjin ##

#-----------------------------------------------------------------------#

# install packages #
#install.packages("qrmdata")
#install.packages("quantmod")
#install.packages("TTR")
#install.packages("QuantTools")
#install.packages("huge")
#install.packages("tseries")
#install.packages("Quandl")
#install.packages("forecast")
#install.packages("lubridate")
#install.packages("xts")
#install.packages("dplyr")
#install.packages("dygraphs")
#install.packages("PerformanceAnalytics")
#install.packages("knitr")
# load library #
library(qrmdata)
library(quantmod)
library(TTR)
library(QuantTools)
library(dygraphs)
library(forecast)
library(Quandl)
library(forecast)
library(lubridate)
library(xts)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(zoo)
library(PerformanceAnalytics)
library(knitr)

#library(huge)
#library(tseries)

# example #
#data("FTSE")
#head(FTSE)

#-------------------------------------------------------------------------#
### data preprocessing ###

# Quandl Ticker list
# GOLD : CHRIS/CME_GC1
# copper : CHRIS/CME_HG1

## Gold and Copper Ratio ##

gold <- Quandl("CHRIS/CME_GC1", type = "xts", collpase = "daily", start_date = "2015-01-01")
copper <- Quandl("CHRIS/CME_HG1", type = "xts", collpase = "daily", start_date = "2015-01-01")

df <- na.locf(merge.xts(copper$Settle, gold$Settle), formLast = TRUE)
colnames(df) <- c("Copper", "Gold")
tail(df)

df$GHratio <- (df$Gold / df$Copper)
df

#dygraph(df$GHratio) %>%
#  dySeries("Copper") %>%
#  dySeries("Gold")

# basic visualization #
chartSeries(df$Gold, subset ='2019::2020-02', theme = chartTheme('white'), TA="addBBands(); addDEMA()")
chartSeries(df$Copper, subset ='2019::2020-02', theme = chartTheme('white'), TA="addBBands(); addDEMA()")

# specific period #
par(mar=c(5,4,4,5))
plot(tail(df$GHratio, 252), type = "l", main = "Gold - Copper Ratio", ylim = c(400, 650))
abline(h=c(603.6, 602.912,603.85,588.22,575.59), col="blue", lty = 2, lwd =2)
par(new=T)
plot(tail(df$Gold, 252), type = "l", lty =1, col = "blue",  yaxt= "n", ylab = NA, main = NA)
par(new=T)
plot(tail(df$Copper, 252), type = "l", lty =1, col = "orange",  yaxt= "n", ylab = NA, main = NA)
par(new=T)
Axis(side = 4)
mtext(side = 4, line = 3, 'Commodity Price')

#--------------------------------------------------------------------------------------------------------#

#### Spread Trading ####

# price chagne regression
df_changes <- apply(df, 2, diff)
df_changes
df_changes[,1] # gold
df_changes[,2] # copper
plot(df_changes[,1], df_changes[,2],
     xlab = "Gold Futures price Changes",
     ylab= "Copper Futures price Changes",
     main = "Gold vs Copper",
     cex.main = 0.8,
     cex.lab = 0.8,
     cex.axis = 0.8)
grid()

ans <- lm(df_changes[,1] ~ df_changes[,2])
beta <- ans$coefficients[2]
ans2 <- lm(df_changes[,2] ~ df_changes[,1])
beta2 <- ans2$coefficients[2]
beta
beta2
# if gold futures moves delta price, copper futures move beta * delta price of gold futures

#### Total least Squares Beta ####

tls_gold <- diff(as.numeric(df$Gold))
tls_copper <- diff(as.numeric(df$Copper))

plot(tls_gold, tls_copper, main = "Scatter plot of returns. gold vs copper",
     cex.main = 0.8, cex.lab= 0.8, cex.axis = 0.8)
abline(lm(tls_gold ~ tls_copper), abline(lm(tls_copper ~ tls_gold), lty =2))
grid()

# First PCA
r <- prcomp(~tls_gold+tls_copper)
slope <- r$rotation[2,1] / r$rotation[1,1]
intercept <- r$center[2] - slope*r$center[1]
abline(a=intercept, b = slope, lty =3)

#---------------------------------------------------------------------------------------#
### Spread Trading Codes ###

calculate_spread <- function(x, y, beat){
  return (y-beta*x)
}

calculate_beta_and_level <- function(x, y, start_date, end_date){
  time_range <- paste(start_date, "::", end_date, sep ="")
  x <- x[time_range]
  y <- y[time_range]
  
  dx <- diff(x[time_range])
  dy <- diff(y[time_range])
  r <- prcomp(~ dx + dy)
  
  beta <- r$rotation[2, 1] / r$rotation[1,1]
  spread <- calculate_spread(x, y, beta)
  names(spread) <- "spread"
  level <- mean(spread, na.rm = TRUE)
  
  outL <- list()
  outL$spread <- spread
  outL$beta <- beta
  outL$level <- level
  
  return(outL)
}


#### Signal Maker ####

calculate_buy_sell_signals <- function(spread, beta, level, lower_threshodl, upper_threshold){
  
  buy_signals <- ifelse(spread <= level - lower_threshold, 1, 0)
  sell_signals <- ifelse(spread >= level + upper_threshold, 1, 0)
  
  # vector binding to matrix
  output <- cbind(spread, buy_signals, sell_signals)
  colnames(output) <- c("spread", "buy_signals", "sell_signals")
  
  return(output)
}

#### Basic  Excecution Part ####

par(mfrow = c(2,1))

# In and out sample period #
start_date_is <- "2018-07-01"
end_date_os <- Sys.Date()
start_date_os <- end_date_os - 90
end_date_is <- start_date_os -1

# In sample Analysis #
# x : gold
# y : copper
results <- calculate_beta_and_level(df$Gold, df$Copper, start_date_is, end_date_is)
results$beta
results$level

# y : copper
results <- calculate_beta_and_level( df$Copper,df$Gold, start_date_is, end_date_is)
results$beta
results$level

plot(results$spread, ylab = "Spread Value", main = "Copper - beta * Gold (IS)",
     cex.main = 1, cex.lab = 0.8, cex.axis = 0.9)
grid()

# Out Sample Analysis#

range <- paste(start_date_os, "::", end_date_os, sep ="")
spread_os <- calculate_spread( df$Copper[range],df$Gold[range], results$beta)

plot(spread_os, main = "Copper - beta * Gold (OS)",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(h = results$level, lwd =2)
grid()

results$beta
results$level

#### Moving Beta ####

# moving period
window_length <- 10

# time period
start_date <- ""