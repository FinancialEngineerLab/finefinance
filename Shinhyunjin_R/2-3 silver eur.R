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
#install.packages("quantstrat")
#install.packages("devtools")
#devtools::install_github("braverock/blotter")
#devtools::install_github("braverock/quantstrat")

# load library #
library(blotter)
library(quantstrat)
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

#----------------------------------------------------------------------------------------------#
### data preprocessing ###

# Quandl Commodity Ticker list
# Silver : CHRIS/CME_GC1
# EUR : CHRIS/CME_HG1
# Silver
# EUR/USD : QUANDL/EURUSD
# opec price : OPEC/ORB
# GDP : FRED/GDP
# PI: FRED/PINCOME
# 

## Silver and EUR Ratio ##
silver <- Quandl("CHRIS/CME_SI1", type = "xts", collpase = "daily", start_date = "2014-01-01")
eur <- Quandl("FRED/DEXUSEU", type = "xts",collapse = "daily", start_date = "2014-01-01")
#eur <- zoo(eur)

silver <- silver$Settle#
# <- eur$v1
# merged : Quandl(c('ticker1', 'ticker2'))

#eur
df <- na.locf(merge.xts(eur, silver$Settle), formLast = TRUE)
colnames(df) <- c("EUR", "Silver")

#tail(df)

df$ESratio <- (df$EUR / df$Silver)
#df
#dygraph(df$ESratio) %>%
#  dySeries("EUR") %>%
#  dySeries("Silver")

# basic visualization #
chartSeries(df$Silver, subset ='2019::2020-02', theme = chartTheme('white'),
            TA="addBBands(); addDEMA()")
chartSeries(df$EUR, subset ='2019::2020-02', theme = chartTheme('white'),
            TA="addBBands(); addDEMA()")

# specific period #
par(mar=c(5,4,4,5))
plot((df$ESratio), type = "l", main = "Silver - EUR Ratio")
abline(h=c(603.6, 602.912,603.85,588.22,575.59), col="blue", lty = 2, lwd =2)
par(new=T)
plot(tail(df$Silver, 252), type = "l", lty =1, col = "blue",  yaxt= "n", ylab = NA, main = NA)
par(new=T)
plot(tail(df$EUR, 252), type = "l", lty =1, col = "orange",  yaxt= "n", ylab = NA, main = NA)
par(new=T)
Axis(side = 4)
mtext(side = 4, line = 3, 'Commodity Price')

#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#

#### Spread Trading ####

# price chagne regression
df_changes <- apply(df, 2, diff)
df_changes
df_changes[,1] # Silver
df_changes[,2] # EUR
plot(df_changes[,1], df_changes[,2],
     xlab = "Silver Futures price Changes",
     ylab= "EUR Futures price Changes",
     main = "Silver vs EUR",
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

# if Silver futures moves delta price, EUR futures move beta * delta price of Silver futures

#### Total least Squares Beta ####

tls_Silver <- diff(as.numeric(df$Silver))
tls_EUR <- diff(as.numeric(df$EUR))

plot(tls_Silver, tls_EUR, main = "Scatter plot of returns. Silver vs EUR",
     cex.main = 0.8, cex.lab= 0.8, cex.axis = 0.8)
abline(lm(tls_Silver ~ tls_EUR), abline(lm(tls_EUR ~ tls_Silver), lty =2))
grid()

# First PCA
r <- prcomp(~tls_Silver+tls_EUR)
slope <- r$rotation[2,1] / r$rotation[1,1]
intercept <- r$center[2] - slope*r$center[1]
abline(a=intercept, b = slope, lty =3)


#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------------------------#

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
start_date_is <- "2017-01-01"
end_date_os <- Sys.Date()
start_date_os <- end_date_os - 180
end_date_is <- start_date_os -1

####################
# In sample Analysis #
# x : Silver
# y : EUR
results <- calculate_beta_and_level(df$Silver, df$EUR, start_date_is, end_date_is)
results$beta
results$level

# y : EUR
results <- calculate_beta_and_level( df$EUR,df$Silver, start_date_is, end_date_is)
results$beta
results$level

plot(results$spread, ylab = "Spread Value", main = "EUR - beta * Silver (IS)",
     cex.main = 1, cex.lab = 0.8, cex.axis = 0.9)
grid()


# Out Sample Analysis#

range <- paste(start_date_os, "::", end_date_os, sep ="")
spread_os <- calculate_spread( df$EUR[range],df$Silver[range], results$beta)

plot(spread_os, main = "EUR - beta * Silver (OS)",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(h = results$level, lwd =2)
grid()

####################

results$beta
results$level

#-------------------------------------------------------------------------------------------#
##### Advanced Spread Trading #####

#### Moving Beta ####

par(mfrow = c(2,1))

# In and out sample period #
#start_date_is <- "2017-01-01"
##end_date_os <- Sys.Date()
#start_date_os <- end_date_os - 180
#end_date_is <- start_date_os -1

# moving period
window_length <- 10

# y: Silver, x : EUR

range_is <- paste(start_date_is, "::", end_date_is, sep = "")
x_is <- df$EUR[range_is]
y_is <- df$Silver[range_is]

dF_is <- cbind(x_is, y_is)
names(dF_is) <- c("x_is","y_is")

# regression #
run_regression_is <- function(dF_is){
  return(coef(lm(y_is~x_is -1, data = as.data.frame(dF_is))))
}
rolling_beta_is <- function(z, width){
  rollapply(z, width = width, FUN = run_regression_is, by.column = FALSE, align = "right")
}


betas_is <- rolling_beta_is(diff(dF_is), window_length)
betas_is
data_is <- merge(betas_is, dF_is)
data_is$spread <- data_is$y_is - lag(betas_is,1) * data_is$x_is
data_is


###############
# IS Analysis #

# returns based #
returns <- diff(dF_is) / dF_is
return_beta <- rolling_beta_is(returns, window_length)
data_is$spreadR <- diff(data_is$y_is) / data_is$y_is - return_beta * diff(data_is$x_is)/ data_is$x_is


#tail(data)

#############################################
data_is$spread
threshold <- sd(data_is$spread, na.rm = TRUE)
threshold

level <- mean(data_is$spread, na.rm= TRUE)
#level <- 0

#level - threshold
#level + threshold

plot(data_is$spread, main = "EuR -  Beta * Silver (IS)",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(h = level + threshold, lty = 2)
abline(h = level -threshold, lty = 2)

##########################################################
# OS Analysis 

range_os <- paste(start_date_os-18, "::", end_date_os, sep ="")
x_os <- df$EUR[range_os]
y_os <- df$Silver[range_os]

dF_os <- cbind(x_os, y_os)
dF_os
names(dF_os) <- c("x_os", "y_os")
#dF_os

run_regression_os <- function(dF_os){
  return(coef(lm(y_os~x_os -1, data = as.data.frame(dF_os))))
}

rolling_beta_os <- function(z, width){
  rollapply(z, width = width, FUN = run_regression_os, by.column = FALSE, align = "right")
}

betas_os <- rolling_beta_os(diff(dF_os),window_length)
betas_os

data_os <- merge(betas_os, dF_os)
tail(data_os,5)
data_os$spread <- data_os$y_os - lag(betas_os, 1) * data_os$x_os

#plot(data_os$spread, main = "Silver vs EUR (OS)",
#     cex.main = 0.8, cex.lab = 0.8, cex.axis =0.8)
#abline(h = threshold, lwd = 2)
#abline(h = -threshold, lwd = 2)

### Trading Order ###
#level <- 0
#level <-0
threshold
upper_level <- level + threshold
under_level <- level - threshold


buys <- ifelse(data_os$spread > level+ threshold, 1, 0)
sells <- ifelse(data_os$spread < level-threshold, -1, 0)


data_os$signal <- buys + sells
data_os


range_os_n <- paste(start_date_os, "::", end_date_os, sep ="")
data_os <- data_os[range_os_n]

plot(data_os$spread, main = "EUR - Beta * Silver (OS)",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
abline(h = threshold, lty = 2)
abline(h = -threshold, lty = 2)

point_type <- rep(NA, nrow(data_os))
buy_index <- which(data_os$signal ==1)
sell_index <- which(data_os$signal ==-1)

point_type[buy_index] <- 21
point_type[sell_index] <-24
points(data_os$spread, pch = point_type)
#threshold

num_of_buy_signals <- sum(buys, na.rm = TRUE)
num_of_sell_signals <- sum(abs(sells), na.rm = TRUE)

#--------------------------------------------------------------------#
## Profit and Loss ##

prev_x_qty <- 0
position <- 0
trade_size <- 10 # 10주
signal <- as.numeric(data_os$signal)
signal[is.na(signal)] <- 0
beta <- as.numeric(data_os$betas_os)

qty_x <- rep(0, length(signal))
qty_y <- rep(0, length(signal))

for (i in 1:length(signal)){
  if(signal[i] ==1 && position ==0){
    # spread long
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- -prev_x_qty
    qty_y[i] <- trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position ==0){
    # initial spread short
    prev_x_qty <- round(beta[i] * trade_size)
    qty_x[i] <- prev_x_qty
    qty_y[i] <- -trade_size
    position <- -1
  }
  
  if(signal[i] == 1 && position == -1){
    # spread buy to cover
    qty_x[i] <- -(round(beta[i] * trade_size)+prev_x_qty)
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- 2 * trade_size
    position <- 1
  }
  
  if(signal[i] == -1 && position ==1){
    # Spread Sell
    qty_x[i] <- round(beta[i] * trade_size)+ prev_x_qty
    prev_x_qty <- round(beta[i] * trade_size)
    qty_y[i] <- -2 * trade_size
    position <- -1
  }
}

qty_x[length(qty_x)] <- -sum(qty_x)
qty_y[length(qty_y)] <- -sum(qty_y)

data_os$qty_x <- qty_x
data_os$qty_y <- qty_y

data_os[1:3,]
tail(data_os, 5)

### PnL Visulaization ###

compute_equity_curve <- function(qty, price){
  cash_buy <- ifelse(sign(qty)==1, qty*price, 0)
  cash_sell <- ifelse(sign(qty)== -1, -qty*price, 0)
  position <- cumsum(qty)
  cumulative_buy <- cumsum(cash_buy)
  cumulative_sell <- cumsum(cash_sell)
  
  equity <- cumulative_sell - cumulative_buy + position * price
  
  return(equity)
}

data_os$equity_curve_x <- compute_equity_curve(data_os$qty_x, data_os$x_os)
data_os$equity_curve_y <- compute_equity_curve(data_os$qty_y, data_os$y_os)

plot(data_os$equity_curve_x + data_os$equity_curve_y, type = 'l',
     main = "EUR - Silver Spread Cummulative P&L", ylab = "P&L",
     cex.main = 0.8, cex.axis = 0.8, cex.lab = 0.8)


### MDD ###
drawdown <- function(x){
  cummax(x) - x
}

sharpe_ratio <- function(x, rf){
  sharpe <- (mean(x, na.rm=TRUE) -rf)/ sd(x, na.rm = TRUE)
  return(sharpe)
}

# ps. omega ratio #
#omega_ratio <- function(r, T){
#  omega <- mean(pmax(r-T,0))/ mean(pmax(T-r,0))
#  return(omega)
#}
#omega_ratio()

#### PnL and MDD Results ####

par(mfrow = c(2,1))

equity_curve <- data_os$equity_curve_x + data_os$equity_curve_y

plot(equity_curve, main = "PnL of Equity Curve",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)
plot(drawdown(equity_curve), main = "MDD of Equity Curve",
     cex.main = 0.8, cex.lab = 0.8, cex.axis = 0.8)

#

equity <- as.numeric(equity_curve[,1])
equity_curve_returns <- diff(equity) / equity[-length(equity)]

# Infinite and NaN Removal #
invalid_values <- is.infinite(equity_curve_returns) | is.nan(equity_curve_returns)
sharpe_ratio(equity_curve_returns[!invalid_values], 0.03)


# trading time #
trade_dates <- data_os$qty_x[data_os$qty_x != 0]
trade_dates

# duration #
duration <- as.numeric(diff(index(trade_dates)))
duration
summary(duration)

hist(duration, breaks = 20, main = "Histogram of trade durations",
     cex.main = 0.8, cex.lab=0.8, cex.axis = 0.8)


#------------------------------------------------------------------------------------------#

# Appendix : Trailing Stop and StopLoss


