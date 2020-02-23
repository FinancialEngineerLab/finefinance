##### ETF Backtest Tool Development #####

## Author : Shin Hyunjin ##

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
library(foreach)
library(lubridate)
library(xts)
library(dplyr)
library(dygraphs)
library(ggplot2)
library(zoo)
library(PerformanceAnalytics)
library(knitr)

#-----------------------------------------------------------------#

## Initial Setting ##

# Warning Message Hide #
options("getSymbols.warning4.0" = FALSE)
# data removal tool
rm(list = ls(.blotter), envir = .blotter)

# Currency, Time
currency('USD')
Sys.setenv(TZ = "UTC")

# Tickers
symbols <- c("XLB", #SPDR 소재
             "XLE", #SPDR 에너지
             "XLF", #SPDR 금융
             "XLP", #SPDR 필수 소비재
             "XLI", #SPDR 산업재 섹터
             "XLU", #SPDR 유틸리티 섹터
             "XLV", #SPDR 헬스케어 섹터
             "XLK", #SPDR 기술 섹터
             "XLY", #SPDR 내구소비재 섹터
             "RWR", #SPDR 다우존스 리츠 ETF
             "EWJ", # ishare japan
             "EWG", # ishare germany
             "EWU", # ishare uk
             "EWC", # ishare Canada
             "EWY", # ishare south korea
             "EWA", # ishare austrailia
             "EWH", # ishare hongkong
             "EWS", # ishare singapore
             "IYZ", # ishare us 통신 섹터
             "EZU", # ishare MSCI 유로존 ETF
             "IYR", # ishare 미국 부동산
             "EWT", # ishare Taiwan
             "EWZ", # ishare Brazil
             "EFA", # ishare EAFE
             "IGE", # ishare 북미 천연 자원
             "EPP", # ishare 일본 제외 아태
             "LQD", # ihsare 투자등급 사채
             "SHY", # ishare 1-3 bill
             "IEF", # ishare 3-7 bill
             "TLT") # ishare 20+ bill

#------------------------------------------------------------------------#

#### ADR Strategy ####

# 1 year before Rolling ATR Signal #

"lagATR" <- function(HLC, n = 14, maType, lag = 1, ...){
  ATR <- ATR(HLC, n =n , maType = maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- "atr"
  return(out)
}

"osDollarATR" <- function(orderside, tradeSize, pctATR, maxPctATR = pctATR, data, timestamp,
                          symbol, prefer = "Open", portfolio, integerQty = TRUE,
                          atrMod = "", rebal = FALSE, ...){
  if(tradeSize >0 & orderside =="short"){
    tradeSize <- tradeSize * -1
  }
  
  pos <- getPosQty(portfolio, symbol, timestamp)
  
  atrString <- paste0("atr", atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  
  if(length(atrCol) == 0){
    stop(paste("Term", atrString, "not found in mktdata column names."))}
  
  atrTimeStamp <- mktdata[timestamp, atrCol]
  
  if(is.na(atrTimeStamp) | atrTimeStamp == 0) {
    stop(paste("ATR corresponding to", atrString, "is invalid at this point in time. Add a logical operator to account for this."))
  }
  
  dollarATR <- pos*atrTimeStamp
  desiredDollarATR <- pctATR * tradeSize
  remainingRiskCapacity <- tradeSize * maxPctATR - dollarATR
  
  if(ordersize == "long"){
    qty <- min(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  }
  else{
    qty <- max(tradeSize * pctATR / atrTimeStamp,
               remainingRiskCapacity / atrTimeStamp)
  }
  
  if(integerQty) {
    qty <- trunc(qty)
  }
  
  if(!rebal){
    if(orderside=="long" & qty < 0){
      qty <- 0
    }
    if(orderside == "short" & qty > 0){
      qty <- 0
    }
  }
  if(rebal) {
    if(pos ==0){
      qty <- 0
    }
  }
  
  return(qty)
}

#------------------------------------------------------------------------------#

### Strategy Execution ###
require(quantstrat)
require(PerformanceAnalytics)

initDate = "1900-01-01"
from = "2010-01-01"
to = "2020-02-20"
options(width = 70)

if(!"XLB" %in% ls()){
  suppressMessages(getSymbols(symbols, from = from, to = to, src = "yahoo", adjust = TRUE))
}

# Class
stock(symbols, currency = "USD", multiplier = 1)

source("c:/users/check/dropbox/shinhyunjin_r/demoData.R")

# trade size and initial Equity #

tradeSize <- 10000
initEq <- tradeSize * length(symbols)

strategy.st <- "Clenow_Simple"
portfolio.st <- "Clenow_Simple"
account.st <- "Clenow_Simple"

rm.strat(portfolio.st)
rm.strat(strategy.st)

initPortf(portfolio.st, symbols = symbols, initDate =initDate, currency = 'USD')

initAcct(account.st, portfolios = portfolio.st, 
         initDate = initDate, currency= 'USD', initEq = initEq)

initOrders(portfolio.st, initDate = initDate)

strategy(strategy.st, store=TRUE)

#---------------------------------------------------------------------------------------#
#### Backtesting ####

nLag = 100
pctATR = 0.02
period = 10

namedLag <- function(x, k=1, na.pad = TRUE, ...){
  out <- lag(x, k = k, na.pad = na.pad, ...)
  out[is.na(out)] <- x[is.na(out)]
  colnames(out) <- "namedLag"
  return(out)
}

#

add.indicator(strategy.st, name = "namedLag",
              arguments = list(x=quote(Cl(mktdata)), k=nLag),
              label = "ind")
add.indicator(strategy.st, name = "lagATR",
              arguments = list(HLC = quote(HLC(mktdata)), n = period),
              label = "atrX")
#test <- applyIndicators(strategy.st, mktdata = (OHLC(XLP)))
#head(round(test, 2), 253)
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("Close", "namedLag.ind"),
                            relationship = "gt"), # 초과
           label = "coverOrBuy")
add.signal(strategy.st, name = "sigCrossover",
           arguments = list(columns = c("Close", "namedLag.ind"),
                            relationship = "lt"), # 미만
           label = "sellOrShort")

# long signal
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "coverOrBuy",
                          sigval = TRUE, ordertype = "market",
                          orderside= "long", replace = FALSE,
                          prefer = "Open", osFUN = osDollarATR,
                          tradeSize = tradeSize, pctATR = pctATR,
                          atrMod= "X"), type ="enter", path.dep = TRUE)
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sellOrShort",
                          sigval = TRUE, orderqty = "all", ordertype = "market",
                          orderside= "long", replace = FALSE,
                          prefer = "Open"), type ="exit", path.dep = TRUE)
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "sellOrShort",
                          sigval = TRUE, ordertype = "market",
                          orderside= "short", replace = FALSE,
                          prefer = "Open", osFUN = osDollarATR,
                          tradeSize = tradeSize, pctATR = pctATR,
                          atrMod= "X"), type ="enter", path.dep = TRUE)
add.rule(strategy.st, name = "ruleSignal",
         arguments = list(sigcol = "coverOrBuy",
                          sigval = TRUE, orderqty = "all", ordertype = "market",
                          orderside= "short", replace = FALSE,
                          prefer = "Open"), type ="exit", path.dep = TRUE)

# start time
t1 <- Sys.time()
out <- applyStrategy(strategy = strategy.st, portfolios =  portfolio.st)

# end time
t2 <- Sys.time()
print(t2- t1)







