
##### Systematic Bigdata Pair Trading Project with Machine and Statistcal Learning #####

## Author : Shin Hyunjin ##

#install.packages("doParallel")
#install.packages("lmtest")
#install.packages("doSNOW")
#install.packages("doMPI")
#install.packages("dplyr")

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
library(urca)
library(doParallel)
#library(lmtest)

#### data load ####


pkgs <- list("quantmod", "doParallel", "foreach", "urca","dplyr")
lapply(pkgs, require, character.only=T)
#registerDoParallel(cores =4)
cl <- parallel::makeCluster(detectCores(logical=TRUE))
registerDoParallel(cl)


jtest <- function(t1, t2){
  start <- sd
  getSymbols(t1, from = start)
  getSymbols(t2, from = start)
  j <- summary(ca.jo(cbind(get(t1)[,6], get(t2)[,6])))
  r <- data.frame(stock1 = t1, stock2 = t2, stat = j@teststat[2])
  r[, c("pct10", "pct5", "pct1")] <- j@cval[2,]
  return(r)
}


pair <- function(lst){

  d2 <- data.frame(t(combn(lst, 2)))
  stat <- foreach(i = 1:nrow(d2), .combine = rbind, .packages = "doParallel") %dopar% jtest(as.character(d2[i,1]), as.character(d2[i,2]))
  stat <- stat[order(-stat$stat),]
  
  rownames(stat) <- NULL
  return(stat)
}


### input ###

sd <- "2015-01-01"
tickers <- c("GSPC", "IXIC")

### result ###
pair(tickers)

#--------------------------------------------------------------------------------------

