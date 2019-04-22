#### Ch4 Bigdata ####

library(quantmod)
library(Quandl)

#### 1. Currency Big data ####
Quandl.api_key("YRyBqws79oytacx7XvDM")

currencies <- c("USD", "CHF", "GBP", "JPY", "RUB", "CAD", "AUD")
currencies <- paste("CURRFX/EUR", currencies, sep  ="")
currency_ts <- lapply(as.list(currencies), Quandl, start_date = "2015-01-01", end_date="2019-03-25", type = "xts")

Q <- cbind(
  currency_ts[[1]]$Rate, currency_ts[[3]]$Rate,currency_ts[[6]]$Rate, currency_ts[[7]]$Rate)
#USD GBP CAD AUD (all  / EUR)

matplot(Q, type = "l", xlab = "", ylab = "", main = "USD,GBP,CAD,AUD",xaxt = 'n', yaxt = 'n')
ticks = axTicksByTime(currency_ts[[1]])
abline(v=ticks, h = seq(min(Q, na.rm=TRUE), max(Q, na.rm = TRUE), length =5),col="grey", lty=4)
axis(1, at = ticks, labels = names(ticks))
axis(2, at = seq(min(Q, na.rm=TRUE), max(Q, na.rm=TRUE), length=5),labels=round(seq(min(Q,na.rm=TRUE),
                                                                                    max(Q,na.rm=TRUE),
                                                                                        length= 5),1))
legend("topright", legend=c("USD/EUR", "GBP/EUR", "CAD/EUR", "AUD/EUR"),col = 1:4, pch=19)

#### 2. Stock Big data ####

etf <- new.env()
getSymbols("139230.KS", from  = as.Date("2016-01-01"), to = Sys.Date())
etf <- `277630.KS`
#etf <- etf$`277630.KS.Adjusted`
head(etf)
chartSeries(etf, multi.col=TRUE, theme= "black")
addMACD()
addBBands()

etf_return <- etf$`277630.KS.Close`/lag(etf$`277630.KS.Close`,-1) -1
etf_return
etf_return <- na.omit(etf_return)
#qqnorm(etf_return$`277630.KS.Close`)
qqnorm(etf_return, main = "Normal QQ plot of ETF heavy log return", xlab = "Theoretical Quantile",
       ylab="Sample Quantile", plot.it  =TRUE, datax=FALSE)
qqline(etf_return, col = "red")
#qqplot(etf_return, index(etf_return))

#### 3. Big Matrix and K-mean Clustering ####

install.packages("bigmemory")
install.packages("biganalytics")
library(bigmemory)
library(biganalytics)

x <- read.big.matrix("C:/users/shinhyunjin/dropbox/data/FlightTicketData.csv", type = 'integer',
                     header =TRUE, backingfile = "data.bin", descriptorfile = "data.desc")
xm <- as.matrix(x)
nrow(x)

#

res_bigkmeans <- lapply(1:10, function(i){
  bigkmeans(x, centers=i, iter.max=50, nstart = 1)})

lapply(res_bigkmeans, function(x) x$withinss)
var <- sapply(res_bigkmeans, function(x) sum(x$withinss))
plot(1:10, var, type = "b", xlab  = "Number of Clusters", ylab ="Percentage of variance explained")

res_big <- bigkmeans(x, centers = 3, iter.max = 50, nstart=1)
res_big

# 가설검정

size <- round(seq(10, 2500000, length = 20))
nsize <- length(size)
calc.time <- matrix(NA, nrow = nsize, ncol =2)
for(i in 1:nsize){
  size.i <- size[i]
  xm.i <- xm[1:size.i,]
  vec1 = rep(0,10)
  vec2 = rep(0,10)
  for(j in 1:10){
    vec1[j] <- system.time(kmeans(xm.i, centers = 3, iter.max = 50, nstart =1))[3]
    vec2[j] <- system.time(bigkmeans(xm.i, centers =3, iter.max = 50, nstart =1))[3]
  }
  calc.time[i,1] <- mean(vec1)
  calc.time[i,2] <- mean(vec2)
}
matplot(size, calc.time, main ="Processing time",type = "l", ylab = "calc time")


### 4. Bigdata with linear Regression of 실업급여 ###

install.packages("ff")
install.packages("biglm")
library(ff)
library(biglm)

download.file("http://www.irs.gov/file_source/pub/irs-soi/12zpallagi.csv", "soi.csv")
x<-read.csv(file = "soi.csv", header =TRUE)
x

mymodel <- biglm(A02300 ~ A00200 + AGI_STUB + NUMDEP + MARS2, data = x)
summary(mymodel)
summary(mymodel)$rsq #r square

