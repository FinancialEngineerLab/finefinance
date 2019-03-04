##### Ch7 Financial Programming #####

#### 7-1 Basic Finance ####

### 7.1.2 Returns ###

yn <- function(t, n, c0, ct) n*((ct/c0)^(1/(t*n))-1)*100
n <- 1:20
c0 <-1 ; ct<-1.05;t<-1
plot(n, sapply(n, function(x) yn(t,x,c0,ct)), type = 'l', ylab = 'yield(%)') # sapply : vector 연산

x <- ((105/100)^(1/6)-1)*12
x

### 7.1.3 미래가치 ###

T <- 1:50
r <- 0.1
FV <- (1+r)^T

plot(T, FV, type = 'l')

### 7.1.4 현재가치 ###

T <- 1:50
r <- 0.1
PV <- 1/(1+r)^T
plot(T,PV, type = 'l')

# 만기별, 수익률별 현재가치와 미래가치 #

T <- 0:10
rates <- 1:20/100
par(mfrow=c(1,2))
# FV
r <- max(rates)
FV <- (1+r)^T
plot(T,FV, type = 'n', main = "FV")
for (r in rates) lines(T, (1+r)^T)
# PV
r <- max(rates)
PV <- 1/(1+r)^T
plot(T,PV, type = 'n', main = "PV")
for(r in rates) lines(T, 1/(1+r)^T)


### 7.2 Security price analysis ###

install.packages("tseries")
require('tseries')
library(tseries)

## Data ##
hsce <- get.hist.quote("^HSCE", quote = "AdjClose", quiet = TRUE, start = as.Date('2017-01-01'), end = Sys.Date())
hsce <-na.omit(hsce)
plot(hsce, main = "HSCE")
head(hsce)
tail(hsce)

## Return ##

ret_simple <- diff(hsce)/lag(hsce, k = -1)*100
plot(ret_simple)
ret_log <- diff(log(hsce))*100
summary(ret_simple)
summary(ret_log)

hist(ret_simple)
hist(ret_log)

#specific periods #

ret_simple_p1 <- window(hsce, start = '2018-10-01', end = '2019-02-04')
plot(ret_simple_p1)

# quantmod

library(quantmod)
getSymbols("^KS11",from = "2017-01-01")
kospi <- KS11
head(kospi)

colnames(kospi) <- c("op", "hi", "lo","close","vol","adj")
head(kospi)

# chart series #

chartSeries(hsce, theme="white", TA=NULL)
chartSeries(kospi, theme= "black")
candleChart(hsce, type = c("matchsticks"), subset = '2017-01::2019', theme = "black")
candleChart(kospi, subset = '2017-01::2019', theme = "black")

# * technical analysis graph #

chartSeries(hsce, theme = "black", TA = "addVo();addBBands();addCCI()")
chartSeries(kospi$adj, theme = "black", TA = "addVo();addBBands();addCCI()")

par(mfrow = c(1,2))
lineChart(hsce, layout =NULL, theme = "black")
lineChart(ret_simple, layout= NULL, theme = "black")

# Distribution of returns #
install.packages("fBasics")
library(fBasics)

X<-returns(hsce)
X <- na.omit(X)
par(mfrow=c(1,2))
nFit(X, doplot=TRUE, apan =  "auto")
nigFit(X, trace =FALSE)


### 7.2.2 Portfolio ###

## Diversitifcation effect -> effect by correlation ##

a_seq <- seq(-0.2, 1.2, length = 100)
rhos <- c(-1, -0.5, 0.5,1)
out <- matrix(0, nrow = 100, ncol = 5)
for(i in 1:5){
  out[,i] <- sapply(a_seq, function(a){
    rho <- rhos[i]
    ans <- a^2*0.2^2 + (1-a)^2 * 0.3^2 + 2*a*(1-a)*rho*0.2*0.3
    return(ans)
  })
}
matplot(a_seq, out, type='l')
nms <-c('rho=-1', 'rho=-0.5', 'rho=0.0','rho=0.5','rho=+1')
legend("topright", legend = nms, lty=1:5, col=1:5, bty="n")

### 7.2.3 Efficient Frontier ###

library(quantmod)
stockData <- new.env()
symbols <- c("000660.KS","051910.KS","068270.KS", "005490.KS")
# 하이닉스 
symbols
getSymbols(symbols, env = stockData, from = "2016-01-01")
x <- list()

for(i in 1:length(symbols)){
  x[[i]] <- get(symbols[i], pos = stockData)
  x[[i]]$gl <-((Cl(x[[i]])-Op(x[[i]]))/Op(x[[i]]))*100
  if(i==1)
    data<-Cl(x[[i]])
  else
    data<- cbind(data, Cl(x[[i]]))
}

data_ret <- apply(data,2, Delt)
napos <- which(apply(data_ret,2,is.na)) # na removed
avg_ret <- apply(data_ret[-napos,], 2, mean)
covariance_mat <- cov(data_ret, use = 'na')

weights <- c(0.25, 0.25, 0.25, 0.25) # initial weight


library(fPortfolio)
library(tseries)
source("C:/Users/Shinhyunjin/Dropbox/data/portfolio.R")
weightedport = getPortfolio(er=avg_ret, cov.mat = covariance_mat, weights =weights)
weightedport
minvar_port <- globalMin.portfolio(avg_ret, covariance_mat)
minvar_port

rf <- 0
efficient_port <- efficient.portfolio(avg_ret, covariance_mat, rf)
efficient_port

tangency_port <- tangency.portfolio(avg_ret, covariance_mat, rf)

efficient_frontier <- efficient.frontier(avg_ret, covariance_mat, alpha.min =-2,alpha.max=2, nport=50)
plot(efficient_frontier, plot.assets =T)
points(minvar_port$sd, minvar_port$er, col = "blue")
points(tangency_port$sd, tangency_port$er, col = "red")
tangent_sharpe_ratio = (tangency_port$er - rf) / tangency_port$sd
abline(a=rf, b=tangent_sharpe_ratio)

#### 7.2.4 최적위험포트폴리오와 분리정리 ####
library(quantmod)
getSymbols("000660.KS", from = "2017-01-01")
getSymbols("005490.KS", from = "2017-01-01")

hyn <- `000660.KS`
pos <- `005490.KS`

hyn <- hyn$`000660.KS.Adjusted`
pos <- pos$`005490.KS.Adjusted`

r_hn <- returns(hyn)
r_ps <- returns(pos)
r_f <- rep(0.05/365, length(r_hn))
ret2 <- cbind(r_f, r_hn, r_ps)
ret2<-na.omit(ret2)
ans2 <- frontier(ret2) # frontier 에러
plot(ans2$stdev, ans2$ret, type='l')
lines(ans1$stdev, ans1$ret, col = 'red')
abline(v=0, col = 'blue')
abline(h=0, col = 'blue')

## 투자비중 ##

frontier_weights <- function(ret, target_rate){
  Q <- cov(ret)
  n <- ncol(ret)
  r <- colMeans(ret)
  Q1 <- rbind(Q, rep(1,n), r)
  Q2 <- cbind(Q1, rbind(t(tail(Q1,2)), matrix(0,2,2)))
  y <- head(solve(Q1, c(rep(0,n),1,target_rate)),n)
}

source("C:/Users/Shinhyunjin/Dropbox/data/portfolio.R")
x<-frontier_weights(ret2, 0.01/365)
x

frontier_risky_weight <- function(ret, r_f){
  Q <- cov(ret)
  n <- ncol(ret)
  r <- colMeans(ret)
  tmp <- solve(Q)%*%(r-rep(r_f,n))
  ans <- tmp/sum(tmp)
}
x <- frontier_risky_weight(ret2, 0.05/365)
x

### 7.2.5  베타 및 증권시장선 ###

library(fBasics)
require(fImport)
library(quantmod)

getSymbols("005930.KS", from = "2018-01-01")
getSymbols("069500.KS", from = "2018-01-01")

ss <- `005930.KS`
ks <- `069500.KS`
ss <- ss$`005930.KS.Adjusted`
ks <- ks$`069500.KS.Adjusted`

r.ss <- returns(ss) # 중요함 
r.ks <- returns(ks)

r.ks
r.ss

z <- merge(r.ss, r.ks,all=TRUE)
x <- na.omit(z)
#x <- data.frame(x)
x

#x <- drop(x[,1:2])
colnames(x) <- c("ss", "ks")
cr <- cov(x$rss, x$rks)
vr <- var(x$rks)
beta <- cr/vr
plot(x$ss, x$ks)
rg <- lm(x$rss ~ x$rks)
abline(rg, col='red')

beta
rg

### 7.2.6 Fama and French 3 Factor ###

fac <- read.csv('C:/Users/Shinhyunjin/Dropbox/data/fama_fac.csv')
ret <- read.csv('C:/Users/Shinhyunjin/Dropbox/data/fama_ret.csv')

colnames(fac) <- c("mon", "mkt-rf", "SMB", "HML", "RF")
colnames(ret) <- c("mon", "SL", "ME1", "SH", "BL", "ME2", "BH")

w<-merge(fac, ret, by="mon")
y <- w$SL
x1 <- w$`mkt-rf`
market_model <- lm(y~x1) #시장모형 
market_model
x2 <- w$SMB
x3 <- w$HML

ff3_model <- lm(y~x1+x2+x3) #FF모형 

source('ff.r')
summary(market_model)
summary(ff3_model)

### 7.2.7 Black_litterman Model ###

# 생략 #

#### 7.3 거래전략 ####

### 7.3.1Pair Trading ###
library(quantmod)
ss <- getSymbols("005930.KS", auto.assign = F)
hd <- getSymbols("005380.KS", auto.assign = F)

sTime <- "2017-01-01"
eTime <- "2019-02-03"
rangeT <- paste(sTime, "::", eTime, sep = "")
tss <- ss[,6][rangeT]
thd <- hd[,6][rangeT]
tss <- as.numeric(tss)
thd <- as.numeric(thd)
# 수익률과 헷지비율 
pdtss <- diff(tss)[-1]
pdthd <- diff(thd)[-1]
model <- lm(pdtss ~ pdthd - 1)
hr <- as.numeric(model$coefficients[1])

#그래프 : 상한보다 크면 ss short, hd long, 하한은 반대 

sp <- tss-hr*thd
meant <- as.numeric(mean(sp, na.rm=T))
sdt <- as.numeric(sd(sp, na.rm=T))
upbound <- meant + 1 *sdt
dnbound <- meant - 1 * sdt
plot(sp, main = "ss vs hd spread", type= 'l')
abline(h=meant, col='red', lwd = 2)
abline(h=upbound, col = 'blue', lwd=2)
abline(h=dnbound, col ='blue', lwd = 2)

hist(sp, col="blue", breaks = 100, main = "spread histogram")
abline(v=meant, col = "red", lwd = 2)

# 거래분석

splen <- length(sp)
pxbuy <- c(rep(NA, splen))
pxsell <- c(rep(NA, splen))
spx <- as.numeric(sp)
tradeQty <- 100
total_p <- 0
for(i in 1:splen){
  spTemp <- spx[i]
  if(spTemp < dnbound){
    if(total_p<=0){
      total_p <- total_p + tradeQty
      pxbuy[i] <- spTemp
    }
  } else if (spTemp > upbound){
    if(total_p >=0){
      total_p <- total_p - tradeQty
      pxsell[i] <- spTemp
    }
  }
}

plot(sp, main = "spread trade", type = 'l', lwd=1)
abline(h=meant, col='red', lwd = 2)
abline(h=upbound, col = 'blue', lwd=2)
abline(h=dnbound, col ='blue', lwd = 2)
points(c(pxbuy, index(sp)), col = 'green', cex=1.9, pch=19)
points(c(pxsell, index(sp)), col = 'red', cex=1.9, pch=19)  

### 7.3.2 PF 보험전략 ###

## CPPI ##

# fomula : Vt = pit * st + (vt-1 - pit-1*st-1)
# V : 투자금액, pi : 주식수
# pi  = (multiple * cusion(V-K)) / St

nt <- 180 ; nsim<-50
par(mfrow=c(1,2))
s <- matrix(0, ncol=nsim, nrow = nt)
s[1,] <-1
for(i in 2:nt){
  s[i,] <- s[i-1,]*0.99
}
matplot(s, type='l')

v0<-10;s0<-1;K<-8;m<-2.5
v <- matrix(0, ncol = nsim, nrow = nt)
phi <- matrix(0, ncol = nsim, nrow = nt)
v[1,] <- v0
phi[1,] <- m*(v[1,]-K)/s[1,]
for(i in 2:nt){
  Vstock <- phi[i-1,]*s[i,]
  Vbond <- v[i-1,] - phi[i-1,]*s[i-1,]
  v[i,] <- Vstock+Vbond
  phi[i,] <- m*(v[i,]-K)/s[i,]
}
matplot(v, type='l') # 0.8에서 더 안 줄어든다!

## 시뮬레이션 #

par(mfrow=c(1,2))
set.seed(3)
mu <- 0.04; vol <- 0.5; t<-0.5; nt <- 180; nsim<-50
dt<-t/nt
s <- matrix(0, ncol = nsim, nrow = nt)
s[1,] <- 1
for(i in 2:nt){
  w1 <- rnorm(nsim, 0, 1)
  s[i,] <- s[i-1,] * exp((mu-0.5*vol^2)*dt + vol*sqrt(dt)*w1)
  
}
matplot(s, type='l', main = "PF보험 전략미실행 시뮬레이션 ")

v0 <-10;s0<-1;K<-8;m<-2
v<-matrix(0, ncol = nsim, nrow = nt)
phi <- matrix(0, ncol = nsim, nrow = nt)
v[1,] <- v0
phi[1,] <- m*(v[1,]-K)/s[1,]
for(i in 2:nt){
  Vstock <- phi[i-1,]*s[i,]
  Vbond <- v[i-1,]-phi[i-1,]*s[i-1,]
  v[i,] <- Vstock+Vbond
  phi[i,] <- m*(v[i,]-K)/s[i,]
  
}
matplot(v, type='l', main = "PF 보험전략 시뮬레이션")

