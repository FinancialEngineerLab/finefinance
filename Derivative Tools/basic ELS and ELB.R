##### 구조화 상품 #####

### 8.5.1 ELS ####

# input data #
library(quantmod)
library(googledrive)
library(tidyverse)
library(tibble)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(HenryQuant)

getSymbols("^HSCE", from = "2017-03-01")
getSymbols("^STOXX50E", from = "2017-03-01")


HSCE<- HSCE$HSCE.Adjusted
HSCE <- HSCE/lag(HSCE,1)-1
STOXX50E<-STOXX50E$STOXX50E.Adjusted
STOXX50E<- STOXX50E/lag(STOXX50E,1)-1
data.df = merge(HSCE, STOXX50E) %>% na.omit() 
data.df
#data.df <- (data.df)/lag(data.df)-1

mu1 <- mean(data.df$HSCE.Adjusted)
mu2 <- mean(data.df$STOXX50E.Adjusted)
vol1 <- sd(data.df$HSCE.Adjusted)
vol2 <- sd(data.df$STOXX50E.Adjusted)
cor(data.df$HSCE.Adjusted,data.df$STOXX50E.Adjusted)
rho <- 0.3793738
r <- 0.0175
t <- 3
ntime <- 252*2
nsim <-10

KO <-110 #가정 
KI <- 65
coupon <- 0

dt <- t/ntime
nt <- ntime+1

s1 <- matrix(0, ncol = nsim, nrow = nt)
s2 <- matrix(0, ncol = nsim, nrow = nt)
s1[1,] <- 100
s2[1,] <- 100

set.seed(1)
z1 <- rnorm(nsim*nt)
z2 <- rnorm(nsim*nt)
w1 <- matrix(z1, nt, nsim)
w2 <- matrix(z2, nt, nsim)
ko.flag <- matrix(0,nt,nsim)
ki.flag <- matrix(0, nt, nsim)

for(i in 2:nt){
  s1[i,] <- s1[i-1,] * exp((mu1 - 0.5*vol1^2)*dt + vol1 * sqrt(dt)*w1[i-1,])
  z1 <- rho * w1[i-1,] + sqrt(1-rho^2)*w2[i-1,]
  s2[i,] <- s2[i-1,] * exp((mu1 - 0.5*vol2^2)*dt + vol2 * sqrt(dt)*z1)
  und <- pmin(s1[i,], s2[i,])
  print(KO < und)
  print(KI < und)
  
  if(sum(KO<und)>0) ko.flag[i,] <- KO < und
  if(sum(KI>und)>0) ko.flag[i,] <- KI > und
}

ret <- rep(0,nsim)
ko.count <-rep(0, nsim)

for(is in 1:nsim){
  check <- match(TRUE, ko.flag[,is])
  if(is.na(check)){ #no knockout
    check <- 9
    ki.check <- match(TRUE, ko.flag[,is])
    if(is.na(ki.check)) ret[is] <- (coupon*t+1)*exp(-r*t) #no knockin
    else ret[is] <- pmin(s1[nt,is],s1[nt,is])/100*exp(-r*t)
  }else{
    ko.time <- (check-1)*dt
    ret[is] <- (ko.time*coupon +1)*exp(-r*ko.time) # Knock in
  }
  ko.count[is] <- (check-1)*dt
}
ans <- mean(ret)
ans
par(mfrow=c(1,2))
tseq <- seq(0, 2*252/2,0.5)
matplot(tseq, pmin(s1,s2), type = 'b')
abline(h=KI,col='red')
abline(h=KO, col = 'blue')
tmp <- hist(ko.count, breaks=seq(0,3.5,0.5), right =F, xaxt='n')
axis(1, at = tmp$mids, labels = seq(0, 3,0.5))


### 삼성증권 ELB 제895회 Simulation and Valuation ###

getSymbols("^KS11",from ="2017-10-02")

KS11 <- KS11$KS11.Adjusted
KS11 <- na.fill(KS11,1)
KS11 <- KS11/lag(KS11,1)-1
KS11 <- na.fill(KS11,1)

mu <- mean(KS11)
vol <- sd(KS11)
r <- 0.0175
t <- 1.5
ntime <- (252+252/2)/5
nsim <- 10

KO <-115 #가정 
KI <- 85
coupon <- 0

dt <- t/ntime
nt <- ntime+1

s1 <- matrix(0, ncol = nsim, nrow = nt)
s1[1,] <- 100

set.seed(1)
z1 <- rnorm(nsim*nt)
w1 <- matrix(z1, nt, nsim)
ko.flag <- matrix(0,nt,nsim)
ki.flag <- matrix(0, nt, nsim)

for(i in 2:nt){
  s1[i,] <- s1[i-1,] * exp((mu - 0.5*vol^2)*dt + vol * sqrt(dt)*w1[i-1,])
  #z1 <- rho * w1[i-1,] + sqrt(1-rho^2)*w2[i-1,]
  und <- pmin(s1[i,])
  print(KO < und)
  print(KI < und)
  
  if(sum(KO<und)>0) ko.flag[i,] <- KO < und
  if(sum(KI>und)>0) ko.flag[i,] <- KI > und
}

ret <- rep(0,nsim)
ko.count <-rep(0, nsim)

for(is in 1:nsim){
  check <- match(TRUE, ko.flag[,is])
  if(is.na(check)){ #no knockout
    check <- ntime
    ki.check <- match(TRUE, ko.flag[,is])
    if(is.na(ki.check)) ret[is] <- (coupon*t+1)*exp(-r*t) #no knockin
    else ret[is] <- pmin(s1[nt,is],s1[nt,is])/100*exp(-r*t)
  }else{
    ko.time <- (check-1)*dt
    ret[is] <- (ko.time*coupon +1)*exp(-r*ko.time) # Knock in
  }
  ko.count[is] <- (check-1)*dt
}


#그래프 
par(mfrow=c(1,2))
tseq <- seq(0,ntime)
matplot(tseq, pmin(s1), type = 'b')
abline(h=KI,col='red')
abline(h=KO, col = 'blue')
tmp <- hist(ko.count, breaks=seq(0,3.5,0.5), right =F, xaxt='n')
axis(1, at = tmp$mids, labels = seq(0, 3,0.5))

#최종수익률
ans <- mean(ret)
ans

### Target Return Forward : 목표수익선물환 ###

#exposure 12,000,000
#contract exchange  : 1135

getSymbols("KRW=X",from="2017-01-01")
usdkrw <- `KRW=X`
usdkrw <- usdkrw$`KRW=X.Close`
usdkrw<-na.omit(usdkrw)
usdkrw_ret <- usdkrw/lag(usdkrw,1)-1
usdkrw_ret<-na.omit(usdkrw_ret)

#mean(usdkrw_ret)
usdkrw["2019-03-01"] #current  1124.22

rd <- 0.0265  # us
rf <- 0.0175
vol <- sd(usdkrw_ret)
t <- 2
ntime <- 24
nsim <- 10
t.profit <- 100 #/ 달러당#목표수익 설정 
K <- 1135
dt <- t/ntime
nt <- ntime +1
s <- matrix(0, ncol=nsim, nrow =nt)
s[1,] <- 1124.22
set.seed(1)

z<-rnorm(nt*nsim)
w <- matrix(z,nt,nsim)
profit <- matrix(0, nt, nsim)
pl <- matrix(0,nt,nsim)

# underlying asset matrix
for(i in 2:nt){
  s[i,] <- s[i-1,] * exp((rd-rf-0.5*vol^2)*dt + vol*sqrt(dt)*w[i-1,])
  pl[i,] <- K-s[i,]
  profit[i,] <- pmax(pl[i,],0)
}

cumprofit <- matrix(0,nt,nsim)
ret <- rep(0, nsim)

# cummulative return
for(is in 1:nsim){
  cumprofit[,is] <- cumsum(profit[,is])
  idx <- cumprofit[,is] <= t.profit
  times <- (1:nt)[idx]
  ret[is] <- sum(pl[idx,is]*exp(-rd*times*dt))
}
ans <- mean(ret)
ans

par(mfrow=c(1,2))
matplot(s, type ='b')
abline(h=K, col='red',lwd=2)
matplot(cumprofit, type='b')
abline(h=t.profit, col='red', lwd= 2)

