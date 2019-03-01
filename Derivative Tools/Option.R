#### Option ####

### 7.5.1 Option Summary ###

f.call <- function(x)sapply(x, function(x) max(c(x-K,0)))
f.put <- function(x)sapply(x, function(x) max(c(K-x,0)))
K <- 100
curve(f.call, 50, 150, col = "blue", lty=1, lwd = 1,
      ylab = expression(f(x)), main = "Call-Put Payoff")
curve(f.put, 50, 150, col = "black", add = TRUE, lty=2,lwd=2)
legend("top", c("call","put"), lty = c(1,2),
       col = c("blue", "black"), lwd = c(2,2), bty = "n")


### 7.5.2 Black-Scholes ###

## 7.5.2.1 Call ##

bso <-function(s,k,r,q,v,t){
  d1 <- log(s/k) + (r-q + 0.5*v*v)*t
  d1 <- d1/(v*sqrt(t))
  d2 <- d1 - v*sqrt(t)
  nd1 <- pnorm(d1)
  nd2 <- pnorm(d2)
  ans <- s*exp(-q*t)*nd1 - k*exp(-r*t)*nd2
  return(ans)
}

bso(100,100, 0.04, 0,0.2,0.5)

# Gamma Graph
s<-100;r<-0.04;t<-1;q<-0
p <- function(v) bso(s,k,r,q,v,t)
k<-80
curve(p, 0,1,xlab=expression(sigma), ylab = expression(P[t]), ylim  =c(0,40))
k<-100
curve(p, 0,1,add=TRUE, lty=2)
k<-120
curve(p, 0,1, add=TRUE, lty=3)
legend("bottomright", c("K=80", "K=100", "K=120"), lty = 1:3, bty = "n") # 그래프 상자

# Theta Graph 
s <- 75:125
opt <- bso(s, 100, 0.04, 0, 0.2, 0.5)
plot(s, opt, type = "l", bty = "n")
t.seq <- seq(0,0.5,0.1)
for(t in t.seq){
  f <- function(x) bso(x,100,0.04, 0,0.2, t)
  curve(f, add= TRUE)
}

## Vanilla BS Calculator ##

bso <- function(s,k,r,q,v,t,putcall){
  d1 = log(s/k) + (r-q+0.5*v*v)*t
  d1 = d1 / (v*sqrt(t))
  d2 = d1 - v *sqrt(t)
  nd1 = pnorm(d1)
  nd2 = pnrom(d2)
  ans = s*exp(-q*t) * nd1 - k*exp(-r*t)*nd2
  if(putcall == 'put'){
    nd1 = pnorm(-d1)
    nd2 = pnrom(-d2)
    ans - k*exp(-r*t)*nd2 - s*exp(-q*t)*nd1
  }
  if(t<=0)
  {
    ans = max(s-k, 0)
    if(putcall =='put') ans=max(k-x,0)
  }
  return(ans)
}

## Greeks ###

# Delta 

bsoDelta <- function(s,k,r,q,v,t, putcall){
  ds = s/10000
  p0 = bso(s,k,r,q,v,t,putcall)
  p1 = bso(s+ds, k,r,q,v,t,putcall)
  ans = (p1-p0)/ds
  return(ans)
}

bsoGamma <- function(s,k,r,q,v,t,putcall){
  ds = s/10000
  p0 = bso(s,k,r,q,v,t, putcall)
  pu = bso(s+ds, k,r,q,v,t,putcall)
  pd = bso(s-ds, k,r,q,v,t,putcall)
  ans = (pu+pd-2*p0)/(ds^2)
  return(ans)
}

bsoVega <- function(s,k,r,q,v,t,putcall){
  dv = v/10000
  p0 = bso(s,k,r,q,v,t,putcall)
  pu = bso(s,k,r,q,v+dv,t,putcall)
  ans = (pu-p0)/dv
  return(ans)
}

bsoTheta <- function(s,k,r,q,v,t,putcall){
  dt = t/10000
  p0 = bso(s,k,r,q,v,t,putcall)
  pu = bso(s,k,r,q,v,t+dt, putcall)
  ans = -(pu-p0)/dt
  return(ans)
}

## 4. Delta -> Exercise: bisectional search ##

bsD2K <- function(delta, s, r, q,v,t,putcall){
  kd = s/100; ku = s*2
  for (i in 1:30)
  { km = (kd+ku)/2
    dm = bsoDelta(s,km,r,q,v,t,putcall)
    if (putcall =="call")
      if (dm>delta) kd <- km else ku<-km
    else
      if (abs(dm)>abs(delta)) ku<-km else kd<-km
  }
  return(km)
}


### Simulation ###
mu <-0.04
vol <-0.2
t<-0.5
nt <-10
nsim <- 100
dt <-t/nt
s <- matrix(0, ncol = nsim, nrow = nt)
s[1,] <- 100
set.seed(1)
for ( i in 2:nt){
  w1 <- rnorm(nsim,0,1)
  s[i,] <- s[i-1,]*exp((mu-0.5*vol^2)*dt + vol*sqrt(dt)*w1)
}
matplot(s, type='l')

### Simlulation Valueation ###

MCPrice <- function(s=1, t=1, r=1, sigma=1, nsim= 1000,f){
  u <- rnorm(nsim)
  st <- s*exp((r-0.5*sigma^2)*t + sigma*sqrt(t)*u)
  payoff <- sapply(st,f)
  mean(payoff)*exp(-r*t)
}

k<-100
f<-function(s) max(0, s-k)
set.seed(1)
nsim <- 10000
t <- 0.5
print(MCPrice(s=100, t=t, r=0.04, sigma=0.2, nsim=nsim, f=f))

### Antithetic Variable Method ###

MCPrice_Ant <- function(s=1, t=1,r=1, sigma=1, nsim=1000, f){
  u <- rnorm(nsim/2)
  st <- s*exp((r-0.5*sigma^2)*t + sigma*sqrt(t)*u)
  st <- c(st,s*exp((r-0.5*sigma^2)*t + sigma*sqrt(t)*(-u)))
  payoff <- sapply(st,f)
  mean(payoff)*exp(-r*t)
}

k<-100
f <- function(s) max(0,s-k)
set.seed(1)
nsim <- 10000
t<-0.5
print(MCPrice_Ant(s=100,t=t,r=0.04,sigma=0.2,nsim=nsim,f=f))

#

k <- 100
t <- 0.5
f<-function(s) max(0, s-k)
set.seed(1)
nsim <- seq(1000,100000,1000)
ans <- matrix(0, nrow=length(nsim), ncol = 3)
for (i in 1:length(nsim)){
  ans[i,1] <- MCPrice(s=100, t=t, r=0.04, sigma = 0.2, nsim = nsim[i], f=f)
  ans[i,2] <- MCPrice_Ant(s=100, t=t, r=0.04, sigma=0.2, nsim=nsim[i], f=f)
  ans[i,3] <- 6.627068
}
matplot(nsim, ans, type='l')
legend('topright', c("normal", "antithetic", "bs"), lty = 1:3, col = 1:3, bty = "n")

### Option Package ###

library(fOptions)
GBSOption("c", 100,100,0.5,0.04,0.04,0.2)@price

#GBSGreeks
#GBSCharacteristcs
#GBSVolatility

CRRBinomialTreeOption("ce", 900,950, 1/4, 0.02,0.02,0.22,3)@price
Crrtree <-BinomialTreeOption(TypeFlag="pa", 50,50,0.4167, 0.1, 0.1, 0.4, n=5)
BinomialTreePlot(Crrtree, dy=1, cex=0.8, ylim = c(-6,7), xlab= "n", ylab="Option Value")
title(main = "Put European Tree")

## Binomial & BS Convergence ##

prices <- sapply(1:200, function(n){CRRBinomialTreeOption("ce", 900,950,1/4,0.02,0.02,0.22,n)@price})
str(prices)
plot(1:200, prices, type='l')

### 7.5.5 Volatility ###

##1. implied volatility ##

library(fOptions)
imvol <- GBSVolatility(6.627068, "c", 100, 100,0.5,0.04,0.04)
imvol

## 2. Volaitility Smile ##

raw_data <- read.csv("C:/users/shinhyunjin/dropbox/data/kospi_option_190219.csv")
raw_data <- na.omit(raw_data)
nmb <- length(raw_data[,1])
nmb


k<-rep(NA,nmb)
imvol <- rep(NA,nmb)

library(quantmod)
getSymbols("^KS200")
KS200 <- KS200$KS200.Adjusted
KS200[1,1]

s<-285.9
t1 <- as.Date("2019-03-14")
t0 <- Sys.Date()
t<-as.numeric(t1-t0)/365
r <- 0.0175
q <-0
b <- r-q

for(i in 1:nmb){
  k[i] <- raw_data$癤풽xercise[i]
  ty <- "c"
  px <- raw_data$CALL[i]
  if(raw_data$癤풽xercise <=s){
    ty <- "p"
    px < raw_data$PUT[i]
  }
  imvol[i] <- GBSVolatility(px,ty,s,k[i],t,r,b)
}
plot(k, imvol, main = "KOSPI200 Option Volatility Smile")

### BS volaitliy
raw_data2 <- read.csv("C:/users/shinhyunjin/dropbox/data/kospi_option_2.csv")
raw_data2 <- na.omit(raw_data2)
nmb <- length(raw_data2[,1])
nmb
t<-rep(NA,nmb)
imvol <- rep(NA,nmb)

strike <- raw_data2[,1]
library(quantmod)
getSymbols("^KS200")
KS200 <- KS200$KS200.Adjusted
KS200[1,1]

s<-285.9
t0 <- Sys.Date()
r <- 0.0175
q <-0
b <- r-q

library(fOptions)
for(i in 1:nmb){
  t[i] <- as.numeric(as.Date(raw_data2$mat[i])-t0)/365
  ty <- "c"
  px <- raw_data2$price[i]
  imvol[i] <- GBSVolatility(px,ty,s,strike[i], t[i],r,b)
}
plot(t, imvol, main = "BS implied Volatility")




install.packages("quantmod")
library(quantmod)
require(quantmod)
ss <- getSymbols("005930.KS", auto.assign=F)
head(ss)
close <- ss[,"005930.KS.Close"]
x <- diff(close) /close[-length(close)]
plot(x)
head(x)

##### EWMA GARCH #####

require(fImport)
getSymbols("^KS11", from = "2013-01-01")
ksp_close <- KS11$KS11.Adjusted
ret <- diff(ksp_close)/close[-length(ksp_close)]
ret <- na.omit(ret)
par(mfrow=c(1,2))
plot(ksp_close,  xlab = "Time", ylab = "KOSPI200", main = "KOSPI200 index")
plot(ret, ylab = "return of kospi200")
chartSeries(ret, xlab = "Time", ylab = "KOSPI200")

### MLE EWMA ###
mle_ewma <- function(lambda){
  nmb <- length(ret)
  logL <- rep(0, nmb)
  var <- var(0, nmb)
  var[1] <- ret[1]^2
  for(i in 2:nmb){
    var[i] <- lambda * var[i-1] + (1-lambda)*ret[i-1]^2
    logL[i] <- (-log(var[i]) - ret[i]^2/var[i])
  }
  sum(logL)
}

ans <- optimize(mle_ewma, lower=0, upper=1, maximum = TRUE)
ans$maximum ## mle value
ans$objective

lam_seq <- seq(0.7, 0.9, length.out = 10)
y <- sapply(lam_seq, mle_ewma)
plot(lam_seq, y, ylab = "loglike", xlab = "lambda", type= 'l')


### MLE GARCH ###

neg_mle_garch <- function(x){
  nmb <- length(ret)
  logL <- rep(0, nmb)
  var <- var(0, nmb)
  var[1] <- ret[1]^2
  omega <- x[1]
  beta <- x[2]
  alpha <- x[3]
  for(i in 2:nmb){
    var[i] <- omega+beta*var[i-1]+alpha*ret[i-1]^2
    logL[i] <- (-log(var[i])- ret[i]^2/var[i])
  }
  -sum(logL)
}

ans <- optim(c(0.1,0.1, 0.1),neg_mle_garch)
ans$par
ans$vlaue
ans$counts
ans$convergence
ans$message


#### 7.2.6 Dynamic Delta Hecge ####


## s : 290, r = 0.0175, d = 0, return  : 12, sigma = 20%, reblance = 1day, k : 275

bso <- function(s,k,r,q,v,t){
  d1 <- log(s/k)+(r-q+0.5*v*v)*t
  d1 <- d1/(v*sqrt(t))
  d2 <- d1 - v*sqrt(t)
  nd1 <- pnorm(d1)
  nd2 <- pnorm(d2)
  ans <- s*exp(-q*t)*nd1 - k*exp(-r*t)*nd2
  return(ans)
}

bsoDelta <- function(s,k,r,q,v,t){
  ds = s/10000
  p0 = bso(s,k,r,q,v,t)
  p1 = bso(s+ds, k,r,q,v,t)
  ans = (p1-p0)/ds
  return(ans)
}

s0 <- 290 ; r<-0.0175 ; q<-0; t<-30/365; k<-s0; nt <- 30
nsim <-500
mu <- 0.12
vol <- 0.2
dt <- t/nt
s <- matrix(0, ncol = nsim, nrow = nt)
V <- matrix(0, ncol = nsim, nrow = nt)
dopt <- matrix(0, ncol = nsim, nrow = nt)
dund <- matrix(0, ncol = nsim, nrow = nt)

s[1,] <- s0
delta <- bsoDelta(s0,k,r,q,0.25, t)
cash <- bso(s0,k,r,q,vol,t) - delta*s[1,]
V[1,] <- -bso(s0,k,r,q,vol,t) + delta*s[1,]+cash
set.seed(1)
vol_real <- 0.25
for(i in 2:nt){
  t <- t-dt
  w1 <- rnorm(nsim,0,1)
  s[i,] <- s[i-1,]*exp((mu-0.5*vol_real^2)*dt + vol_real*sqrt(dt)*w1)
  dopt[i,] <- bso(s[i,],k,r,q,vol,t) - bso(s[i-1,],k,r,q,vol,t)
  dund[i,] <- delta*(s[i,]-s[i-1,])*(-1)
  V[i,] <- V[i-1,]  - dopt[i,]-dund[i,]
}

#시뮬레이션 결과 
par(mfrow=c(1,2))
matplot(s, type ='l')
plot(dund + dopt, type = 'l', ylim = c(-3,3), lwd = 2)
lines(dund,col  = 'blue')
lines(dopt, col = 'red')
abline(h=0, lty = 2)
#왼쪽은 주식가격 시뮬레이션
#오른쪽은 손익(red : option payoff, blakc : total, blue : underlying)

#오차시뮬레이션
par(mfrow = c(1,2))
matplot(s, type ='l')
hist(V[nt,], xlab = "", main = "")
xmean <- mean(V[nt,])
abline(v= xmean, lty = 2)
text(xmean, 100, paste("mean = ", format(xmean, digit =5, nsmall=4)), pos=4)

## pf가치 = theta * delta t + 1/2 * gamma*(delta S)^2 :
# 실제 변동성이 예상보다 크다면 pf가치가 감소함.
#(volatilty smile)