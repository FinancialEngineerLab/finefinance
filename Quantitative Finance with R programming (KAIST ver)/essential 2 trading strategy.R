
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
