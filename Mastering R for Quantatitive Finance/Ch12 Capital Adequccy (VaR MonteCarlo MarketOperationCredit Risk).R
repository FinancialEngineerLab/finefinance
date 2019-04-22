##### ch12. Capital Adeequacy #####

## 1. 분석적 VaR ##
install.packages("quantmod")
library(quantmod)

getSymbols("^KS11", from = "2015-01-01")
KS11 <- na.omit(KS11)

r <- log(head(KS11$KS11.Close,-1) / tail(KS11$KS11.Low,-1))
m <- mean(r)
s <- sd(r)
VaR1 <- -qnorm(0.05, m, s)
print(VaR1)
hist(KS11$KS11.Close)

## 2. 과거 VaR ##

VaR2 <- -quantile(r, 0.05)
print(VaR2) #하위 5%

## 3. 몬테카를로 ##

sim_norm_return <- rnorm(10000, m, s)
VaR3 <- -quantile(sim_norm_return, 0.05)
VaR3 <- -quantile(sim_norm_return)#shows total qunaitle
print(VaR3)

#몬테카를로 시뮬레이션
sim_return <- r[ceiling(runif(10000)*1049)] #정수 1~251 무작위 시뮬레이션
VaR4 <- -quantile(sim_return, 0.05)
print(VaR4)


##### 시장 리스크 market risk #####
install.packages("fOptions")
library(fOptions)

# stock vs call
X <- 100
Time <-2
r <- 0.05
sigma <- 0.22
mu <- 0.2
S <- seq(1,200, length = 1000)
call_price <- sapply(S, function(S) GBSOption("c", S, X, Time, r, r, sigma)@price)
plot(S, call_price, type = "l", ylab ="", main = "Call option price in function of
     stock prompt price")

### Option Portfolio with VaR ###
X <- 100 
Time <-2
r <- 0.05
sigma <- 0.22
mu <- 0.2
S <- seq(1,200, length = 1000)
#
call_price <- sapply(S, function(S) GBSOption("c", S, X, Time, r, r, sigma)@price)
put_price <- sapply(S, function(S) GBSOption("p", S, X, Time, r,r,sigma)@price)
portfolio_price <- call_price + put_price
#
plot(S, portfolio_price, type = "l", ylab = "",
     main = "Portfolio price in function of stock prompt price")

# Portfolio vaR Simulation #

p0 <- GBSOption("c", 100, X, Time, r, r, sigma)@price +
  GBSOption("p", 100, X, Time, r, r, sigma)@price
print(paste("price of portfolioL", p0))
#
S1 <- 100*exp(rnorm(10000, mu - sigma^2 /2, sigma))
P1 <- sapply(S1, function(S) GBSOption("c", S,X,1,r,r,sigma)@price+
               GBSOption("p", S,X,1,r,r,sigma)@price)
VaR <- quantile(P1, 0.05)
print(paste("95% VaR of portfolip: ", p0-VaR))
## 95% var : 8.05 
### => portfolio expected loss ; portfolio value - 8.05 ###
### 5%이상 확률로  portfolio expected loss 이상 손해 ###

##### Credit Risk #####

### expected loss = PD * LGD * EAD ###
## 자산가치 = 회사채 pv - 부도옵션 p + 주식 시장가치 c ##

kmv_error <- function(V_and_vol_V, E =3, Time =1, D = 10, vol_E = 0.8, r = 0.05){
  V <- V_and_vol_V[1]
  vol_V <- V_and_vol_V[2]
  E_ <- GBSOption("c", V, D, Time, r,r, vol_V)@price
  tmp <- vol_V*sqrt(Time)
  d1 <- log(V/(D*exp(-r*Time)))/tmp + tmp/2
  Nd1 <- pnorm(d1)
  vol_E_ <- Nd1*V/E*vol_V
  err <- c(E_ - E, vol_E_ - vol_E)
  err[1]^2 + err[2]^2
}

a <- optim(c(1,1), fn = kmv_error)
print(a)

##### Operation Risk #####

op <- function(){
  n <- rpois(1,20) #poisson dist
  z <- rlnorm(n,5,2) #lognormal dist
  sum(z)
}

Loss<-replicate(10000, op()) # 몬테카를로 10000번
hist(Loss[Loss<50000], main = "", breaks =20, xlab = "", ylab = "")
print(paste("Expected Loss= ", mean(Loss)))
print(paste("99.9 % qunatile of loss = ", quantile(Loss, 0.999)))
