#### Shin's Trading Book ####

## Author : Shin Hyunjin ##
## KAIST Business School
## Derivative Trader

#### I. Fixed Income and Rates Derivatives ####

### Contents ##

### 1. Bond ###
## Bond Price
## Duration and Convexity

### 2. Swap ###
## IRS (Interest rates Swap)
## FSR (Forward Swap Rate)

### 3. Interest Structural Moodel ###
## Bond Price with Engineering Method
## Interest Options 
## caplet
## Swaption
## Calibration
## Bermudan Swaption

#------------------------------------------------------------------------------------#

library(RQuantLib)

## 1. Bond ##

### Bond Price ###
bond.price <- function(T,c,y,m=2){
  r <- 1/(1+y/m)
  n <- T*m
  dfs <- r^(1:n)
  cfs <- c(rep(c/m, n-1), c/m+1)
  ans <- sum(dfs*cfs)
}

a <- bond.price(3,2.5/100, 1.8/100)

bond_price <- function(T,c,y,m=2){
  x <- 1+y/m
  n <- T*m
  ans <- c/y*(1-x^-n)+x^-n
}
yseq <- seq(0, 0.2, length.out=100)
yseq

price <- c()
b<- bond_price(5,0.05, 0.1, 0.1)
b
for (i in yseq){
  price2 <- bond_price(5,0.05,0.1, m=i)
  price <- c(price, price2)
}
price
plot(yseq, price, type='l')

### Duration and Convexity ###

bond_price <- function(T,c,y,m=2){
  x <- 1+y/m
  n <- T*m
  ans <- c/y*(1-x^-n)+x^-n
}
duration <- function(T,c,y, m =2){
  nt <- T*m
  p <- bond_price(T,c,y)
  tmp <- sum((1:nt)/m*c/m*1/(1+y/m)^(1:nt))+T/(1+y/m)^nt
  ans <- tmp/p
}

md <- function(T,c,y,m=2) duration(T,c,y,m=2)/(1+y/m)
p1 <- function(y) 1-md(T,c=0.1,y)*(y-0.1)

c = 0.1
yseq <- seq(0.05, 0.4, length.out=100)
price <- bond_price(10,c=0.1, yseq)
plot(yseq, price, type= 'l')
curve(p1, add=TRUE, col = 'red')
abline(v=0.1, lty =2)
abline(h=1.0, lty = 2)
abline(v=0.2, lty =2,col='blue')
abline(h=p1(0.2), lty=2, col='blue')
abline(h=bond_price(T,c,0.2), lty=2, col='blue')


#------------------------------------------------------------------------------------#

## 1. Swap ##

### IRS ###

# data : bloomberg, kmbco(한국자금중개) #

irs_data <- read.csv('c:/users/shinhyunjin/dropbox/data/irs_data.csv')
irs_data

colnames(irs_data) <-c('term', 'rate')
irs_data$rate <- irs_data$rate /100

term <- irs_data$term
rate <- irs_data$rate
f <- approxfun(term, rate)

# discount factor
tenor.set <- seq(0, 20, 0.25)
df <- NULL
df[1] <- 1
for (i in 2:length(tenor.set)){
  irs_rate <- f(i*0.25-0.25)
  if(i==2) bunja <- 1
  else bunja <- 1-sum(df[2:(i-1)]) * 0.25*irs_rate
  df[i] <- bunja / (1+irs_rate*0.25)
}

dfx <- approxfun(tenor.set, df)

plot(tenor.set, df, xlab= "year", ylab = "discount factor", ylim = c(dfx(20),1))

# Valuation 
swap.value<- function(Tswap, fix_rate, payrec = 1, effective = 0){

  # dates
  payments <- seq(effective + 0.25, Tswap, 0.25)
  
  # fixed coupon
  pv_fix_leg <- sum(dfx(payments)*fix_rate*0.25)
  
  # float coupon
  if(effective==0) pv_float_leg <- 1- dfx(Tswap)
  else pv_float_leg <- dfx(effective) - dfx(Tswap)
  
  # pricing results
  ans <- (pv_fix_leg - pv_float_leg)*payrec
  return(ans)
  # fixed coupon
}

x <- swap.value(4, 1.9525/100)*10000
y <- swap.value(4, 1.9625/100)*1000
delta <- x-y
delta

# Tip : float bond's duration => close to 0, fixed bond's duration = swap's duration

### FSR (Forward Swap Rate) ###
calc.fsr <- function(Tfwd, Tswap){
  tset <- seq(Tfwd + 0.25, Tfwd + Tswap, 0.25)
  bunja = 0
  bunmo = 0
  
  for (t in tset){
    d0 = dfs(t- 0.25)
    d1 = dfs(t)
    
    frate = (d0/d1-1)*4
    
    bunja = bunja+0.25*frate*dfs(t)
    bunmo = bunmo + 0.25*dfs(t)
  }
  ans= bunja/bunmo
  return(ans)
}
  
####


### 8.3.1 Cap and Floor ###

#선도r, 행사r, rf, sigma, mat, 이자계산날짜,1년일수
caplet <- function(f,k,r,v,t,days,basis){
  d1 <- (log(f/k)+0.5*v*v*t)/(v*sqrt(t))
  d2 <- d1-v*sqrt(t)
  nd1 <- pnorm(d1)
  nd2 <- pnorm(d2)
  tmp <- exp(-r*t)*(f*nd1-k*nd2)
  ans <- days/basis/(1+f*days/basis)*tmp
  return(ans)
}

x <- caplet(0.08, 0.08, 0.07, 0.28,0.5,182,360)
x*10000000 #FV


### 8.3.2 Swaption ###

swaption.price <- function(rate,f,payrec,Topt,Tswap,strike,v,m=4){
  part.a <- (1-1/(1+f/m)^(Tswap*m))/f
  d1 <- (log(f/strike)+v*v/2*Topt)/(v*sqrt(Topt))
  d2 <- d1-v*sqrt(Topt)
  if (payrec == "pay"){
    nd1 <- pnorm(d1)
    nd2 <- pnorm(d2)
    part.b <- f*nd1- strike*nd2
  }
  else{
    nd1 <- pnorm(-d1)
    nd2 <- pnorm(-d2)
    part.b <- -f*nd1 + strike*nd2
  }
  disc <- exp(-rate*Topt)
  ans <- disc*part.a*part.b
  return(ans)
}

swaption.delta <- function(rate,f,payrec,Topt,Tswap,strike,v,m=4){
  a <- swaption.price(rate,f,payrec,Topt,Tswap,strike,v,m=4)
  b <- swaption.price(rate,f+0.0001, payrec,Topt, Tswap, strike, v,m=4)
  delta <- (a-b)/(0.0001)
  ans <- delta
  return(ans)
}
swaption.delta(0.05,0.05,"rec", 1,3,x,0.05)
#curve(swaption.delta(0.05,0.05,"rec",1,3,x,0.05))
x <- seq(0.01, 0.1, 0.01)
curve(swaption.price(0.05, 0.05, "rec", 1,3,x,0.05),0.01,0.1)
points(x, swaption.price(0.05,0.05,"rec", 1,3,x,0.05))

#### 8.4 이자율 옵션 ####

### 8.4.2 채권 시뮬레이션 ###

r0 <- 0.05
phi.t<-0.05
a <-0.02
v<-0.03
nt <-11
ns <- 10

set.seed(1)
z <- rnorm(nt*ns)
w <- matrix(z, nt,ns)
t <-1
dt <- t/(nt-1)
t.seq <- seq(0,t,dt) 

r <- matrix(NA,nt,ns) #시간행, 주가열 
r[1,] <- r0
for(it in 2:nt)
  r[it,] <- r[it-1,] + a*(phi.t -r[it-1,])*dt + v*w[it,]*sqrt(dt)
par(mfrow = c(1,2))
matplot(t.seq, r, type ='l', main = "Shortterm interest rate simulation")

p <- matrix(0,nt,ns)
p[nt,] <-1
for(it in (nt-1):1) p[it,] <- p[it+1,]/(1+r[it,]*dt)
matplot(t.seq, p, type = 'l', main = "Fixed Income Price Simulation")
px <- mean(p[1,])

## 다른 방법 ##

zcb <- function(t1, t2, p1,p2,a,sigma, fmt, rt){
  B <- 1/a*(1-exp(-a*(t2-t1)))
  A <- p2/p1*exp(B*fmt-sigma^2/(4*a)*(1-exp(-2*a*t1))*B^2)
  ans <- A*exp(-B*rt)
}

rt <- 0.05
fmt <- 0.05
a <- 0.02
sigma <- 0.03
p2 <- exp(-0.05*1)
p1 <- 1
x <- zcb(0,1,p1,p2,a,sigma,fmt,rt)
x #위에 일치

## Discount Fixed Income  Option ###

p1 <- 1
p2 <- exp(-0.05*1)
Tbond <- 1
Topt <- 0.5
L <- 1
K <- 0.95
a <- 0.02
t2 <- Tbond
t1 <- Topt
sigma <- 0.03

vp <- sigma/a*(1-exp(-a*(t2-t1)))*sqrt((1-exp(-2*a*t1))/(2*a))
h <- 1/vp*log(L*p2/(p1*K))+vp/2
ans <- L*p2*pnorm(h)-  K*p1*pnorm(h-vp)
ans

### Simulation for valuation ###

## call ##

r0 <- 0.05
phi.t <- 0.05
a <- 0.02
v <- 0.03
nt <- 11
ns <- 100
set.seed(1)
z <- rnorm(nt*ns)
w <- matrix(z,nt,ns)
t <- 0.5
fmt <- 0.05
dt <- t/(nt-1)
t.seq <- seq(0,t,dt)
r <- matrix(NA, nt, ns)
r[1,] <- r0

## floorlet : Fixed Income Call option ##
for(it in 2:nt){
  r[it,] <- r[it-1,] + a*(phi.t-r[it-1,])*dt + v*w[it,]*sqrt(dt)}

zcb <- function(t1,t2,p1,p2,a,sigma,fmt,rt){
  B <- 1/a*(1-exp(-a*(t2-t1)))
  A <- p2/p1*exp(B*fmt-sigma^2/(4*a)*(1-exp(-2*a*t1))*B^2)
  ans <- A*exp(-B*rt)
}

zcb.dist <- zcb(t1,t2,p1,p2,a,sigma,fmt,r[nt,])
ans <- mean(pmax(zcb.dist-K,0))*p1
print(ans)

hist(zcb.dist, main = "zcb dist")
abline(v=0.95, col = 'red')

# Caplet : Fixed Income Put
caplet <- function(p1, p2, Topt, Tbond, L,K){
  t2 <- Tbond
  t1 <- Topt
  vp <- sigma/2*(1-exp(-a*(t2-t1)))*sqrt((1-exp(-2*a*t1))/(2*a))
  h <- 1/vp*log(L*p2/(p1*K))+vp/2
  ans <- K*p1*pnorm(-h+vp)-L*p2*pnorm(-h)
}
a <- caplet(p1,p2,Topt,Tbond,L,K)
a

#### European Swaption ####

PPut <- function(p1, p2, Topt, Tbond, L,K){
  t2 <- Tbond
  t1 <- Topt
  vp <- sigma/a*(1-exp(-a*(t2-t1)))*sqrt((1-exp(-2*a*t1))/(2*a))
  h <- 1/vp*log(L*p2/(p1*K))+vp/2
  ans <- K*p1*pnorm(-h+vp)-L*p2*pnorm(-h)
}
B <- function(t1,t2) 1/a*(1-exp(-a*(t2-t1)))
A <- function(t1,t2,p1,p2){
  p2/p1*exp(B(t1,t2)*fmt-sigma^2/(4*a)*(1-exp(-2*a*t1))*B(t1,t2)^2)}

## r star calculation by Newton method
rt <- 0.05
fmt <- 0.05
a <- 0.02
sigma <- 0.03
Topt <- 3
Tswap <- 1
K <- 0.05

f <- function(r){
  tmp <- rep(0,  Tswap*4)
  for ( i in 1:(Tswap*4)){
    wi <- K*0.25
    if(i==Tswap*4) wi <- 1+K*0.25
    t1 <- Topt
    t2 <- Topt+i*0.25
    p1 <- exp(-0.05*t1)
    p2 <- exp(-0.05*t2)
    tmp[i] <- wi*A(t1,t2,p1,p2)*exp(-B(t1,t2)*r)
  }
  ans <-(sum(tmp)-1)^2
}

ans.optim <- optim(0,f,method= "SANN")
r.star <- ans.optim$par

## Valuation of European Swaption ##

tmp <-rep(0, Tswap*4)
for(i in 1:(Tswap*4)){
  wi <- K*0.25
  if(i == Tswap*4) wi <- 1+K*0.25
  t1 <- Topt
  t2 <- Topt+i*0.25
  p1 <- exp(-0.05*t1)
  p2 <- exp(-0.05*t2)
  Xi <- A(t1,t2,p1,p2)*exp(-B(t1,t2)*r.star)
  tmp[i] <- wi*PPut(p1,p2,t1,t2,1,Xi)
}
Ppay <- sum(tmp)
Ppay  

## Valuation of European Swaption by Simulation ##

p.swap <- function(rt,Teff, Tswap, fix,payrec){
  t1 <- Teff
  t2 <- Teff+Tswap
  p1 <- exp(-0.05*t1)
  p2 <- exp(-0.05*t2)
  floatleg <- 1-zcb(t1,t2,p1,p2,a,v,fmt,rt)
  fixedleg <- 0
  for(i in 1:(Tswap*4)){
    t1 <- Teff
    t2 <- Teff+i/4
    p1 <- exp(-0.05*t1)
    p2 <- exp(-0.05*t2)
    df <- zcb(t1,t2,p1,p2,a,v,fmt,rt)
    fixedleg <- fixedleg + df*fix/4
  }
  ans <- (fixedleg-floatleg)*payrec
}

r0 <- 0.05
phi.t <- 0.05
a <- 0.02
v <- 0.03
fmt <- 0.05
Tswap <- 1
Topt <- 3
ns <- 100
nt <- Topt*4
set.seed(1)
z <- rnorm(nt*ns)
w <- matrix(z,nt,ns)
dt <- 0.25
r <- matrix(NA,nt,ns)

fix <- 0.05
payrec <- (-1)

for(it in 1:nt){
  if (it ==1) r_prev <- r0
  else r_prev <- r[it-1,]
  r[it,] <- r_prev + a*(phi.t-r_prev)*dt + v*w[it,]*sqrt(dt)
}

sdf <- rep(0, ns)
for(is in 1:ns){
  f <- function(x) p.swap(x,Topt,Tswap, fix, payrec)
  cf_no_opt <- sapply(r[it,],f)
  cf_opt <- pmax(cf_no_opt,0) ## early exercise
  sdf <- prod(exp(-r[1:it,is]*dt)) # stochastic discount factor
}
ans <- mean(cf_opt*sdf)
ans

par(mfrow=c(1,2))
hist(cf_no_opt)
hist(cf_opt)


### Option price and Exercise ###

Pswaption <- function(K){
  f <- function(r){
    tmp <- rep(0,Tswap*4)
    for(i in 1:(Tswap*4)){
      wi <- K*0.25
      if(i==Tswap*4) wi <- 1+K*0.25
      t1 <- Topt
      t2 <- Topt+i*0.25
      p1 <- exp(-0.05*t1)
      p2 <- exp(-0.05*t2)
      tmp[i] <- wi*A(t1,t2,p1,p2)*exp(-B(t1,t2)*r)
    }
    ans <- (sum(tmp)-1)^2
  }
  set.seed(1)
  ans.optim <- optim(0.1, f, method = "SANN")
  r.star <- ans.optim$par
  tmp <- rep(0, Tswap*4)
  for(i in 1:(Tswap*4)){
    wi <- K*0.25
    if(i==Tswap*4) wi <- 1+K*0.25
    t1 <- Topt
    t2 <- Topt + i*0.25
    p1 <- exp(-0.05*t1)
    p2 <- exp(-0.05*t2)
    Xi <- A(t1,t2,p1,p2)*exp(-B(t1,t2)*r.star)
    tmp[i] <- wi*PPut(p1,p2,t1,t2,1,Xi)
  }
  Ppay <- sum(tmp)
}

kseq <- seq(0, 0.2, 0.01)
y <- rep(0, length(kseq))
for(i in 1:length(kseq)){
  y[i] <- Pswaption(kseq[i])
  print(c(kseq[i], y[i]))
}

plot(y, ylab = "Price", xlab= "K")

### Calibration ####

## Black Formula ##
swaption.price <- function(payrec, Topt, Tswap, strike, v,fwd,r){
  m <- 4
  part.a <- (1-1/(1+fwd/m)^(Tswap*m))/fwd
  d1 <- (log(fwd/strike)+v*v/2*Topt)/(v*sqrt(Topt))
  d2 <- d1 - v*sqrt(Topt)
  if (payrec == "pay"){
    nd1 <- pnorm(d1)
    nd2 <- pnorm(d2)
    part.b <- fwd*nd1 - strike*nd2
  }
  else{
    nd1 <- pnorm(-d1)
    nd2 <- pnorm(-d2)
    part.b<- -fwd*nd1 + strike*nd2
  }
  
  disc <- exp(-r*Topt)
  ans <- disc*part.a*part.b
  return(ans)
}

PPut <- function(p1,p2,Topt, Tbond, L, K, a,sigma){
  t2 <- Tbond
  t1 <- Topt
  vp <- sigma/a*(1-exp(-a*(t2-t1)))*sqrt((1-exp(-2*a*t1))/(2*a))
  h <- 1/vp*log(L*p2/(p1*K))+vp/2
  ans <- K*p1*pnorm(-h+vp)-L*p2*pnorm(-h)
}
B <- function(t1,t2,a) 1/a*(1-exp(-a*(t2-t1)))
A <- function(t1,t2,p1,p2,a,sigma){
  p2/p1*exp(B(t1,t2,a)*fmt
            -sigma^2/(4*a)*(1-exp(-2*a*t1))*B(t1,t2,a)^2)}

Pswaption <- function(Topt, Tswap, K, a, sigma){
  f <- function(r){
    tmp <- rep(0, Tswap*4)
    for(i in 1:(Tswap*4)){
      wi <- K*0.25
      if(i == Tswap*4) wi <- 1+K*0.25
      t1 <- Topt
      t2 <- Topt + i*0.25
      p1 <- exp(-0.05*t1)
      p2 <- exp(-0.05*t2)
      tmp[i] <- wi*A(t1,t2,p1,p2,a,sigma)*exp(-B(t1,t2,a)*r)
    }
    ans <- (sum(tmp)-1)^2
  }
  set.seed(1)
  ans.optim <- optim(0.1, f, method= "SANN")
  r.star <- ans.optim$par
  tmp <- rep(0, Tswap*4)
  
  for(i in 1:(Tswap*4)){
    wi <- K*0.25
    if(i == Tswap*4) wi <- 1+K*0.25
    t1 <- Topt
    t2 <- Topt + i*0.25
    p1 <- exp(-0.05*t1)
    p2 <- exp(-0.05*t2)
    Xi <- A(t1,t2,p1,p2,a,sigma)*exp(-B(t1,t2,a)*r.star)
    tmp[i] <- wi*PPut(p1,p2,t1,t2,1,Xi,a,sigma)
  }
  Ppay <- sum(tmp)
}

fmt <- 0.05
vol <- c(0.1,0.15,0.2,0.25)
Topt <- c(1,2,3,4)
Tswap <- c(4,3,2,1)
nmb <- length(vol)
tmp<- rep(0, nmb)
calib <- function(x){
  for(i in 1:nmb){
    p_market <- swaption.price("pay", Topt[i], Tswap[i], 0.05, vol[i], 0.05, 0.05)
    p_model <- Pswaption(Topt[i], Tswap[i], 0.05, x[1],x[2])
    tmp[i] <- (p_market-p_model)^2
  }
  ans <- sum(tmp)
}

para <- optim(c(1,1),calib)
para

#### 8.4.6 Bermudan Swaption ####

zcb <- function(t1,t2,p1,p2,a,sigma,fmt,rt){
  B <- 1/a*(1-exp(-a*(t2-t1)))
  A <- p2/p1 * exp(B*fmt-sigma^2/(4*a)*(1-exp(-2*a*t1))*B^2)
  ans <- A*exp(-B*rt)
}

p.swap <- function(rt, Teff, Tswap, fix, payrec){
  t1 <- Teff
  t2 <- Teff + Tswap
  p1 <- exp(-0.05*t1)
  p2 <- exp(-0.05*t2)
  floatleg <- 1-zcb(t1,t2,p1,p2,a,v,fmt,rt)
  fixedleg <- 0
  for(i in 1:(Tswap*4)){
    t1 <- Teff
    t2 <- Teff + i/4
    p1 <- exp(-0.05*t1)
    p2 <- exp(-0.05*t2)
    df <- zcb(t1,t2,p1,p2,a,v,fmt,rt)
    fixedleg <- fixedleg + df*fix/4
  }
  ans <- (fixedleg - floatleg)*payrec
}

#시뮬레이션을 위한 이자율 난수 생성 #

r0 <- 0.05
phi.t <- 0.05
a <- 0.02
v <- 0.03
fmt <- 0.05
Tswap <- 3
Topt <- seq(0.25, 2.75,0.25)

ns <- 10
nt <- length(Topt)
set.seed(1)
z <- rnorm(nt * ns)
w <- matrix(z, nt, ns)
dt <- Topt[2] - Topt[1]
r <- matrix(NA,nt,ns)

for(it in 1:nt){
  if(it==1) r_prev <- r0
  else r_prev <- r[it-1,]
  r[it,] <- r_prev + a*(phi.t-r_prev)*dt + v*w[it,]*sqrt(dt)
}

fix <- 0.05
payrec <- (-1)

cf_next <- NULL

#조기행사반영 + 시뮤레이션을 버뮤다 스왑션 가격 ###

m.ex.rule <- matrix(0,nt,ns)
m.cf_next <- matrix(0,nt,ns)
m.cf_no_opt <- matrix(0,nt,ns)

for(it in nt:1){
  Teff <- Topt[it]
  Tswap_remain <- Tswap - Teff
  f <- function(x) p.swap(x,Teff,Tswap_remain, fix, payrec)
  cf_no_opt <- sapply(r[it,],f)
  m.cf_no_opt[it,] <- cf_no_opt
  
  cf_opt <- pmax(cf_no_opt, 0) # 조기행사
  if(it == length(Topt)) cf_next<- cf_opt #ITM 무조건 행사 
  if(it!=length(Topt)){ # 최종 옵션행사시점 아니라면 행사 여부 비교 
    idx <- (1:ns)[cf_opt>0]
    y <- cf_next[idx]*exp(-r[it,idx]*dt)
    x1 <- r[it,idx]
    x2 <- x1*x1
    if(length(idx)>=3) coef<-lm(y~x1+x2)$coefficients #최소 3개 이상일때 회귀분석 
    else coef <- c(1,1,1)
    cont.est <- coef[1] + coef[2]*x1 + coef[3]*x2
    ex.rule <- rep(0,ns)
    ex.rule[idx] <- (cf_opt[idx] > cont.est)
    cf_next <- (ex.rule==0) * cf_next*exp(-r[it,]*dt) +(ex.rule==1)*cf_opt
    #옵션 미행사시 다음기 가치, 옵션행사시 행사가치 
    m.ex.rule[it,] <- ex.rule
    m.cf_next[it,] <- cf_next
  }
}

ans <- mean(cf_next)
ans

par(mfrow = c(1,2))
t.seq <- seq(0,Topt[nt],dt)
rates <- rbind(r0*rep(1,ns),r)
matplot(t.seq,rates, type= 'b')
price <- rbind(ans*rep(1,ns),m.cf_next)
matplot(t.seq, price, type = 'b')

for(is in 1:ns){
  idx <- (1:nt)[m.ex.rule[,is] ==1]
  points(idx, m.cf_next[idx,is], lwd=2)
}