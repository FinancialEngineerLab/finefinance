
#----------------------------------------#
###### Simulation Methods for Fiance ####
#----------------------------------------#

## Author : Shin Hyunjin (KAIST)
## Main Reference : Rpubs / R Lecture notes from KAIST Mgt Engineering

## Contents

# 1. American Option

# 2. Binary Option

# 3. Barrier Option
# 3-1 Down and out Call
# 3-2 Down and in Call
# 3-3 Up and in Call
# 3-4 Up and out Call
# 3-5 Down and out Put
# 3-6 Down and in Put
# 3-7 Up and in Put
# 3-8 Up and out Put

# 4. Double Rebate Option

# 5. Asian Option
# 5-1 Asian Call
# 5-2 Asian Put

#--------------------------------------------------------------------------------#

library(fExoticOptions)
library(RQuantLib)

# 1. American Option #

#### LSMC American Option ####
#input
s0 <-280; K <-270; r <- 0.0175 ; sigma <- 0.2 ; nt.tmp <- 10; ns<-10;t<-0.5
set.seed(2) 
nt <- nt.tmp +1 # index starts at 1
u <- rnorm(ns*nt)

s <- matrix(NA,nt,ns)
w <- matrix(u,nt,ns)
m.ex.rule <- matrix(0,nt,ns)

dt <- t/nt.tmp
s[1,] <- s0

#


for(it in 2:nt){
  s[it,] <- s[it-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[it,])
}

cf.next <- pmax(K-s[nt,],0)

for(it in (nt-1):1){
  cf.ee<-pmax(K-s[it,],0)
  idx<-(1:ns)[cf.ee>0]
  y <- cf.next[idx]*exp(-r*dt)
  x1 <- s[it, idx]
  x2 <- x1*x1
  if(length(idx)>=3) a<- lm(y~x1+x2)$coefficients
  else a <- c(1,1,1)
  cont.est <- a[1] + a[2]*x1 + a[3]*x2
  ex.rule <- rep(0,ns)
  ex.rule[idx] <- (cf.ee[idx] > cont.est)
  cf.next <- (ex.rule==0)*cf.next*exp(-r*dt) + (ex.rule ==1)*cf.ee
  m.ex.rule[it,] <- ex.rule
}

p.am <- mean(cf.next)
p.eu <- mean(pmax(K-s[nt,],0)*exp(-r*t))
p.am
p.eu

matplot(s, type ='l')
for(is in 1:ns){
  idx <- (1:nt)[m.ex.rule[,is]==1]
  points(idx, s[idx,is], lwd =2)
}
abline(h=K, col = 'red', lty = 2, lwd =2)


#### LSMC American Put Option ####

am.put.sim = function(t=0.5, s0=100, r=0.05, sigma = 0.2, K=95, ns = 10, nt.tmp = 10){
  set.seed(2)
  nt <- nt.tmp +1 # index starts at 1
  u <- rnorm(ns*nt)
  
  s <- matrix(NA, nt, ns)
  w <- matrix(u, nt, ns)
  
  dt <- t/nt.tmp
  s[1,] <- s0
  for(it in 2:nt){
    s[it,] <- s[it-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[it,])
  }
  cf.next <- pmax(K-s[nt,],0)
  for(it in (nt-1):1){
    cf.ee <- pmax(K-s[it,],0)
    idx <- (1:ns)[cf.ee>0]
    y <- cf.next[idx]*exp(-r*dt)
    x1 <- s[it,idx]
    x2 <- x1*x1
    if(length(idx)>=3) a<- lm(y ~ x1+x2)$coefficients
    else a=c(1,1,1)
    cont.est <- a[1] + a[2]*x1 + a[3]*x2
    ex.rule <- rep(0,ns)
    ex.rule[idx] <- (cf.ee[idx] > cont.est)
    cf.next <- (ex.rule==0)*cf.next*exp(-r*dt) + (ex.rule==1)*cf.ee
  }
  
  
  p.am <- mean(cf.next)
  p.eu <- mean(pmax(K-s[nt,],0)*exp(-r*t))
  eep <- p.am - p.eu
  ans <- data.frame(p.am, p.eu, eep)
}

require(fOptions)
x <- NULL
x[1] <- GBSOption("p", 100,95, 0.5, 0.05,0.05,0.2)@price
x[2] <- BSAmericanApproxOption("p", 100,95,0.5,0.05,0.05,0.2)@price
print(x)

nsim.seq <- seq(0,10000, 100)
f <- function(x) am.put.sim(ns=x)$p.am
g <- function(x) am.put.sim(ns=x)$p.eu
x <- sapply(nsim.seq, f)
y <- sapply(nsim.seq, g)
plot(nsim.seq, x, type='l')
lines(nsim.seq, y, col='red')
abline(h=2.5877956)
abline(h=2.527191, col='red')

#------------------------------------------------------------------------------#
# 2. Binary Option

N <- 1000
T <- 2
S0 <- 100
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
dt <- T / nsteps
set.seed(1)

binary.call <- function(S0,K,r,sigma,T,N){
    x <- rnorm(N,0,1) # random numbers
   
    y <- (x - mean(x))/sd(x)
    
    P <- 0 # initialization of the price P of digital
    
    for(n in 1:N){
      if(S0*exp((r-0.5*sigma^2)*T+sigma*sqrt(T)*y[n])>K)
        {P <- P + 1}
    }
    P <- exp(-r*T)*P/
      
    return(c("price of binary call"=P))
}
binary.call(S0,K,r,sigma,T,N)

#

binary.put <- function(S0,K,r,sigma,T,N){
  x <- rnorm(N,0,1) # random numbers
  
  y <- (x - mean(x))/sd(x)
  
  P <- 0 # initialization of the price P of digital
  
  for(n in 1:N){
    if(S0*exp((r-0.5*sigma^2)*T+sigma*sqrt(T)*y[n])<K)
    {P <- P + 1}
  }
  P <- exp(-r*T)*P/
    
  return(c("price of binary put"=P))
}
binary.put(S0,K,r,sigma,T,N)


#------------------------------------------------------------------------------------#

# 3. Barrier Option
# 3-1 Down and out Call
# 3-2 Up and Out Call
# 3-3 Up and in Call
# 3-4 Down and in Call
# 3-5 Down and out Put
# 3-6 Down and in Put
# 3-7 Up and in Put
# 3-8 Up and out Put

# KI KO 모두 KO라고 변수를 지정하였음. 헷갈리지 말것.

## black Scholes ##
bso <- function(s,k,r,q,v,t,putcall){
  d1 = log(s/k) + (r-q+0.5*v*v)*t
  d1 = d1 / (v*sqrt(t))
  d2 = d1 - v *sqrt(t)
  nd1 = pnorm(d1)
  nd2 = pnorm(d2)
  ans = s*exp(-q*t) * nd1 - k*exp(-r*t)*nd2
  if(putcall == 'put'){
    nd1 = pnorm(-d1)
    nd2 = pnorm(-d2)
    ans - k*exp(-r*t)*nd2 - s*exp(-q*t)*nd1
  }
  if(t<=0)
  {
    ans = max(s-k, 0)
    if(putcall =='put') ans=max(k-x,0)
  }
  return(ans)
}

#3-1 Down and Out Call

T <- 6/12
B <- 95
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
  if(ko.flag[i,is]==1) { # s > B
    payoff[,is] <- max(s[,is]-K,0)#s > B Call payoff
  }
    
  else{
    payoff[,is] <- 0 # S < B call payoff
  }
  }
}

ans <- sum(payoff[ntime,])*exp(-r*T)/ N
ans

BarrierOption("downout", "call", 100,K, 0,r, T, sigma, B)$value

# 3-2 Up and Out Call #

T <- 6/12
B <- 105
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <- 0 # no KO payoff
    }
    else{
      payoff[,is] <- max(s[,is]-K,0) #KO payoff
    }
  }
}

ans <- sum(payoff[ntime,])*exp(-r*T)/ N
ans

BarrierOption("upout", "call", 100,K, 0,r, T, sigma, B)$value

#BarrierOption("upout", "call", 100,K, 0,r, T, sigma, B)$value

# 3-3 Up and in Call

T <- 6/12
B <- 105
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <- bso(s[ntime, is], K, r,0,T,sigma,'call') # no KO payoff
    }
    else{
      payoff[,is] <-0
    }
  }
}

ans <- sum(payoff[ntime,])/ N
ans

BarrierOption("upin", "call", 100,K, 0,r, T, sigma, B)$value


# 3-4 Down and in Call

T <- 6/12
B <- 95
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <- bso(s[ntime, is], K, r,0,T,sigma,'call') # no KO payoff
    }
    else{
      payoff[,is] <-0
    }
  }
}

ans <- sum(payoff[ntime,])/ N
ans

#BarrierOption("downin", "call", 100,K, 0,r, T, sigma, B)$value


# 3-5 Down and out Put


T <- 6/12
B <- 95
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <-max(K-s[,is],0) # no KO payoff
    }
    else{
      payoff[,is] <-0
    }
  }
}

ans <- sum(payoff[ntime,])/ N
ans

BarrierOption("downout", "put", 100,K, 0,r, T, sigma, B)$value


# 3-6 Down and in Put

T <- 6/12
B <- 95
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <- 0 # no KO payoff
    }
    else{
      payoff[,is] <- bso(s[ntime, is], K, r,0,T,sigma,'put')
    }
  }
}

ans <- sum(payoff[ntime,])/ N
ans

BarrierOption("downin", "put", 100,K, 0,r, T, sigma, B)$value




# 3-7 Up and in Put

T <- 6/12
B <- 105
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <- bso(s[ntime, is], K, r,0,T,sigma,'put') # no KO payoff
    }
    else{
      payoff[,is] <-0
    }
  }
}

ans <- sum(payoff[ntime,])/ N
ans

BarrierOption("upin", "put", 100,K, 0,r, T, sigma, B)$value




# 3-8 Up and Out Put

T <- 6/12
B <- 105
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime

s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)

z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B < und)
  #print(KI > und)
  if(sum(B<und)>0) ko.flag[i,] <- B < und
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[,is] <-0 # no KO payoff
    }
    else{
      payoff[,is] <-bso(s[ntime, is], K, r,0,T,sigma,'put')
    }
  }
}

ans <- sum(payoff[ntime,])/ N
ans

BarrierOption("upout", "put", 100,K, 0,r, T, sigma, B)$value



#-------------------------------------------------------------------------------------#
# 4. Double Rebate Option 
# 4-1. Up or Down In Option

T <- 2
B1 <- 90
B2 <- 110
sigma <- 0.2
K <- 100
r <- 0.015
R <- 5
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime


s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)


z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B1 < und && und < B2)
  #print(KI > und)
  if ((sum(B1<und)>0) && (sum(und < B2)>0)) ko.flag[i,] <- (B1 < und && und < B2)
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==0) { # not KO
      payoff[i,is] <- R #no KO payoff
    }
    else{
      payoff[i,is] <- 0 #KO payoff
    }
  }
}

ans <- sum(payoff[ntime,])*exp(-r*T)/ N
ans


#

# 4-2. Up or Down out Option

T <- 2
B1 <- 95
B2 <- 105
sigma <- 0.2
K <- 100
r <- 0.015
R <- 5
#n <- 1000
ntime <- 100
N <- 1000
dt <- T/ntime


s <- matrix(0, ncol = N, nrow = ntime)
s[1,] <- 100
set.seed(1)


z <- rnorm(N * ntime)
w <- matrix(z, ntime, N)
ko.flag <- matrix(0, nrow = ntime,ncol= N)
ki.flag <- matrix(0, ntime, N)

for (i in 2:ntime){
  
  s[i,] <- s[i-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[i-1,])
  und <- s[i,]
  print(B1 < und && und < B2)
  #print(KI > und)
  if ((sum(B1>und)>0) | (sum(und > B2)>0)) ko.flag[i,] <- (B1 > und | und > B2)
  #if(sum(KI>und)>0) ki.flag[i,] <- KI > und
}

payoff <- matrix(0, nrow  = ntime, ncol = N)

#ko.count <- rep(0, N)

for (is in 1:N){
  for (i in 1: ntime){
    
    if(ko.flag[i,is]==1) { # not KO
      payoff[i,is] <- R #no KO payoff
    }
    else{
      payoff[i,is] <- 0 #KO payoff
    }
  }
}

ans <- sum(payoff[ntime,])*exp(-r*T)/ N
ans

#-------------------------------------------------------------------------------------#
# 5. Asian Option #

library(plyr)

nsteps <- 1000
T <- 2
S0 <- 100
sigma <- 0.2
K <- 100
r <- 0.015
#n <- 1000
dt <- T / nsteps

S <- numeric(nsteps+1)
S[1] <- S0

asian_call <- function(){
  
  for (i in 1:nsteps){
    W <- rnorm(1)
    S[i+1] <- S[i] * exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*W)
  }
  
  Si.bar <- mean(S[-1])
  exp(-r*T) * max(Si.bar - K, 0)
}

asian_call_result <- raply(n, asian_call(), .progress = "text")
asian_call_price <- mean(asian_call_result)
asian_call_price

#

asian_put <- function(){
  
  for (i in 1:nsteps){
    W <- rnorm(1)
    S[i+1] <- S[i] * exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*W)
  }
  
  Si.bar <- mean(S[-1])
  exp(-r*T) * max(K-Si.bar, 0)
}

asian_put_result <- raply(n, asian_put(), .progress = "text")
asian_put_price <- mean(asian_put_result)
asian_put_price

# benchmark 
GeometricAverageRateOption("c", S0, K,T,r,0,sigma)
GeometricAverageRateOption("p", S0, K,T,r,0,sigma)
