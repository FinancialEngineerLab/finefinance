##### 8.1 Exotic Option #####

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

## Greeks ###

# Delta 

bsoDelta <- function(s,k,r,q,v,t, putcall){
  ds = s/10000
  p0 = bso(s,k,r,q,v,t,putcall)
  p1 = bso(s+ds, k,r,q,v,t,putcall)
  ans = (p1-p0)/ds
  return(ans)
}

### 8.1.1 Digital Option ###

digitalCall <- function(f,k,v,t){
  d2 = (log(f/k)-v^2/2*t)/(v*sqrt(t))
  ans = pnorm(d2) # non-discounted payoff
  return(ans)
}

digitalCall.delta <- function(f,k,v,t){
  p0 <- digitalCall(f,k,v,t)
  p1 <- digitalCall(f+1/10000,k,v,t)
  ans <- p1-p0
  return(ans)
}

digitalCall.vega <- function(f,k,v,t){
  p0 <- digitalCall(f,k,v,t)
  p1 <- digitalCall(f,k,v+1/100,t)
  ans <- p1-p0
  return(ans)
}

par(mfrow = c(1,2))
f <- 100 ; k<- 100; v <-0.2 ; t <- 0.5
f.seq <- seq(50,150,1)
fn <- function(x) digitalCall(x,k,v,t)
plot(f.seq, sapply(f.seq, fn), type = 'l', xlab = "stock price", ylab = "option price")
fn <- function(x) digitalCall.delta(x,k,v,t)
plot(f.seq, sapply(f.seq, fn), type = 'l', xlab = "stock price", ylab = "option delta")

#### 8.1.2 Barrier Option ####

library(RQuantLib)

s<- seq(80,150)
barrierPrice <- function(s){
  BarrierOption("downout","put", s, 120, 0.01,0.03,0.1,0.4,80)$value
}

plot(s, sapply(s, barrierPrice), ylab = "Price", xlab = "Underlying", type ="l")
abline(v=120, lty = 2, col = 'red')
abline(v=80, lty=2, col = 'blue')
abline(h=0)


#Barrier option insecure structure
library(fExoticOptions)
delta.bar <- function(ty, s,k,H,rebate = 0, t,r,b,vol){
  p0 <- StandardBarrierOption(ty,s,k,H,rebate, t, r,b,vol)@price
  ans <- StandardBarrierOption(ty, s+1, k, H,rebate,t,r,b,vol)@price - p0
}

s0 <- 100
r<-0.04
q <-0
mu <-0.12
vol <- 0.4
t <- 30/365
b <- r-q
k<-s0 ; H=115
nsim <- 1;nt <-30
dt <- t/nt
s <- rep(0,nt)
dvanilla <- rep(0,nt)
dbarrier <- rep(0,nt)
s[1] <- s0
set.seed(1)
dvanilla[1] <- bsoDelta(s[1], k,r,q,vol,t, putcall = 'call')
dbarrier[1] <- delta.bar("cuo", s[1], k, H,0,t,r,b,vol)

for( i in 2:nt){
  t <- t-dt
  w1 <- rnorm(nsim, 0, 1)
  s[i] <- s[i-1]*exp((mu-0.5*vol^2)*dt + vol*sqrt(dt)*w1)
  dvanilla[i] <- bsoDelta(s[i], k,r,q,vol,t, 'call')
  dbarrier[i] <- delta.bar("cuo", s[i],k,H,0,t,r,b,vol)
}

par(mfrow = c(1,2))
plot(s,type = 'l', xlab=  "stock price path")
plot(dbarrier, type = 'l', xlab=  "delta")
lines(dvanilla, lty = 2)

#### 8.1.3 Asian Option ####

# 1. Geometric Average Rate option #

GeometricAverageRateOption(TypeFlag = "p", 80,85,0.25,0.05,0.08, 0.2)

# 2. Turnbull Wakeman Approxiamation #

TurnbullWakemanAsianApproxOption(TypeFlag = "p", 90, 88,95,Time =0.5, time = 0.25,
                                 tau = 0.0, 0.07, 0.02, 0.25)

#### 8.2 Adavnced Numerical Methods for Finance ####

t <- 0.5 ; s0 <- 287; r <-0.0175 ; sigma <- 0.2 ; K <- 275

set.seed(2)
ns <- 10
nt.tmp <- 10
nt <- nt.tmp +1 # index starting point : 1
u <- rnorm(ns*nt)

s <- matrix(NA,nt,ns)
w <- matrix(u, nt, ns)
opt <- matrix(NA, nt, ns)
pt <- rep(0, ns)

dt <- t / nt.tmp
s[1,] <- s0
for (it in 2:nt){
  s[it,] <- s[it-1,]*exp((r-0.5*sigma^2)*dt + sigma*sqrt(dt)*w[it,])
}

for(is in 1:ns) pt[is] <- which(s[,is]<K)[1]

matplot(s, type='l')
abline(h=K, col='red', lwd =2, lty =2)
for(is in 1:ns) points(pt[is], s[pt[is], is], lwd = 2)
p.eu <- mean(pmax(K-s[nt,], 0)*exp(-r*t))

idx <- which(pt!="NA")
p.am <- sum((K-diag(s[pt[idx], idx]))*exp(-r*dt*(pt[idx]-1)))/ns

## ITM되는 순간 행사하는 American OPtion의 Simulation

p.eu
p.am
#유럽형옵션이 가치가 높아지는 상황이 발생 -> 비효율적 투자임
#그러므로 LSMC시뮬레이션으로 개선이 가능함

#### LSMC American Option ####

s0 <-280; K <-270; r <- 0.0175 ; sigma <- 0.2 ; nt.tmp <- 10; ns<-10;t<-0.5
set.seed(2) 
nt <- nt.tmp +1 # index starts at 1
u <- rnorm(ns*nt)

s <- matrix(NA,nt,ns)
w <- matrix(u,nt,ns)
m.ex.rule <- matrix(0,nt,ns)

dt <- t/nt.tmp
s[1,] <- s0

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

### 8.2.2 FDM(Finite Difference Method) 유한차분법 ###

AmericanPutExp <-
  function(Smin = 0, Smax, T=1, N=10, M=10, K, r= 0.05, sigma=0.01){
    Dt = T/N
    DS = (Smax - Smin)/M
    t <- seq(0,T,by=Dt)
    S <- seq(Smin, Smax,by = DS)
    A <- function(j) (-0.5*r*j*Dt + 0.5*sigma^2*j^2*Dt)/(1+r*Dt) #A~C 계수 구현 
    B <- function(j) (1-sigma^2*j^2*Dt)/(1+r*Dt)
    C <- function(j) (0.5*r*j*Dt + 0.5*sigma^2*j^2*Dt)/(1+r*Dt)
    P <- matrix(, M+1, N+1) #옵션 가격 
    colnames(P) <- round(t, 2)
    rownames(P) <- round(rev(S), 2)
    P[M+1,] <- K  # 하한경계 
    P[1,] <- 0 #상한경계 
    P[,N+1] <- sapply(rev(S), function(x) max(K-x,0)) #조기행사여부
    optTime <- matrix(FALSE, M+1, N+1)  #하한경계 옵션행사여부 
    optTime[which(P[,N+1]>0), N+1] <- TRUE #만기시점 옵션행사여부 
    for(i in (N-1):0){ #Backward Induction
      for(j in 1:(M-1)){
        J <- M+1-j
        I <- i+1
        P[J,I] <- A(j)*P[J+1, I+1]+B(j)*P[J,I+1]+C(j)*P[J-1,I+1]
        if(P[J,I]<P[J,N+1])
          optTime[J,I] <- TRUE
      }
   }
    
    colnames(optTime) <- colnames(P)
    rownames(optTime) <- rownames(P)
    ans <- list(P=P, t=t, S=S, optTime = optTime, N=N, M=M)
    class(ans) <- "AmericanPut" 
    return(invisible(ans))
}

plot.AmericanPut <- function(obj){
  plot(range(obj$t), range(obj$S), type = "n", axes = F, xlab = "t", ylab = "S")
  axis(1, obj$t, obj$t)
  axis(2, obj$S, obj$S)
  abline(v= obj$t, h = obj$S, col = "darkgray", lty = "dotted")
  for(i in 0:obj$N){
    for(j in 0:obj$M){
      J <- obj$M+1-j
      I <- i+1
      cl <- "grey"
      if(obj$optTime[J,I])
        cl <- "black"
      text(obj$t[i+1], obj$S[j+1], round(obj$P[J,I],2), cex = 0.75, col=cl)
    }
  }
  DS <- mean(obj$S[1:2])
  y <- as.numeric(apply(obj$optTime, 2, function(x) which(x)[1]))
  lines(obj$t, obj$S[obj$M+2-y]+DS, lty =2)
}

put <- AmericanPutExp(Smax=60, sigma=0.4, K=30)
plot(put)

library(lattice)
x <- AmericanPutExp(Smax= 60, sigma = 0.4, K=30)
opt_rev <- x$P
timeopt <- apply(opt_rev, 2, rev)
wireframe(timeopt, xlab = 'stock', ylab = 'time')

par(mfrow=c(1,3))
wireframe(timeopt, xlab = 'stock', ylab = 'time')
persp(x$S, x$t,timeopt)
persp(x$S, x$t, timeopt, theta = 30, phi = 0)

## 이동하는 3D ##
library(rgl)
open3d()
persp3d(x$S, x$t, timeopt, col='gray')


#### 8.2.3 Fourier Transformation ####

x <- seq(-3,3, length=20)
f <- function(x) dnorm(x)
y <- fft(f(x), inverse=TRUE)
invFFT <- as.numeric(fft(y)/length(y))

head(f(x))
head(y)
head(invFFT)

s0 <- 100
k <- 100
t <- 0.5
r <- 0.04
sigma <- 0.2
phi <- function(u) exp((0+1i)*u*(r-0.5*sigma^2)*t - 0.5*sigma^2*t*u^2)
#
z<-seq(-100,100, length=1000)
dz <- z[2] - z[1]
density <- function(x) sum(0.5/pi*exp(-1i*z*x)*phi(z)*dz)
df<-function(x) Re(sapply(x, density))
par(mfrow = c(1,2))
x <- seq(-1,1,length=100)
y <- df(x)
plot(x,y, type= 'l', ylab = 'density(Fourier)')
plot(x, dnorm(x, (r-0.5*sigma^2)*t, sigma*sqrt(t)), type='l', ylab = 'density')
call.price <- sum(pmax(s0*exp(x)-k,0)*df(x)*dx)*exp(-r*t)
# 확률분포는 모르고 특성함수만 알고 있다면 Fourier 역변환을 통해 옵션가격을 구할 수 있다. 