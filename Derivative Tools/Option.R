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

