#### 3. Down and out Call ####

library(RQuantLib)
library(fExoticOptions)
library(fOptions)
library(GUIDE)
GUIDE()
AmericanOption(type = 'p', 100,105,0,0.1,0.2,0.3,timeSteps=10000)


### Standard Fomula ###
barrierPrice <- function(s){
  BarrierOption(
    "downout","call", underlying=s,strike = 100,
    dividendYield = 0,
    riskFreeRate = 0.1,
    maturity = 0.2,
    volatility = 0.3,
    barrier = 95)$value
}

standard <- barrierPrice(100)


### CRR model ###

 


barrier_CRR <- function(S0,k,h,v,r,div,T,nmb_p){
  nt <- nmb_p +1 #time nod
  ns <- nt
  dt <- T/nmb_p
  st<-matrix(0,nt,ns)
  opt<-matrix(0,nt,ns)
  u <- exp(v*sqrt(dt))
  d <- 1/u
  
  adj <- exp((r-div)*dt) #upper
  dsc <- exp(-r*dt) #discount
  pu <- (adj-d)/(u-d) # riskneutral prob
  pd <- 1- pu
  st[,1] <- S0*d^(0:(nt-1))
  for(i in 2:nt){
    st[i, 2:i] <- st[(i-1), 1:(i-1)]*u
  }
  opt[nt,] <- pmax(st[nt,] - k,0)
  for(i in (nt-1):1){
    for (j in (nt-1):1){
      if (st[i,j] < 95){
        opt[i,j] <- 0
      }
      else{
        opt[i,j] <- (opt[i+1, j+1]*pu+ opt[i+1, j]*pd)*dsc}
    }
  }
  ans <- opt[1,1]
}

## Results
result <- numeric(length(50:1000))
n<- seq(50,1000)
for (i in n){
  result[i-49] <- barrier_CRR(100,100,95, 0.3, 0.1, 0, 0.2,i)
}

error_result <- result-standard
error_result

plot(error_result, type='l',col='black', main = "C-DO Barrier Option error (BS-CRR)", ylab = "Results", xlab="Steps", xlim=c(0,1000))

## Lambda ##

lambda <- c()

for(n in 50:1000){
  for (i in 2:n){
    T <- 0.2
    u <- exp(0.3 * sqrt(T/n))
    d <- 1/u
    s1 <- 100 * (u ^ i) * (d ^ (n - i))
    if (s1 > 95){
      sk <- s1
      sk1 <- S0 * (u ^ (i - 1)) * (d ^ (n - (i - 1)))
      lam <- (sk-95)/(sk-sk1)
      lambda <- c(lambda, lam)
      flag = 1
      break
      
    }
  }
}

lambda
length(lambda)
plot(lambda, main = "Error Profile : Lambda Graph", xlim = c(50,1000), xlab = "Steps", ylab = "Lambda")


