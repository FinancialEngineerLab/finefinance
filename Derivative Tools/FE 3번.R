

###
S0 = 100
K = 100
B =95
r = 0.1
d = 0
sigma = 0.3
T = 0.2
time = seq(from=50, to= 1000)
length(time)

barrierBinom <- function(S0, k, h, r, div, T, v, nmb_p){
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
    if(st[nt,] > h){
      opt[i, 1:i] <- (opt[i+1, 2:(i+1)]*pu + opt[i+1, 1:i]*pd)*dsc
    }
    else{
      opt[i, 1:i] <-0
    }
  }
  ans <- opt[1,1]
  
}

a <- barrierBinom(100,100,95,0.1,0, 0.2, 0.3, 50)
a