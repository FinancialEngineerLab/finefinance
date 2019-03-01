#### Project 1 ####
library(dplyr)

S <- 2643.85 ##S&P500 at 2019-01-28
S0 <- 1000
v <- 0.1593 ##implied vol of S&P500 at 2019-01-28 
div <- 0.022 ## div yield of S&P500 at 2019-01-28
r <- 0.0265 ## libor maturity 2024-01-31
acclist <- seq(21,1260,21)
#acclist <- sort(acclist, decreasing = TRUE)
redlist <- seq(252,564 ,21)
#redlist <- sort(redlist, decreasing = TRUE)
T <- 5 # Maturtiy by years
nmb_p <- T*252 # traing days 
q <- 0.0615 # accrual coupon


MLS_CRR <- function(S,S0,v,r,div,T,nmb_p,euro){
  nt <- nmb_p +1 #time nod
  ns <- nt #time node 
  dt <- T/nmb_p 
  st<-matrix(0,nt,ns)
  payoff<-matrix(0,nt,ns)
  opt <- matrix(0,nt,ns)
  mls <- matrix(0,nt,ns)
  mls2 <- matrix(0,nt,ns)
  u <- exp(v*sqrt(dt))
  d <- 1/u
  k <-S
  adj <- exp((r-div)*dt) #upper
  dsc <- exp(-r*dt) #discount
  pu <- (adj-d)/(u-d) # riskneutral prob
  pd <- 1- pu
  st[,1] <- S*d^(0:(nt-1))
  mls[,1] <- S0*d^(0:(nt-1)) #original mls value
  mls2[,1] <- S0*d^(0:(nt-1)) #mls value for accrual
  eopt <- matrix(0,nt,ns)
  
  # basic stock price matrix
  for(i in 2:nt){
    st[i, 2:i] <- st[(i-1), 1:(i-1)]*u
  }
  # option payoff
  eopt[nt,] <- pmax(st[nt,] - k,0)
  
  # Constructing option value matrix
  for(i in (nt-1):1){
    eopt[i,1:i] <- (eopt[i+1, 2:(i+1)]*pu+eopt[i+1, 1:i]*pd)*dsc
    if(euro == FALSE) opt[i,1:i] <- pmax(st[i,1:i]-k, opt[i,1:i])}
  
  ac <- st #accrual coupon matrix
  
  # barrier knockin matrix
  ac = ifelse(ac<0.8*S,0,1)
  
  # basic mls matrix
  for(i in 2:nt){
    mls[i, 2:i] <- mls[(i-1), 1:(i-1)]*u
  }
  payoff <- mls
  
  # Loss Condition of Initial value
  for(i in 1:nt){
    if (st[i,1261] < 0.8*S){
      payoff[i,1261] <- S0 - ((0.8*S - st[i, 1261])/S)*S0
    #  mls2[i,1260] <- S0 <- ((0.8*S - st[i,1260])/S)*S0
    }
  }
  
  # accural coupon matrix
  for(a in acclist){
      for(i in 1:21){
          payoff[,a] <- payoff[,a] + sum(ac[a-i, a-i] == 1)/21*1000*(q/12)
         }
      }
  # final valuation
  for(i in (nt-1):1){
    for (j in (nt-1):1){
      opt[i,j] <- (payoff[i+1, j+1]*pu+ payoff[i+1, j]*pd)*dsc
    }
  }
  
  ## redemtion
  for (i in redlist){
      payoff[,i] <- payoff[,i] - eopt[,i]
    }
   
  # re- final valuation
  for(i in (nt-1):1){
    for (j in (nt-1):1){
      opt[i,j] <- (payoff[i+1, j+1]*pu+ payoff[i+1, j]*pd)*dsc}
  }
  ans <- opt[1,1]
}

result <- MLS_CRR(S,S0,v,r,div,T,nmb_p,euro=FALSE)
result
