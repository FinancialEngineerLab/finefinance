#### 2. American Option ####


library(RQuantLib)
library(fExoticOptions)
library(fOptions)
### Standard Fomula ###
standard_am <- GBSOption(
    TypeFlag = "c",100,100,0.2,0.1,0, 0.3 )@price
standard_am

### CRR model ###

normal_CRR <- function(S0,k,h,v,r,div,T,nmb_p,euro){
  nt <- nmb_p +1 #time nod
  ns <- nt
  dt <- T/nmb_p #time difference
  st<-matrix(0,nt,ns) #stock binomial matrix
  opt<-matrix(0,nt,ns) #option payoff binomial matrix
  u <- exp(v*sqrt(dt)) 
  d <- 1/u
  
  adj <- exp((r-div)*dt) # adj-r
  dsc <- exp(-r*dt) #discount
  pu <- (adj-d)/(u-d) # riskneutral prob
  pd <- 1- pu
  
  # Constructing stock binomial matrix
  st[,1] <- S0*d^(0:(nt-1))
  for(i in 2:nt){
    st[i, 2:i] <- st[(i-1), 1:(i-1)]*u
  }
  
  # option payoff
  opt[nt,] <- pmax(st[nt,] - k,0)
  
  # Constructing option value matrix
  for(i in (nt-1):1){
    opt[i,1:i] <- (opt[i+1, 2:(i+1)]*pu+ opt[i+1, 1:i]*pd)*dsc
    if(euro == FALSE) opt[i,1:i] <- pmax(st[i,1:i]-k, opt[i,1:i])}
  
  ans <- opt[1,1]
  
}

## Valuation Results ##
result <- numeric(length(50:1000))
n<- seq(50,1000)
for (i in n){
  result[i-49] <- normal_CRR(100,100,95, 0.3, 0.1, 0, 0.2,i,FALSE)
}

## Error Calculation
error_result_am <- result - standard_am
error_result_am

plot(error_result_am, main = "American Option error (BS-CRR)", ylab = "results", xlab="steps", xlim=c(0,1000))



## Lambda ##

lambda_a <- c()

for(n in 50:1000){
  for (i in 2:n){
    S0 <- 100
    T <- 0.2
    u <- exp(0.3 * sqrt(T/n))
    d <- 1/u
    s1 <- 100 * (u ^ i) * (d ^ (n - i))
    if (s1 > 100){
      sk <- s1
      sk1 <- S0 * (u ^ (i - 1)) * (d ^ (n - (i - 1)))
      lam <- (sk-100)/(sk-sk1)
      lambda_a <- c(lambda_a, lam)
      flag = 1
      break
      
    }
  }
}


## lambda result
lambda_a
length(lambda_a)
plot(lambda_a, type='l',main = "American Opion Error : Lambda Graph", xlim = c(50,1000), xlab = "Steps", ylab = "Lambda")

