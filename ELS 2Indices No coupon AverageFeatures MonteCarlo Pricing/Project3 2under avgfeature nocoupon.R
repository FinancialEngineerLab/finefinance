 ### Advanced MLS/ELS Pricing ###
## Author : Shin Hyunjin ##

library(quantmod)

### 2underlying assets + Average Feature + no coupon ###
## Product :  <Accelerated Return Securities Based on the Value of the Worst Performing of
## the S&P 500® Index and the Russell 2000® Index due September 11, 2023>, Morgan Stanley

### data crawling ###
getSymbols("^RUT", from = "2008-04-08", to = "2018-04-08")
s2 <- RUT$RUT.Clo  
s2 <- s2/lag(s2,1)-1
s2 <- na.omit(s2)

getSymbols("^GSPC",from = "2008-04-08", to = "2018-04-08")
s1 <- GSPC$GSPC.Close
s1 <- s1/lag(s1,1)-1
s1 <- na.omit(s1)

#corraltion between two #
cor(s1,s2)

### input data ## 

d1 <- 0.0189 ## S&P 500 historical dividend yield at 2018 mar 31
d2 <- 0.013 ## Russell 2000 histrocal dividend yield at 2018 mar 31
rf <- 0.0261 ## tbill longterm rate 5Y
rho <- cor(s1,s2) ## correlaion between two indices from 2010-01-01 to 2018-04-06

s_2 <- 1512.155
s_1 <- 2614.45

v_2 <- 0.2083 # implied vol of Russell 2000 at 2018 apr 04
v_1 <- 0.2006 # implied vol of S&P500 at 2018 apr 04

mu1 <- mean(s1)
mu2 <- mean(s2)
  
t<- 5.5
ntime <- as.numeric(as.Date("2023-09-11")- as.Date("2018-04-04"))
dt <- t/ntime
nt <- ntime +1
avgtime <- as.numeric(as.Date("2023-09-06")-as.Date("2023-06-06")) #1895번째 ~ 1985번째 평균 도출

nsim <- 10000 # numbers of doing simulation

# making barrier condition by both indices
# 1000 * % form
K1 <- 1210
K2 <- 1210
K3 <- 1000
K4 <- 1000
K5 <- 950
K6 <- 950

u1 <- matrix(0,ncol = nsim, nrow = nt)
u2 <- matrix(0, ncol= nsim, nrow =nt)

# 1000 * % form
u1[1,] <- 1000 #2614.45 
u2[1,] <- 1000 #1512.155

### basic matrix built ###
set.seed(1)
z1 <- rnorm(nsim*nt)
z2 <- rnorm(nsim*nt)
w1 <- matrix(z1, nt, nsim) # brwonian motion
w2 <- matrix(z2, nt, nsim) # brownian motion

avg1 <- matrix(0, 1,nsim)
avg2 <- matrix(0,1,nsim)
und <- matrix(0,1,nsim)
result <-matrix(0,nt,nsim)

sum1 <- matrix(0,91, nsim)
sum2 <- matrix(0,91,nsim)


##### Brownian motion and Monte Carlo #####

### Stochastic Calculus for both two indices ###
for(i in 2:nt){
  u1[i,] <- u1[i-1,] * exp((mu1-0.5*v_1^2)*dt + v_1*sqrt(dt)*w1[i-1,])
  z1<- rho * w1[i-1,] + sqrt(1-rho^2)*w2[i-1,]
  u2[i,] <- u2[i-1,] * exp((mu2-0.5*v_2^2)*dt + v_2*sqrt(dt)*z1)
}

### MonteCarlo Simulation ###
for(i in 1:nt){
   for(l in 1:nsim){
    for(j in 1895:1985){  # specific periods average from product description
        sum1[j-1894,l] <- u1[j,l]
        sum2[j-1894,l] <- u2[j,l]
        
        avg1[,l] <- sum(sum1[,l])/91
        avg2[,l] <- sum(sum2[,l])/91
     
    
    if(i == nt){
      und[,l] <- pmin(avg1[,l], avg2[,l]) # or condition average : pmin 
      
      # condition refelcted : worst performing factor #
      if(und[,l]>= K1) result[i,l] <- (1000+1000*((und[,l]/1000-1.21)*3.34)+415)*exp(-rf*t)
      if((und[,l] < K1) & (und[,l] >= K3)) result[i,l] <- (1000+1000*((und[,l]/1000-1)*1.5)+100) * exp(-rf*t)
      if((und[,l] < K3) & (und[,l] >= K5)) result[i,l] <- (1000+1000*((und[,l]/1000-0.95)*2)) * exp(-rf*t)
      if(und[,l] < K5) result[i,l] <- (1000*(und[,l]/1000)+50) * exp(-rf*t)
    }
    if(result[i,l] > 2116.6){ # max value
      result[i,l] <- 2116.6
    if(result[i,l] < 50){ # min value
      result[i,l] <- 50
    }
    }
      
    }
  }
}

ans<-sum(result[nt,])/nsim
print(ans)
