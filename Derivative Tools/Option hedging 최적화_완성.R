

##### Optimization of Option Hedging #####

library(fOptions)
# sample option price
GBSOption("c", 100, 100, 1/2, 0.05, 0.05, 0.3)

# input (option characteristics) #
S <- 49
X <- 50
r <- 0.05
vol <- 0.2
mat <- 20/52
mu <- 0.13
dt <- 1/52

set.seed(1)
price_simulation <- function(S0, mu, sigma, rf, K, Time, dt, plots = FALSE){
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W <- c(0, cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  for(i in 1:(N-1)){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price}
  if(plots){
    dev.new(width = 30, height = 10)
    par(mfrow = c(1,3))
    plot(t, S, type = "l", main = "Price of underlying")
    plot(t[-length(t)], delta, type = "l", main = "Delta", xlab = "t")
    plot(t[-length(t)], call_, type = "l", main = "Price of Option", xlab = "t")
  }
}

price_simulation(100, 0.2, 0.3, 0.05, 100, 0.5, 1/250, plots=TRUE)   

#delta는 1에 수렴하며, 기초자산의 변동성을 따른다
# 현물 가격이 오르면 기초자산 long, 내리면 기초자산 short (delta)에 비례

### Transaction cost : option price movement affects ###

cost_simulation = function(S0, mu, sigma, rf, K, Time, dt, periods){
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W <- c(0, cumsum(rnorm(N-1)))
  S <- S0 * exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  SN = S[N]
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  for(i in 1:(N-1)){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price
  }
  
  S = S[seq(1, N-1, by = periods)]
  delta = delta[seq(1, N-1, by = periods)]
  m = length(S)
  share_cost <- rep(0, m)
  interest_cost <- rep(0, m)
  total_cost <- rep(0, m)
  share_cost[1] <- S[1]*delta[1] # underlying * delta = 매입 cost
  interest_cost[1] <- (exp(rf*dt*periods)-1)*share_cost[1] # 포지션 유지비 
  total_cost[1]<- share_cost[1] + interest_cost[1]
  
  for(i in 2:(m)){
    share_cost[i] <- (delta[i] - delta[i-1])*S[i]
    interest_cost[i] <- (total_cost[i-1] + share_cost[i])*(exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]}
  c = max(SN - K,0) #call
  cost = c-delta[m]*SN + total_cost[m]
  return(cost*exp(-Time*rf))
}

# simulation results #

par(mfrow = c(2,3))
i = 0
per = c(2,4,8,20,40,80)
call_price = GBSOption("c", 49,50, 20/52, 0.05, 0, 0.2)@price
results = matrix(0, 6, 5)
rownames(results) = c("1/2 days", "1 day", "2 days", "1 week", "2weeks", "4 weeks")
colnames(results) = c("E", "lower", "upper", "v", "ratio")

for(j in per){
  i = i +1
  A = rep(0, 1000)
  set.seed(2)
  
  for (h in 1:1000){A[h] = cost_simulation(49, 0.13, 0.2, 0.05, 50, 20/52, 1/1000,j)}
  E = mean(A)
  v = sd(A)
  results[i,1] = E
  results[i, 2] = E-1.96 * v/ sqrt(1000)
  results[i,3] = E+1.96*v/sqrt(1000)
  results[i,4] = v
  results[i,5] = v/call_price
  hist(A, freq = F, main = "", xlab = "")
  title(main = rownames(results)[i], sub = paste("E = ", round(E,4)," sd = ", round(v,4)))
  curve(dnorm(x, mean = mean(A), sd = sd(A)), col = "darkblue", lwd = 2 , add =TRUE, yxat = "n")
}

print(results)
dev.new()
curve(dnorm(x, results[1,1], results[1,4]), 6, 14, ylab = "", xlab ="cost")
for(l in 2:6) curve(dnorm(x, results[l,1], results[l,4]), add = TRUE, lty = 1)
legend(legend = rownames(results), "topright", lty = 1:6)
#simulation cost simulation S0, mu, sigma, rf, K, time, dt
#cost_simulation = function(S0, mu, sigma, rf, K, Time, dt)

#---------------------------------------------------------------------#
#1 weekley
A = rep(0, 1000)
for(i in 1:1000){A[i] = cost_simulation(49, 0.13, 0.2, 0.05, 50, 20/52, 1/52)}
#2 daily
#B = rep(0, 1000)
#for(i in 1:1000){B[i] = cost_simulation(100, 0.2, 0.3, 0.05, 100, 0.5 , 1/250)}
#
#### Histogram ####

#dev.new(width = 20, height = 10)
#par(mfrow  = c(1,2))
#hist(A, freq = F, main = paste("E = ", round(mean(A), 4)," sd = ",
 #                              round(sd(A),4)))
#curve(dnorm(x, mean = mean(A), sd = sd(A)), col ="darkblue", lwd= 2,
#      add =TRUE, yaxt = "n")
#hist(B, freq = F, main = paste("E = ", round(mean(B), 4)," sd = ",
#                               round(sd(B),4)), xlim = c(6,14), ylim = c(0,0.7))
#curve(dnorm(x, mean = mean(B), sd = sd(B)), col ="darkblue", lwd= 2,
#      add =TRUE, yaxt = "n")

# *** dt가 작아지면 비용의 표준편차를 줄일 수 있으나 기대가치는 줄어든다.
#-----------------------------------------------------------------------------#


#-----------------------------------------------------------------------------#
### Hedging Optimization ###

#fixed cash transaction costs
cost_simulation_ab = function(S0, mu, sigma, rf, K, Time, dt, periods, cost_per_trade){
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W <- c(0, cumsum(rnorm(N-1)))
  S <- S0 * exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  SN = S[N]
  delta <- rep(0, N-1)
  call_ <- rep(0, N-1)
  for(i in 1:(N-1)){
    delta[i] <- GBSGreeks("Delta", "c", S[i], K, Time-t[i], rf, rf, sigma)
    call_[i] <- GBSOption("c", S[i], K, Time-t[i], rf, rf, sigma)@price
  }
  
  S = S[seq(1, N-1, by = periods)]
  delta = delta[seq(1, N-1, by = periods)]
  m = length(S)
  share_cost <- rep(0, m)
  interest_cost <- rep(0, m)
  total_cost <- rep(0, m)
  share_cost[1] <- S[1]*delta[1] + cost_per_trade # underlying * delta = 매입 cost
  interest_cost[1] <- (exp(rf*dt*periods)-1)*share_cost[1] # 포지션 유지비 
  total_cost[1]<- share_cost[1] + interest_cost[1]
  
  for(i in 2:(m)){
    share_cost[i] <- (delta[i] - delta[i-1])*S[i] + cost_per_trade
    interest_cost[i] <- (total_cost[i-1] + share_cost[i])*(exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]}
  c = max(SN - K,0) #call
  cost = c-delta[m]*SN + total_cost[m]
  return(cost*exp(-Time*rf))
}
## proportional transaction cost

cost_simulation_pro <- function(S0, mu, sigma, rf, K, Time, dt, periods, relative_cost, plots = F){
  
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W <- c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  SN <- S[N]
  
  delta <- mapply(GBSGreeks, S = S[1:(N-1)], Time = (Time-t)[1:(N-1)], Selection = "Delta", TypeFlag = "c", X = K, r = rf, b = rf, sigma = sigma)
  
  S <- S[seq(1, N-1, by = periods)]
  delta <- delta[seq(1, N-1, by = periods)]
  m <- length(S)
  
  share_cost <- rep(0,m)
  interest_cost <- rep(0,m)
  total_cost <- rep(0, m)
  
  share_cost[1] <- S[1]*delta[1] * (1 + relative_cost)
  interest_cost[1] <- (exp(rf*dt*periods)-1) * share_cost[1]
  total_cost[1] <- share_cost[1] + interest_cost[1]
  
  for(i in 2:(m)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i] + abs( delta[i] - delta[i-1] ) * (1 + relative_cost)
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
  }
  
  c <- max( SN - K , 0)
  
  cost <- c - delta[m]*SN + total_cost[m]                         
  
  #call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
  
  return(cost*exp(-Time*rf))
  
}
### results ####

#cash

par(mfrow = c(2,3))
i = 0
per = c(2,4,8,20,40,80)
call_price = GBSOption("c", 49,50, 20/52, 0.05, 0, 0.2)@price
results = matrix(0, 6, 5)
rownames(results) = c("1/2 days", "1 day", "2 days", "1 week", "2weeks", "4 weeks")
colnames(results) = c("E", "lower", "upper", "v", "ratio")

for(j in per){
  i = i +1
  A = rep(0, 1000)
  set.seed(2)
  
  for (h in 1:1000){A[h] = cost_simulation_ab(49, 0.13, 0.2, 0.05, 50, 20/52, 1/1000,j, 0.02)}
  E = mean(A)
  v = sd(A)
  results[i,1] = E
  results[i, 2] = E-1.96 * v/ sqrt(1000)
  results[i,3] = E+1.96*v/sqrt(1000)
  results[i,4] = v
  results[i,5] = v/call_price
  hist(A, freq = F, main = "", xlab = "")
  title(main = rownames(results)[i], sub = paste("E = ", round(E,4)," sd = ", round(v,4)))
  curve(dnorm(x, mean = mean(A), sd = sd(A)), col = "darkblue", lwd = 2 , add =TRUE, yxat = "n")
}

print(results)
dev.new()
curve(dnorm(x, results[1,1], results[1,4]), 6, 14, ylab = "", xlab ="cost")
for(l in 2:6) curve(dnorm(x, results[l,1], results[l,4]), add = TRUE, lty = 1)
legend(legend = rownames(results), "topright", lty = 1:6)


#pro

par(mfrow = c(2,3))
i = 0
per = c(2,4,8,20,40,80)
call_price = GBSOption("c", 49,50, 20/52, 0.05, 0, 0.2)@price
results = matrix(0, 6, 5)
rownames(results) = c("1/2 days", "1 day", "2 days", "1 week", "2weeks", "4 weeks")
colnames(results) = c("E", "lower", "upper", "v", "ratio")

for(j in per){
  i = i +1
  A = rep(0, 1000)
  set.seed(2)
  
  for (h in 1:1000){A[h] = cost_simulation_pro(49, 0.13, 0.2, 0.05, 50, 20/52, 1/1000,j, 0.01)}
  E = mean(A)
  v = sd(A)
  results[i,1] = E
  results[i, 2] = E-1.96 * v/ sqrt(1000)
  results[i,3] = E+1.96*v/sqrt(1000)
  results[i,4] = v
  results[i,5] = v/call_price
  hist(A, freq = F, main = "", xlab = "")
  title(main = rownames(results)[i], sub = paste("E = ", round(E,4)," sd = ", round(v,4)))
  curve(dnorm(x, mean = mean(A), sd = sd(A)), col = "darkblue", lwd = 2 , add =TRUE, yxat = "n")
}

print(results)
dev.new()
curve(dnorm(x, results[1,1], results[1,4]), 6, 14, ylab = "", xlab ="cost")
for(l in 2:6) curve(dnorm(x, results[l,1], results[l,4]), add = TRUE, lty = 1)
legend(legend = rownames(results), "topright", lty = 1:6)


#------------------------------------------------------------------------------#
### Optimization with objective function ####

## Proportional Costs ##

n_sim <- 1000
threshold <- 12

# new cost_simulation function for optimization #
cost_sim <- function(cost = 0.01, n = n_sim, per = 1){
  a <- replicate(n, cost_simulation_pro(49, 0.13, 0.2 , 0.05, 50, 20/52,1/1000, per, cost));
  l <- list(mean(a), sd(a), quantile(a, 0.95))}
A <- sapply(seq(1,80), function(per) {print(per);set.seed(2);cost_sim(per=per)})
e <- unlist(A[1,])
s <- unlist(A[2,])
q <- unlist(A[3,])
u <- e+s^2

A <- cbind(t(A), u)

z1 <- which.min(e)
z2 <- which.min(s)
z3 <- which.min(u)
(paste("E min = ", z1, "cost of hedge = ", e[z1]," sd = ", s[z1]))
(paste("s min = ", z2, "cost of hedge = ", e[z2]," sd = ", s[z2]))
(paste("U min = ", z3, "u = ", u[z3]," cost of hedge = ",e[z3], " sd = ",  s[z3]))
matplot(A, type = "l", lty = 1:4, xlab= "dt", col = 1)
lab_for_leg = c("E", "Sd", "95% quantile", "E + variance")
legend(legend = lab_for_leg, "bottomright", cex = 0.6, lty = 1:4)
abline(v = c(z1,z2,z3), lty= 6, col = "grey")
abline(h = threshold, lty = 1, col = "grey")
text(c(z1,z1,z2,z2,z3,z3,z3), c(e[z1], s[z2], e[z2], e[z3], s[z3], u[z3]),
     round(c(e[z1], s[z1], s[z2], e[z2], e[z3], s[z3], u[z3]), 3), pos = 3, cex =0.7)
e2 <- e
e2[q > threshold] <- max(e)
z4 <- which.min(e2)
z5 <- which.min(q)
if(q[z5] < threshold){
  print(paste(" min VaR = ", q[z4], "at", z4, "E(cost | VaR < threshold = ", e[z4], " s = ", s[z4]))
} else {
  print(paste("optimization failed, min VaR = ", q[z5], "at", z5, "where cost = ", e[z5], " s = ", s[z5]))
}



##### Final Optimization time periods #####

n_sim <- 1000
threshold <- 6

## proportional transaction cost ##

cost_simulation_pro <- function(S0, mu, sigma, rf, K, Time, dt, periods, relative_cost, plots = F){
  
  t <- seq(0, Time, by = dt)
  N <- length(t)
  W <- c(0,cumsum(rnorm(N-1)))
  S <- S0*exp((mu-sigma^2/2)*t + sigma*sqrt(dt)*W)
  SN <- S[N]
  
  delta <- mapply(GBSGreeks, S = S[1:(N-1)], Time = (Time-t)[1:(N-1)], Selection = "Delta", TypeFlag = "c", X = K, r = rf, b = rf, sigma = sigma)
  
  S <- S[seq(1, N-1, by = periods)]
  delta <- delta[seq(1, N-1, by = periods)]
  m <- length(S)
  
  share_cost <- rep(0,m)
  interest_cost <- rep(0,m)
  total_cost <- rep(0, m)
  
  share_cost[1] <- S[1]*delta[1] * (1 + relative_cost)
  interest_cost[1] <- (exp(rf*dt*periods)-1) * share_cost[1]
  total_cost[1] <- share_cost[1] + interest_cost[1]
  
  for(i in 2:(m)){
    share_cost[i] <- ( delta[i] - delta[i-1] ) * S[i] + abs( delta[i] - delta[i-1] ) * (1 + relative_cost)
    interest_cost[i] <- ( total_cost[i-1] + share_cost[i] ) * (exp(rf*dt*periods)-1)
    total_cost[i] <- total_cost[i-1] + interest_cost[i] + share_cost[i]
  }
  
  c <- max( SN - K , 0)
  
  cost <- c - delta[m]*SN + total_cost[m]                         
  
  #call_price = GBSOption("c", 100, 100, 0.5, 0.05, 0.05, 0.3)@price
  
  return(cost*exp(-Time*rf))
  
}

cost_Sim <- function(cost = 0.01, n = n_sim, per = 1){a <- replicate(n, cost_simulation_pro(49, .13, .2,0.05, 50, 20/52, 1/1000,per,cost)); 
l <- list(mean(a), sd(a), quantile(a,0.95))}
A <- sapply(seq(1,300) ,function(per) {print(per); set.seed(2); cost_Sim(per = per)})
e <- unlist(A[1,])
s <- unlist(A[2,])
q <- unlist(A[3,])
u <- e + s^2
A <- cbind(t(A), u)
z1 <- which.min(e)
z2 <- which.min(s)
z3 <- which.min(u)
(paste("E min =", z1, "cost of hedge = ",e[z1]," sd = ", s[z1]))
(paste("s min =", z2, "cost of hedge = ",e[z2]," sd = ", s[z2]))
(paste("U min =", z3, "u = ",u[z3],"cost of hedge = ",e[z3]," sd = ", s[z3]))
matplot(A, type = "l", lty = 1:4, xlab = "Rebalancing Time Diff", col = 1, main = "Hedging Optimization with Transaction cost(1%)", ylab = "Hedging Cost")
lab_for_leg = c("E", "Sd", "95% quantile","E + variance")
legend(legend = lab_for_leg, "topright", cex = 0.6, lty = 1:4)
abline( v = c(z1,z2,z3), lty = 6, col = "grey")
abline( h = threshold, lty = 1, col = "grey")
abline( h = 19.23077, lty = 2, col = "black")
text(c(z1,z1,z2,z2,z3,z3,z3),c(e[z1],s[z1],s[z2],e[z2],e[z3],s[z3],u[z3]),round(c(e[z1],s[z1],s[z2],e[z2],e[z3],s[z3],u[z3]),3), pos = 3, cex = 0.7)
e2 <- e
e2[q > threshold] <- max(e)
z4 <- which.min(e2)
z5 <- which.min(q)
if( q[z5] < threshold ){
  print(paste(" min VaR = ", q[z4], "at", z4 ,"E(cost | VaR < threshold = " ,e[z4], " s = ", s[z4]))
} else {
  print(paste("optimization failed, min VaR = ", q[z5], "at", z5 , "where cost = ", e[z5], " s = ", s[z5])) 
}

#해석
# lowest expected cost by dt
   # lowest volatility by dt
# mean-varaince optimization 

