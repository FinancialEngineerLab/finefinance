##### Ch7 Exotic Option #####
install.packages("plot3D")
library(plot3D)
library(fOptions)
library(fExoticOptions)


### 1.Vanila Option ###

a <- GBSOption("c", 100, 100, 1, 0.02, -0.02, 0.3, title = NULL,description = NULL)
z <- a@price
z

b <- GeometricAverageRateOption("c", 100, 100, 1, 0.02, -0.02, 0.3, title = NULL, description = NULL)
w <- b@price
w

### 2. KIKO ###
## Vanilla - KO - KI = 0 ##

a <- StandardBarrierOption("cuo", 100, 90, 130, 0, 1, 0.02, -0.02, 0.3, title = NULL, description = NULL)
x <- a@price
x

b <- StandardBarrierOption("cui", 100, 90, 130, 0, 1, 0.02, -0.02, 0.3, title = NULL, description = NULL)
y <- b@price
y

c <- GBSOption("c", 100, 90, 1, 0.02, -0.02, 0.3, title = NULL, description = NULL)
z <- c@price
z

v <- z-x-y
v

# Barrier vs Underlying difference is higher, KO = Vanila

vanilla <- GBSOption(TypeFlag = "c", S = 100, X = 90, Time = 1, r=0.02, b =-0.02, sigma = 0.3)
KO <- sapply(100:300, FUN = StandardBarrierOption,
             TypeFlag = "cuo", S = 100, X = 90, K =0, Time = 1, r = 0.02, b = -0.02, sigma = 0.3)
plot(KO[[1]]@price, type = "l", xlab = "Barrier distance from spot", ylab = "price of option",
     main = "Price of KO converges to plain vanilla")
abline(h = vanilla@price, col = "red")

### Black Scholes Surface ###

# Vanilla BS Surface #
BS_surface <- function(S, Time, FUN, ...){
  library(plot3D)
  n <- length(S)
  k <- length(Time)
  m <- matrix(0, n, k)
  for(i in 1:n){
    for(j in 1:k){
      l <- list(S = S[i], Time = Time[j], ...)
      m[i,j] <- max(do.call(FUN, l)@price,0)
    }
  }
  persp3D(z = m, xlab = "Underlying Asset", ylab = "Remaining Time", zlab = "Option Price",
          phi = 30, theta = 20, bty = "b2")
}

BS_surface(seq(1, 200, length = 200), seq(0,2,length=200),
           GBSOption, TypeFlag = "c", X = 90, r = 0.02, b = 0, sigma = 0.3)

# Up and Out Call BS Surface #

BS_surface(seq(1,200,length =200), seq(0,2,length =200),
           StandardBarrierOption, TypeFlag = "cuo", H = 130, X = 90, K = 0,
           r = 0.02, b = -0.02, sigma = 0.3)

# Onetouch+ Notouch =  TBILL
# Double one touch + Double no touch = TBILL

#### Greek and Vanilla ####

GetGreeks <- function(FUN, arg, epsilon,...){
  all_args1 <- all_args2 <- list(...)
  all_args1[[arg]] <- as.numeric(all_args1[[arg]] + epsilon)
  all_args2[[arg]] <- as.numeric(all_args2[[arg]] - epsilon)
  (do.call(FUN, all_args1)@price - do.call(FUN, all_args2)@price)/(2*epsilon)
}

# Epsilon 
#vega : 0.005
#FX delta : 0.0001
#Stock delta : 0.01
#Theta : 1/365
#rho : 1 basis point = 0.0001

x <- seq(10, 200, length =200)
delta <- vega <- theta <- rho <- rep(0,200)
for(i in 1:200){
  delta[i] <- GetGreeks(FUN = FloatingStrikeLookbackOption,
                        arg = 2, epsilon = 0.01, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
  vega[i] <- GetGreeks(FUN = FloatingStrikeLookbackOption,
                       arg = 7, epsilon = 0.005,"p", x[i], 100, 1, 0.02, -0.02, 0.2)
  theta[i] <- GetGreeks(FUN = FloatingStrikeLookbackOption,
                        arg =4, epsilon =1/365, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
  rho[i] <- GetGreeks(FUN = FloatingStrikeLookbackOption,
                      arg = 5, epsilon=0.0001, "p", x[i], 100, 1, 0.02, -0.02, 0.2)
}

par(mfrow = c(2,2))

plot(x, delta, type = "l", xlab = "S", ylab = "", main = "Delta")  
plot(x, vega, type = "l", xlab = "S", ylab = "", main = "Vega")
plot(x, theta, type = "l", xlab=  "S", ylab = "", main = "Theta")
plot(x, rho , type ="l", xlab = "S", ylab = "", main = "Rho")


#### Double No Touch Option ####

dnt1 <- function(S,K,U,L, sigma, T, r, b, N= 20, ploterror = FALSE){
  if(L>S | S >U) return(0)
  Z <- log(U/L)
  alpha <- -1/2 * (2*b/sigma^2 -1)
  beta <- -1/4*(2*b/sigma^2 - 1)^2 - 2*r/sigma^2
  v <- rep(0, N)
  for(i in 1:N)
    v[i] <- 2*pi*i*K/(Z^2)*(((S/L)^alpha - (-1)^i*(S/U)^alpha)/
                              (alpha^2 + (i*pi/Z)^2))*sin(i*pi/Z*log(S/L))*exp(-1/2*((i*pi/Z)^2-beta)*sigma^2*T)
  if(ploterror) barplot(v, main = "Formula Error")
  sum(v)
}
par(mfrow=c(1,2))
print(dnt1(100, 10, 120, 80, 0.1, 0.25, 0.05,0.03,20,TRUE))
print(dnt1(100, 10, 120, 80, 0.07, 0.25, 0.05, 0.03, 50, TRUE))

## Develping error : U or L outlier removal and converging speed
dnt1 <- function(S,K,U,L, sigma, Time, r, b){
  if(L>S | S >U) {return(0)}
  Z <- log(U/L)
  alpha <- -1/2 * (2*b/sigma^2 -1)
  beta <- -1/4*(2*b/sigma^2 - 1)^2 - 2*r/sigma^2
  p <- 0
  i <- a <- 1
  while(abs(a) >0.0001){
    a <- 2*pi*i*K/(Z^2)*(((S/L)^alpha -(-1)^i*(S/U)^alpha)/
                              (alpha^2 + (i*pi/Z)^2))*sin(i*pi/Z*log(S/L))*exp(-1/2*((i*pi/Z)^2-beta)*sigma^2*Time)
    p <-p + a
    i <-i + 1
  }
  p
}

x <- seq(0.92, 0.96, length = 2000) # L and U, step = 2000
y <- z <- rep(0,2000)

for(i in 1:2000){
  y[i] <- dnt1(x[i], 1e6, 0.96, 0.92, 0.06, 0.25, 0.0025, -0.025) #vol 6%, T= 0.25,, rf = 0.25%,b=-.025%
  z[i] <- dnt1(x[i], 1e6, 0.96, 0.92, 0.065, 0.25, 0.0025, -0.025) # vol 6.5%
}

matplot(x, cbind(y,z), type = "l", lwd = 2, lty = 1,
        main = "Price of a DNT with Volatility 6% and 6.5%",
        cex.main = 0.8, xlab = "Price of underlying asset")

# vol higher vega lower
# Barrier close -> abs(vega) lower
# vega -


### Vega Gamma Theta Delta Estimation ###
GetGreeks <- function(FUN, arg, epsilon,...){
  all_args1 <- all_args2 <- list(...)
  all_args1[[arg]] <- as.numeric(all_args1[[arg]] + epsilon)
  all_args2[[arg]] <- as.numeric(all_args2[[arg]] - epsilon)
  (do.call(FUN, all_args1) - do.call(FUN, all_args2))/(2*epsilon)
}

Gamma <- function(FUN, epsilon, S, ...) {
  arg1 <- list(S, ...)
  arg2 <- list(S + 2 *epsilon, ...)
  arg3 <- list(S - 2 * epsilon, ...)
  y1 <- (do.call(FUN, arg2) - do.call(FUN, arg1))/(2*epsilon)
  y2 <- (do.call(FUN, arg1) - do.call(FUN, arg3))/(2*epsilon)
  (y1- y2)/ (2*epsilon)
}

dnt1 <- function(S,K,U,L, sigma, Time, r, b){
  if(L>S | S >U) {return(0)}
  Z <- log(U/L)
  alpha <- -1/2 * (2*b/sigma^2 -1)
  beta <- -1/4*(2*b/sigma^2 - 1)^2 - 2*r/sigma^2
  p <- 0
  i <- a <- 1
  while(abs(a) >0.0001){
    a <- 2*pi*i*K/(Z^2)*(((S/L)^alpha -(-1)^i*(S/U)^alpha)/
                           (alpha^2 + (i*pi/Z)^2))*sin(i*pi/Z*log(S/L))*exp(-1/2*((i*pi/Z)^2-beta)*sigma^2*Time)
    p <-p + a
    i <-i + 1
  }
  p
}
x = seq(0.9202, 0.9598, length = 200)
delta <- vega <- theta <- gamma <- rep(0,200)

for(i in 1:200){
  delta[i] <- GetGreeks(FUN = dnt1, arg =1, epsilon = 0.0001, 
                        x[i], 1000000, 0.96, 0.92, 0.06, 0.5, 0.02, -0.02)
  vega[i] <- GetGreeks(FUN = dnt1, arg =5, epsilon = 0.0005,
                       x[i], 1000000, 0.96, 0.92, 0.06, 0.5, 0.0025, -0.025)
  theta[i] <- -GetGreeks(FUN = dnt1, arg = 6, epsilon = 1/365,
                         x[i], 1000000, 0.96, 0.92, 0.06, 0.5, 0.0025, -0.025)
  gamma[i] <- Gamma(FUN = dnt1, epsilon = 0.0001, S = x[i], K = 1e6, U = 0.96, L = 0.92,
                    sigma = 0.06, Time = 0.5, r = 0.02, b = -0.02)
}
windows()
plot(x, vega, type = "l", xlab = "S", ylab = "", main = "Vega")
plot(x, delta, type = "l", xlab = "S", ylab = "", main = "Delta")
#높은 배리어가까워지면 델타 음수, Dynamic hedging : price higer long, lower short

plot(x, gamma, type = "l", xlab = "S", ylab ="", main = "Gamma")
plot(x, theta, type = "l", xlab = "S", ylab = "", main = "Theta")

#### DNT BS Surface ####

BS_surf <- function(S, Time, FUN, ...){
  n <- length(S)
  k <- length(Time)
  m <- matrix(0, n, k)
  for(i in 1:n){
    for(j in 1:k){
      l <- list(S = S[i], Time = Time[j], ...)
      m[i,j] <- do.call(FUN, l)
    }
  }
  persp3D(z = m, xlab  = "underlying asset", ylab = "Time",
          zlab = "option price", phi = 30, theta = 30, bty = "b2")
}

BS_surf(seq(0.92, 0.96, length = 200), seq(1/365, 1/48, length = 200),
        dnt1, K = 1000000, U = 0.96, L = 0.92, r=  0.0025, b = -0.0250, sigma = 0.2)

### DNT pricing another method ###

dnt2 <- function(S, K, U, L, sigma, T, r, b){
  a <- DoubleBarrierOption("co", S, L, L, U, T, r, b, sigma, 0, 0, title = NULL, description = NULL)
  z <- a@price
  
  b <- DoubleBarrierOption("po", S, U, L, U, T, r, b, sigma, 0, 0, title = NULL, description = NULL)
  y <- b@price
  
  (z + y) / (U-L)*K
}

dnt1(0.9266, 1000000, 0.96, 0.92, 0.06, 0.25, 0.0025, -0.025)
dnt2(0.9266, 1000000, 0.96, 0.92, 0.06, 0.25, 0.0025, -0.025)


##### Double No Touch Simulation #####

dnt1 <- function(S,K,U,L, sigma, Time, r, b){
  if(L>S | S >U) {return(0)}
  Z <- log(U/L)
  alpha <- -1/2 * (2*b/sigma^2 -1)
  beta <- -1/4*(2*b/sigma^2 - 1)^2 - 2*r/sigma^2
  p <- 0
  i <- a <- 1
  while(abs(a) >0.0001){
    a <- 2*pi*i*K/(Z^2)*(((S/L)^alpha -(-1)^i*(S/U)^alpha)/
                           (alpha^2 + (i*pi/Z)^2))*sin(i*pi/Z*log(S/L))*exp(-1/2*((i*pi/Z)^2-beta)*sigma^2*Time)
    p <-p + a
    i <-i + 1
  }
  p
}

d <- read.table("C:/users/shinhyunjin/dropbox/data/audusd.csv", colClasses =c("character", rep("numeric",5)), sep = ";", header = TRUE)
underlying <- as.vector(t(d[,2:5]))
t <- rep(d[,6], each = 4)

n <- length(t)
option_price <- rep(0,n)

for(i in 1:n){
  option_price[i] <- dnt1(S = underlying[i], K = 1000000, U = 0.96, L = 0.92, sigma = 0.06,
                          T = t[i]/(60*24*365), r= 0.0025, b = -0.025)
}

a <- min(option_price)
b <- max(option_price)
option_price_transformed = (option_price - a)*0.03 / (b-a)+0.92
par(mar=c(6,3,3,5))

matplot(cbind(underlying, option_price_transformed), type= "l", lty=1, col = c("blue","red"),
        main  ="Price of underlying asset and DNT", ylim = c(0.91, 0.97),
        xaxt = "n", yaxt = "n",
        ylab = "", xlab = "Remaining time")
abline(h = c(0.92, 0.96), col = "green")

axis(side = 2, at = pretty(option_price_transformed), col.axis = "blue", col = "blue")
axis(side =4, at = pretty(option_price_transformed), labels = round(seq(a/1000,1000, length =7)),las=2,
     col = "red", col.axis = "red")
axis(side = 1, at = seq(1, n, length =6), labels = round(t[round(seq(1,n,length=6))]))

dnt1(0.9203, 1000000, 0.96, 0.92, 0.06, 59/365, 0.0025, -0.025)

### DNT Brownian Simulation by MonteCarlo Simulation ###

library(matrixStats)

DNT_sim <- function(S0 = 0.9266, mu = 0, sigma = 0.06, U =0.96, L = 0.92, N = 5){
  dt <- 5 /(365*24*60)
  t <- seq(0, 0.25, by = dt)
  Time <- length(t)
  
  W <- matrix(rnorm((Time-1)*N), Time -1, N)
  W <- apply(W, 2, cumsum)
  W <- sqrt(dt) * rbind(rep(0,N), W)
  S <- S0 * exp((mu - sigma^2/2)*t + sigma*W)
  option_price <- matrix(0, Time, N)
  for(i in 1:N)
    for(j in 1:Time)
      option_price[j,i] <- dnt1(S[j,i], K = 1000000, U, L, sigma, 0.25-t[j], r = 0.0025, b = -0.025)*(min(S[1:j,i])> L & max(S[1:j,i])<U)

  survivals <- sum(option_price[Time,]>0)
  dev.new(width = 19, height = 10)

  par(mfrow = c(1,2))
  matplot(t, S, type = "l", main = "Underlying Price", xlab = paste("Survived", survivals, "from", N), ylab ="")
  abline(h = c(U,L), col = "blue")
  matplot(t, option_price, type = "l", main = "DNT Price", xlab = "", ylab ="")}

set.seed(210)
system.time(DNT_sim())



DNT_sim <- function(S0 = 0.9266, mu = 0, sigma = 0.06, U =0.96, L = 0.92, N = 15){
  dt <- 5 /(365*24*60)
  t <- seq(0, 0.25, by = dt)
  Time <- length(t)
  
  W <- matrix(rnorm((Time-1)*N), Time -1, N)
  W <- apply(W, 2, cumsum)
  W <- sqrt(dt) * rbind(rep(0,N), W)
  S <- S0 * exp((mu - sigma^2/2)*t + sigma*W)
  option_price <- matrix(0, Time, N)
  for(i in 1:N)
    for(j in 1:Time)
      option_price[j,i] <- dnt1(S[j,i], K = 1000000, U, L, sigma, 0.25-t[j], r = 0.0025, b = -0.025)*(min(S[1:j,i])> L & max(S[1:j,i])<U)
  
  survivals <- sum(option_price[Time,]>0)
  dev.new(width = 19, height = 10)
  
  par(mfrow = c(1,2))
  matplot(t, S, type = "l", main = "Underlying Price", xlab = paste("Survived", survivals, "from", N), ylab ="")
  abline(h = c(U,L), col = "blue")
  matplot(t, option_price, type = "l", main = "DNT Price", xlab = "", ylab ="")}

set.seed(210)
system.time(DNT_sim())


#### Structured Product and Exotic Option ####

dnt1(0.9266, 100000, 0.96, 0.92, 0.06, 90/365, 0.0025, -0.025)
dnt1(0.9266, 100000, 0.96, 0.9195, 0.06, 90/365, 0.0025, -0.025)
dnt1(0.9266, 1000000, 1.06, 0.92, 0.06, 90/365, 0.0025,-0.025)
dnt1(0.9266, 100, 0.9705, 0.9095, 0.06, 90/365, 0.0025,-0.025)

a <- BinaryBarrierOption(9, 0.9266, 0, 0.92, 1000000, 90/365, 0.0025, -0.025, 0.06, 1,1,title = NULL,description = NULL)
z <- a@price
z

## Implied DNT ##

implied_DNT_image <- function(S = 0.9266, K = 1000000, U = 0.96, L=0.92, sigma = 0.06, Time = 0.25,
                              r= 0.0025, b = -0.025){
  S_ <- seq(L, U, length = 300)
  K_ <- seq(800000, 1200000, length =300)
  U_ <- seq(L + 0.01, L + 0.15, length =300)
  L_ <- seq(0.8, U - 0.001, length =300)
  sigma_ <- seq(0.005, 0.1, length =300)
  T_ <- seq(1/365, 1, length =300)
  r_ <- seq(-10, 10, length = 300)
  b_ <- seq(-0.5, 0.5, length = 300)
  
  p1 <- lapply(S_, dnt1, K = 1000000, U = 0.96, L = 0.92, sigma =0.06, Time = 0.25, r=0.0025, b = -0.025)
  p2 <- lapply(K_, dnt1, S = 0.9266, U = 0.96, L = 0.92, sigma =0.06, Time = 0.25, r=0.0025, b = -0.025)
  p3 <- lapply(U_, dnt1, S = 0.9266, K = 1000000, L = 0.92, sigma =0.06, Time = 0.25, r=0.0025, b = -0.025)
  p4 <- lapply(L_, dnt1, S = 0.9266, K = 1000000, U = 0.96, sigma =0.06, Time = 0.25, r=0.0025, b = -0.025)
  p5 <- lapply(sigma_, dnt1, S = 0.9266, K = 1000000, U = 0.96, L = 0.92, Time = 0.25, r=0.0025, b = -0.025)
  p6 <- lapply(T_, dnt1, S = 0.9266, K = 1000000, U = 0.96, L = 0.92, sigma =0.06, r=0.0025, b = -0.025)
  p7 <- lapply(r_, dnt1, S  =0.9266, K = 1000000, U = 0.96, L = 0.92, sigma =0.06, Time = 0.25, b = -0.025)
  p8 <- lapply(b_, dnt1, S = 0.9266, K = 1000000, U = 0.96, L = 0.92, sigma =0.06, Time = 0.25, r=0.0025)
  dev.new(width = 20, height= 10)
  
  par(mfrow = c(2,4), mar = c(2,2,2,2))
  
  plot(S_, p1, type = "l", xlab = "", ylab ="", main = "S")
  plot(K_, p2, type = "l", xlab = "", ylab ="", main = "K")
  plot(U_, p3, type = "l", xlab = "", ylab ="", main = "U")
  plot(L_, p4, type = "l", xlab = "", ylab ="", main = "L")
  plot(sigma_, p5, type = "l", xlab = "", ylab ="", main = "sigma")
  plot(T_, p6, type = "l", xlab = "", ylab ="", main = "T")
  plot(r_, p7, type = "l", xlab = "", ylab ="", main = "r")
  plot(b_, p8, type = "l", xlab = "", ylab ="", main = "b")
}

implied_vol_DNT <- function(S = 0.9266, K = 1000000, U = 0.96, L = 0.92, Time = 0.25, r=0.0025, b=-0.025,price){
  f <- function(sigma)
    dnt1(S,K,U,L,sigma,Time,r,b) - price
  uniroot(f, interval = c(0.001, 100))$root}

implied_U_DNT <- function(S = 0.9266, K = 1000000, L = 0.92, sigma = 0.06, Time = 0.25, r = 0.0025, b = -0.025, price = 4){
  f <- function(U)
    dnt1(S, K, U, L, sigma, Time, r, b) - price
  uniroot(f, interval = c(L+0.01, L +100))$root
}

implied_T_DNT <- function(S =0.9266, K = 1000000, U = 0.96, L = 0.92, sigma = 0.06, r = 0.0025, b= -0.025, price=4){
  f <- function(Time)
    dnt1(S, K, U, L, sigma, Time, r, b) - price
  uniroot(f, interval = c(1/365, 100))$root  }


#install.packages("rootSolve")
library(rootSolve)

implied_DNT_image()
print(implied_vol_DNT(price = 6))
print(implied_U_DNT(price = 4))
print(implied_T_DNT(price = 30))
