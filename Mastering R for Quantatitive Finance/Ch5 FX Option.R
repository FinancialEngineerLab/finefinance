##### Ch5 FX Derivatives #####

library(Quandl)
library(xts)
library(quantmod)
library(fOptions)

getSymbols("KRW=X", from = "2018-01-01")
getSymbols("KRWUSD=X", from = "2018-01-01")

krwusd <- `KRW=X`
usdkrw <- `KRWUSD=X`

krwusd <- krwusd$`KRW=X.Close`
usdkrw <- usdkrw$`KRWUSD=X.Close`

dev.new(width = 15, height = 8)
par(mfrow = c(1,2))
plot(krwusd)
plot(usdkrw)

krwusd[1:5,] #행
usdkrw[1:5,] #행


#### Currency Option ####

BlackScholesOption("c", 0.745, 0.7,5,0.03,0.01,0.2) # Type S X Mat r d sigma
BlackScholesOption("p", 0.745, 0.7, 5, 0.03, 0.01, 0.2)


#### Exchange Option ####
## 2D Wiener Processes ##

D2_wiener <- function(){
  dev.new(width=10, height = 4)
  par(mfrow=c(1,3), oma = c(0,0,2,0))
  for(i in 1:3){
    W1 <- cumsum(rnorm(100000))
    W2 <- cumsum(rnorm(100000))
    plot(W1, W2, type = "l", ylab = "", xlab = "")
  }
  mtext("2-D Wiener Processes with no correlation", outer = TRUE, cex = 1.5, line = -1)
}

D2_wiener()

## Correlation ##

Correlated_Wiener <- function(cor){
  dev.new(width =10, height = 4)
  par(mfrow=c(1,3), oma=c(0,0,2,0))
  for(i in 1:3){
    W1 <- cumsum(rnorm(100000))
    W2 <- cumsum(rnorm(100000))
    W3 <- cor * W1 + sqrt(1-cor^2) * W2
    plot(W1, W3, type = "l", ylab = "", xlab = "")
  }
  mtext(paste("2D Wiener-processes(", cor, " Correlation)", sep = ""), outer = TRUE, cex = 1.5, line=-1)
}
Correlated_Wiener(0.6)
Correlated_Wiener(0.4)
Correlated_Wiener(-0.5)




##### Margrabe Function #####

Margrabe <- function(S1, S2, sigma1, sigma2, Time, rho, delta1 = 0, delta2 = 0){
  sigma <- sqrt(sigma1^2  + sigma2^2 - 2*sigma1* sigma2 * rho)
  d1 <- (log(S1/S2) + (delta2 - delta1 + sigma^2/2)*Time)/(sigma*sqrt(Time))
  d2 <- (log(S1/S2) + (delta2 - delta1 - sigma^2/2)*Time)/(sigma*sqrt(Time))
  M <- S1 * exp(-delta1 * Time)*pnorm(d1) - S2*exp(-delta2*Time)*pnorm(d2)
  return(M)
}

if (min(S1,S2)<=0) stop("prices must be positive")

Margrabe(100, 120, 0.2, 0.3, 2, 0.15)
Margrabe(100,120,0.2,0,2,0,0,0.03)

# Compare to BS

BlackScholesOption("c", 100, 120, 2, 0.03, 0.03,0.2)

Margrabe(100, 120, 0, 0.2, 2, 0, 0.03, 0)
BlackScholesOption("p", 120, 100, 2, 0.03, 0.03, 0.2)


### Currency Option with Margrabe ###

Margrabe(0.745, 0.7, 0.2, 0, 5, 0.15, 0.02, 0.03)

# Correlation affects option value
x <- seq(-1, 1,length =1000)
y <- rep(0, 1000)
x
y
for(i in 1:1000){
  y[i] <- Margrabe(100, 120, 0.2, 0.3, 2, x[i])
}

plot(x, y, xlab = "Correlation", ylab  ="Price", main = "Price of Exchange Option", type = "l", lwd =3)

#### Quanto Option ####
## Quanto : Quantity adjusting option ##
# if Stock price is higher than Exercise price, the differecne(S-X) = Foreign Currency more received

Margrabe(74.67, 90*0.7467, 0.2, 0.3, 0, 0.007, 0.3)
# S, X*FXrate, sigma1, sigma2, T, rho, delta1, delta2  
#S1 : 외국주식가격
#S2 : 외화행사가격
#delta1: 외국r - 내국r - 외국v * 내국v * 상관관계
#deltat2  : 외국r
#시그마 : 내국시그마