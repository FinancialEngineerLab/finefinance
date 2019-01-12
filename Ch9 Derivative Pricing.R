##### Ch9. 파생상품 가격결정 #####

library(fOptions)
install.packages("RQuantLib", type = 'binary')
library(RQuantLib)
install.packages("CreditMetrics")
library(CreditMetrics) #신용파생상품
install.packages("credule")
library(credule) #신용파생상품
install.packages("GUIDE") #금리파생상품
library(GUIDE)
install.packages("fExoticOptions")
library(fExoticOptions)
### 9-1 바닐라 옵션 ###

## 9-1-1 블랙숄즈 ##

GBSOption(TypeFlag = "c", S = 900, X = 950, Time = 1/4, r =0.02, sigma =0.22, b= 0.02)
GBSOption(TypeFlag = "p", S = 900, X = 950, Time = 1/4, r = 0.02, sigma = 0.22, b = 0.02)
# type : c or p, 기초자산, 행사가격, 만기, 무위험이자율, 변동성, 보유비용순

## 9-1-2 Cox Ross Rubinstein -> 이항모형 ##

CRRBinomialTreeOption(TypeFlag = "ce", S =900, X =950, Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22, n = 3)
CRRBinomialTreeOption(TypeFlag = "pe", S = 900, X = 950, Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22, n = 3)
#
model <- BinomialTreeOption(TypeFlag = "ce", S = 900, X = 950, Time  = 1/4, r = 0.02, b = 0.02, sigma =0.22, n=3)
BinomialTreePlot(model, dy =1, xlab = "Time steps", ylab = "Option Value", xlim = c(0,4), ylim=c(-3,4))
title(main  = "Call Option Tree")
##함수로 정의
func <- function(n){
  pr <- CRRBinomialTreeOption(TypeFlag = "ce", S = 900, X = 950, Time = 1/4, r = 0.02, b =0.02, sigma = 0.22, n = n)@price
  return(pr)
}
# 최종 비교 그래프 
price <- sapply(1:100, func) # 1~100반복 
plot(price, type="l", xlab = "Number of steps", ylab = "Option Value")
bs_price <- GBSOption(TypeFlag = "c", S = 900, X = 950, Time = 1/4, r = 0.02, sigma = 0.22, b = 0.02)@price
abline(h = bs_price, col = 'red')
legend("topright", legend = c('CRR-price', 'BS-price'), col = c('black', 'red'), pch = 19)
title(main = "Call Option Pricing models")

## Greeks
GBSGreeks(Selection = "delta", TypeFlag = "c", S = 900, X = 950, Time = 1/4, r = 0.02, b = 0.02, sigma = 0.22)
GBSGreeks(Selection = "gamma", TypeFlag = "c", S = 900, X = 950, Time = 1/4, r = 0.02, b = 0.02, sigma  =0.22)
#
portfolio <- sapply(c('c', 'p'), function(otype){sapply(500:1500, function(price){
  GBSGreeks(Selection = 'delta',
            TypeFlag = otype,
            S=price, X = 950,
            Time = 1/4, r = 0.02,
            b = 0.02,
            sigma =0.22)
})
})

head(portfolio)

## Straddle Delta
plot(500:1500, rowSums(portfolio), type = 'l', xlab = 'underlying Price', ylab = 'Straddle Delta')

##Implied Volatility ###

iv <- EuropeanOptionImpliedVolatility("call", 11.10, 100, 100, 0.01, 0.03 ,05, 0.4)
#순서 (옵션type, 옵션가격, 기초자산, 행사가격, 배당수익률, 무위험수익률, 잔존만기, 변동성initial)
iv
iv_a <- AmericanOptionImpliedVolatility("call", 11.10, 100, 100, 0.01, 0.03 ,05, 0.4)
iv_a

### 9-2 신용파생상품  ###
rc <- c("AAA", "AA", "A", "BBB", "BB", "B", "CCC", "Default")
M <- matrix(c(90.81, 8.33, 0.68, 0.06, 0.08, 0.02, 0.01, 0.01,
              0.70, 90.65, 7.79, 0.64, 0.06, 0.13, 0.02, 0.01,
              0.09, 2.27, 91.05, 5.52, 0.74, 0.26, 0.01, 0.06,
              0.02, 0.33, 5.95, 85.93, 5.30, 1.17, 1.12, 0.18,
              0.03, 0.14, 0.67, 7.73, 80.53, 8.84, 1.00, 1.06,
              0.01, 0.11, 0.24, 0.43, 6.48, 83.46, 4.07, 5.20,
              0.21, 0, 0.22, 1.30, 2.38, 11.24, 64.86, 19.79,
              0, 0, 0, 0, 0, 0, 0, 100
              )/100, 8, 8, dimnames = list(rc, rc), byrow = TRUE)
lgd <- 0.2 # 부도시손실률
cm.cs(M, lgd) #신용스프레드
#
ead <- c(140000,100000,100000) # 부도시 exposure
N <- 3 # firms
n <- 50000 #난수
r <- 0.03 #rf
rating <- c("BBB", "AA", "B")
firmnames <- c("Blizzard", "Activision", "Nexon")
alpha <- 0.99
rho <- matrix(c(1,0.4,0.6,0.4,1,0.5,0.6,0.5,1),3,3,dimnames=list(firmnames, firmnames),byrow=TRUE)
cm.CVaR(M, lgd, ead, N, n,r,rho,alpha,rating) #신용VaR
pnl <- cm.gain(M, lgd, ead, N, n, r, rho,rating) # 시뮬레이션 신용VaR
pnl

## CDS ##

yct = c(1,2,3,4,5,7) #테너 (테너 : 채무발생일 ~ 만기일간 기한, 결제기간이라고함) 
ycr = c(0.0050, 0.0070, 0.0080, 0.0100, 0.0120, 0.0150) #수익률곡선할인률 
cct = c(1,3,5,7) #테너 
ccsp = c(0.99, 0.98, 0.95, 0.92) #생존확률 
tenors = c(1,3,5,7) #만기
r = 0.4 #회수율 
priceCDS(yct, ycr, cct, ccsp, tenors, r)
#
cdsSpreads = c(0.0050, 0.0070, 0.0090, 0.0110)
bootstrapCDS(yct,ycr,cct,ccsp,r)

## 금리파생상품 ##
irswapvalue()


### 9-3 Exotic Option ###

## 9-3-1 Asian Option  - 평균가 ##
#1번 방법 
price <- GeometricAverageRateOption("c", 110, 120, 0.5, 0.03, 0.05 ,0.1)
price
#2번 방법
TurnbullWakemanAsianApproxOption(TypeFlag = "p", S = 100, SA = 102, X = 120, Time = 0.5,
                                 time = 0.25, tau = 0, r = 0.03, b = 0.05, sigma = 0.1)@price
#3번 방법
LevyAsianApproxOption(TypeFlag = "p", S = 100, SA = 102, X = 120, Time = 0.5,
                      time = 0.25, r = 0.03, b = 0.05, sigma = 0.1)@price

## 9-3-2 Barrier Option ##

## 1번 Down and Out
StandardBarrierOption(TypeFlag = "cdo", S  =100, X=90, H = 95, K = 3, Time=0.5, r = 0.08, b = 0.04, sigma = 0.25)@price
#H는 경계값, K는 리베이트값

##2번 Up and out down and out call (Double Barrier)
DoubleBarrierOption(TypeFlag = "co", S = 100,X=100, L = 50, U = 150, Time =0.25,
                    r = 0.1 ,b=0.1, sigma = 0.15, delta1 = -0.1, delta2 = 0.1)@price
#L하한, U상한, 그에 맞는 Delta1, Delta2

##3번 룩백 Barrier Up and out
LookBarrierOption(TypeFlag = "cuo", S = 100, X = 100, H = 130, time1 = 0.25, Time2 = 1, r=0.1,b=0.1, sigma=0.15)@price

##4번 Gap Digital
GapOption(TypeFlag = "c", S = 50, X1= 50, X2= 57, Time = 0.5, r = 0.09, b = 0.09, sigma = 0.20)
#두개의 행사가격

## 5번 Cash or Nothing : 만기시점에 기초자산가격이 행사가격에 도달할 경우 미리 정한가 지불 ##
CashOrNothingOption(TypeFlag = "p", S = 100, X = 80, K = 10, Time = 9/12, r = 0.06, b = 0, sigma = 0.35)

## 6번 Two ASSet cash or nothing down-up 옵션 ##
# 1번 자산 가격 > 행사가격 & 2번 자산가격 > 행사가격 일 경우 고정된 현금 지불

TwoAssetCashOrNothingOption(TypeFlag = "c", S1 = 100, S2 = 100, X1 = 110, X2 = 90,
                            K = 10, Time = 0.5, r=0.1, b1 = 0.05, b2= 0.06, sigma1 = 0.2, sigma2=0.25,
                            rho = 0.5)@price
# K는 만기시 지불하는 그 고정현금액
# rho는 상관관계 