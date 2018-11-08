rm(list=ls())

# Loading raw data -------------------------------------------------------------
# Index : KOSPI200 수익률 환산 
# Stock : 현대제철 NC소프트 현대건설 
# Risk free rate : CD금리 (3개월) 5년 평ㄱ
# Prior Authour : Eunsoo Park
# revisr : Hyunjin Shin균

raw.data <- read.csv('C:/Users/Shinhyunjin/Dropbox/total52.csv', header=T, fileEncoding="UTF-8-BOM")
#nrow(raw.data)
raw.data <- raw.data[1:61,-1]
#raw.data

# Monthly retrun, excess return, risk free rate --------------------------------
return.m <- raw.data$KOSPI200
return.v <- var(raw.data$KOSPI200)
# INDEX & Stocks
r.free   <- 0.018             # risk free rate (CD금리 평균) 
excess.return.m <- return.m - r.free    # excess return of INDEX & Stocks

# least square
out.HCONST    <- lm(HCONST~KOSPI200, data=raw.data)
out.NCSOFT <- lm(NCSOFT~KOSPI200, data=raw.data)
out.HSTEEL     <- lm(HSTEEL~KOSPI200, data=raw.data)

# alpha, beta & variance of each stock
alpha <- c(coef(out.HCONST)[1],
           coef(out.NCSOFT)[1],
           coef(out.HSTEEL)[1])
beta  <- c(coef(out.HCONST)[2],
           coef(out.NCSOFT)[2],
           coef(out.HSTEEL)[2])
variance <- c(deviance(out.HCONST),
              deviance(out.NCSOFT),
              deviance(out.HSTEEL))
resivar <- c((deviance(out.HCONST)-coef(out.HCONST)[2]**2*return.v), 
             (deviance(out.NCSOFT)-coef(out.NCSOFT)[2]**2*return.v),
             (deviance(out.HSTEEL)-coef(out.HSTEEL)[2]**2*return.v))

out.m <- cbind(alpha, beta, variance, resivar) # monthly
rownames(out.m) <- c('현대제철', '엔씨소프트', '현대건설')
colnames(out.m) <- c('alpha', 'beta', 'total variance', 'residual variance')

out.a <- out.m%*%diag(c(12,1,12,12))     # annualized
rownames(out.a) <- c('현대제철', '엔씨소프트', '현대건설')
colnames(out.a) <- c('alpha', 'beta', 'total variance', 'residual variance')

# 알파와 마켓리스크프리미엄 입력 #
RP.M.a  <- 0.06 # risk premium of the index(6%)
vol.M.a <- 0.20 # standard deviation of risk preminum(20%)

# risk premiume of stocks using beta estimated & market premium(6%)
RP.Stock.a <- out.a[,'beta']*RP.M.a

# assumed alpha #
alpha.a.1 <- 0.03
alpha.a.2 <- 0.07
alpha.a.3 <- 0.1

# expected return for each stock #
ER.Stock.a.1 <- alpha.a.1 + RP.Stock.a
ER.Stock.a.2 <- alpha.a.2 + RP.Stock.a
ER.Stock.a.3 <- alpha.a.3 + RP.Stock.a

# function for Sharpe Ratio
Sharpe.ratio <- function(w, RP, V){
  w <- c(w, 1-sum(w)) # short sale allowed
  RP.p <- w %*% RP
  s.p  <- sqrt(w%*%V%*%w)
  return(RP.p/s.p)
}

w <- c(0.1, 0.1, 0.1) # initial weight

# risk premium of stocks & market index
RP.1 <- c(alpha.a.1 + RP.Stock.a, RP.M.a) # for alpha = 0.03
RP.2 <- c(alpha.a.2 + RP.Stock.a, RP.M.a) # for alpha = 0.05
RP.3 <- c(alpha.a.3 + RP.Stock.a, RP.M.a) # for alpha = 0.7

# covariance matrix
beta.p <- matrix(c(out.a[,'beta'], 1), 4,1)
V <- beta.p %*% t(beta.p) * vol.M.a^2
V <- V + diag(4) * c(out.a[,'variance'], 0)

# max Sharpe ratio
out.1 <- optim(w, Sharpe.ratio, control = list(fnscale=-1), RP=RP.1, V=V)
out.2 <- optim(w, Sharpe.ratio, control = list(fnscale=-1), RP=RP.2, V=V)
out.3 <- optim(w, Sharpe.ratio, control = list(fnscale=-1), RP=RP.3, V=V)

# max Sharpe ratio에서 market - Passive Strategy 비중 
out.1.m <- 1-sum(out.1$par)
out.2.m <- 1-sum(out.2$par)
out.3.m <- 1-sum(out.3$par)

# short sale안되면 constOptim : input / constraint : 선형에서 쓴다.
#value가 sharpe지수고, par미터가 1,2,3가 주식의 비중. 인덱스는 1-parmeterㅎ
# constriant 에서 shorsale 이 안되면 제곱성질을 이용 #

# Sharpe ration of index 기준점 #
RP.M.a/vol.M.a


#### 참고 : Active & passive 비중  #### 
# security
ws1.1 <- (alpha.a.1)/resivar[1]
ws2.1 <- (alpha.a.1)/resivar[2]
ws3.1 <- (alpha.a.1)/resivar[3]

ws1.2 <- (alpha.a.2)/resivar[1]
ws2.2 <- (alpha.a.2)/resivar[2]
ws3.2 <- (alpha.a.2)/resivar[3]

ws1.3 <- (alpha.a.3)/resivar[1]
ws2.3 <- (alpha.a.3)/resivar[2]
ws3.3 <- (alpha.a.3)/resivar[3]

# security normal
wa1.1 <- ws1.1/(ws1.1 + ws2.1 + ws3.1)
wa2.1 <- ws2.1/(ws1.1 + ws2.1 + ws3.1)
wa3.1 <- ws3.1/(ws1.1 + ws2.1 + ws3.1)

wa1.2 <- ws1.2/(ws1.2 + ws2.2 + ws3.2)
wa2.2 <- ws2.2/(ws1.2 + ws2.2 + ws3.2)
wa3.2 <- ws3.2/(ws1.2 + ws2.2 + ws3.2)

wa1.3 <- ws1.3/(ws1.3 + ws2.3 + ws3.3)
wa2.3 <- ws2.3/(ws1.3 + ws2.3 + ws3.3)
wa3.3 <- ws3.3/(ws1.3 + ws2.3 + ws3.3)

#active

was.1 <- ((wa1.1*alpha.a.1+wa2.1 * alpha.a.1 + wa3.1 * alpha.a.1)/(wa1.1**2*resivar[1]+wa2.1**2*resivar[2]+wa3.1**2*resivar[3]))/((excess.return.m)/return.v)
was.2 <- ((wa1.2*alpha.a.2+wa2.2 * alpha.a.2 + wa3.2 * alpha.a.2)/(wa1.2**2*resivar[1]+wa2.2**2*resivar[2]+wa3.2**2*resivar[3]))/((excess.return.m)/return.v)
was.2 <- ((wa1.3*alpha.a.3+wa2.3 * alpha.a.3 + wa3.3 * alpha.a.3)/(wa1.3**2*resivar[1]+wa2.3**2*resivar[2]+wa3.3**2*resivar[3]))/((excess.return.m)/return.v)


#Beta -> active weight #
pfbeta1 <- wa1.1 * beta[1] + wa2.1 * beta[2] + wa3.1 * beta[3]
pfbeta2 <- wa1.2 * beta[1] + wa2.2 * beta[2] + wa3.2 * beta[3]
pfbeta3 <- wa1.3 * beta[1] + wa2.3 * beta[2] + wa3.3 * beta[3]

wasb.1 <- was.1 / ((1+(1-pfbeta1)*was.1))
wasb.2 <- was.2 / ((1+(1-pfbeta2)*was.2))
wasb.3 <- was.3 / ((1+(1-pfbeta3)*was.3))

#market - passive weight #

wm.1 <- 1-wasb.1
wm.2 <- 1-wasb.2
wm.3 <- 1-wasb.3
