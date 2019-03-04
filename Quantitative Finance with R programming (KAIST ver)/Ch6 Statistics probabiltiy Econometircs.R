#### Ch6 확률 통계 계량경제학 ####

### 6.1 확률 및 확률과정 ###

## 6.1.1. 난수 ##

x <- runif(100,0,1)
y <- runif(100,0,1) # unifrom distribution; 균등분포
plot(x,y)

set.seed(1)
x <- 1000*runif(5)
sprintf("$ %3.2f",x)

x <- rnorm(1000)
hist(x)

## 6.1.2 확률과정 ##

#1. random walk #

set.seed(1)
x <- runif(100,-1,1)
y <- runif(100,-1,1)
xs <- cumsum(x)
ys <- cumsum(y)
plot(xs,ys)

## 6.1.3 확률미분방정식 ##

install.packages("sde")
library(sde)

plot(BM()) #Brownian motion simulation 

x <- BM(0,0,1,100)
plot(x)
plot(GBM(1,1,0.2,1,100)) #GBM : Geometric Brownian Motion

### 6.2 통계 ###

##1. 기술, 요약통계 ##

set.seed(1)
x <- rnorm(1000)
summary(x)

install.packages("pastecs")
library(pastecs)
score <- c(88,43,57)
score <- rbind(score, c(90,80,70))
score <- rbind(score, c(70,80,75))
print(score)
print(stat.desc(score)) ## 기술통계 요약 험수 !!

## 6.3 계량경제학 ##

set.seed(1)
x <- rnorm(1000)
y <- (x-2)+rnorm(1000)

ans <- lm(y~x)
summary(ans)
plot(x,y)
abline(ans, col = 'brown', lwd =2)

#

require(polynom)
f <- as.function(polynomial(ans$coefficients))
names(ans)
summary(ans)
summary(ans)$coefficient[2,1]

## 6.3.2 다변량회귀분석 ##

x <- rnorm(100)
y <- 2*x + rnorm(100)
z <- x+y
reg <- lm(z~x+y)

## 6.3.3. MLE 최우추정법 ##

library(stats4)
set.seed(1)
x <- rnorm(1000, mean = 5, sd = 2)
log.lik <- function(mu,sigma) sum(dnorm(x, mu, sigma, log = TRUE))
fit <- mle(log.lik, lower = c(0,0), method = "L-BFGS-B")
fit

hist(x)
f <- function(z) dnorm(z, 4.976, 2.06)*length(x)
curve(f, col = 'red', add= T)

## 6.3.4 이분산성과 자기상관 ##

library(sandwich)
library(car)
library(lmtest)

# 이분산x
set.seed(1)
x <- 1:100
y <- 1+ 0.2*x + rnorm(100)*mean(x)/2
model1 <- lm(y~x)
vcv <- vcov(model1)
vcv.nw <- NeweyWest(model1)
print(coeftest(model1, vcv))
print(coeftest(model1, vcv.nw))

# 이분산o
set.seed(1)
w <- 1:100
z <- 1+ 0.2*x + rnorm(100)*mean(x)/2
model2 <- lm(z~w)
vcv <- vcov(model2)
vcv.nw <- NeweyWest(model2)
print(coeftest(model2, vcv))
print(coeftest(model2, vcv.nw))
par(mfrow = c(1,2))
plot(x,y)
abline(model1, col = 'red')
plot(w,z)
abline(model2, col = 'red')

par(mfrow = c(1,2))
plot(model1$residuals)
plot(model2$residuals)

## 6.3.5 구조적 변화 ##

install.packages('strucchange')
library(strucchange)
require(strucchange)
set.seed(1)
x <- arima.sim(n=100, list(ar=0.5))
data <- cbind(x, lx = lag(x))
data
sctest(x~lx, data = data, type = "Chow", point=10)

#

y <- c(rep(0,5), rep(1,5))
x <- breakpoints(Fstats(y~1))

plot(y, type = 'b')
abline(v=x$breakpoints[1], col= 'red', lwd =2)

### 6.4 Time Series Analysis ###

## 6.4.1 time ##

## 6.4.2 object ##

x <- as.Date("2015-06-29")
y <- as.Date("2016-6-29")
x
y
z <- as.Date("20150629", format = "%Y%m%d")
z

# basic date #

x <- as.Date(32500, origin = as.Date("1900-1-1"))
x

# extract

xyear <- format(x, "%Y")
xyear
weekdays(x)
x+7
x > (x-2)
y-x

# seq function #

seq(as.Date("2000-1-1"), as.Date("2000-6-1"), "2 months")
seq(as.Date("2000-1-1"), as.Date("2000-6-1"), "months")
seq(Sys.Date(), by="2 weeks", length.out=2)[2]


### 6.4.3 POSIXt 클래스 ###

x <- "2015-6-1 12:11:13"
y <- as.POSIXct(x) #숫자 
y
class(y)
as.numeric(y)

#현지시간
Sys.timezone()

x <- "2016-6-1 12:11:13"
z <- as.POSIXlt(x) #구조체 
z

names(unclass(z))
z$mon

#POSIXct와 lt간 전환하자 

w <- as.POSIXct(z)
class(w)

x <- "2015-6-29 11:12:13.123"
y <- as.POSIXct(x)
y

options(digits.sec=3) #초이하 단위 option
y

## 6.4.4 lubridate ##

install.packages("lubridate")
library(lubridate)

x<-now()
yday(x) # 연중 날

x <- now()
x
year(x) <- 2014
x

## 6.4.5 시계열 데이터 다루기 ##

library(tseries)
wti <- get.hist.quote("130680.KS", start = "2018-12-01") # 	130680: TIGER WTI ETF
inver <- get.hist.quote("217770.KS", start = "2018-12-01") # 217770	: TIGER WTI inverse

wti
inver

# 종가 추출 

wti.ts <- wti$Close
inver.ts <- inver$Close

z <- window(wti.ts, start = as.Date("2018/12/3"), end = as.Date("2019/2/1"))
z
z <- wti.ts[as.Date(c("2018-12-03", "2019-02-01"))]
z

w <- diff(wti.ts,1) #뒤로 밀림
cbind(wti.ts, w)

w<-lag(wti.ts,2) # 앞으로 밀림 
cbind(wti.ts, w)

ret.wti <- diff(wti.ts, 1) / lag(wti.ts,-1)
cbind(wti.ts, ret.wti)

ret.inver <- diff(inver.ts,1)/lag(inver.ts, -1)
cbind(inver.ts, ret.inver)

data <- cbind(ret.wti, ret.inver)
data
plot(data, plot.type = 'single', type = 'l', lty = 1:2, col = 1:2)
legend('topleft', legend = c('WTI ETF', 'WTI inverse ETF'), lty = 1:2, col=1:2, bty = 'n')

par(mfrow = c(1,2))
acf(wti.ts, na.action = na.pass)
acf(inver.ts, na.action = na.pass)

fit <- arima(wti.ts, order=c(2,0,1))
fit


### 6.4.6 ARIMA ###

phi <- 0.8
cons <- 1.5

set.seed(1)
e <- rnorm(100)
x <- arima.sim(n=100, model=list(ar = phi, order = c(1,0,0)), start.innov = 4.1, n.start=1, innov=cons+e)
#상동
y<-rep(NA,100)
y[1] <- 4.1
for (i in 2:100) y[i] <- cons+phi*y[i-1]+e[i-1]
y
#상동
z <- filter(c(4.1, cons+e), filter = phi, method = "recursive")
z

par(mfrow = c(2,2))
plot(x)
plot(y, type = 'l')
plot(z)

par(mfrow = c(1,2))
acf(x)
pacf(x)

fit <- arima(x, order=c(1,0,0))
fit
mean(x)
Box.test(x, lag=1) #p values 높아 기각하여 잔차에 대한 자기상관 없음을 증명 

### appendix : linear filter ###

set.seed(1)
x <- arima.sim(n=100, model = list(ar=0.7))
trnd <- (1:100)/10
y <- x+trnd
plot(y)
f_y5 <- filter(y, filter = rep(1/5,5)) # 5일 이동 평균 
lines(f_y5, col = 'red', lwd = 2)
f_y25 <- filter(y, filter = rep(1/25, 25)) # 25일 이동 평균 
lines(f_y25, col = 'blue', lwd =2)
