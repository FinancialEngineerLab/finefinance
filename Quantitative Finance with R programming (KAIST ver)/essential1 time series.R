
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

