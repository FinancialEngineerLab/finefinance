### Ch3. Statistical Analysis ###


### 1. Linear Regression ##

data <- read.csv("C:/Users/Shinhyunjin/Dropbox/data/rdata1.csv")
head(data)

#scatter
Yprice = data$price.etf.autos
Xprice = data$price.etf.insurance
scatter.smooth(Yprice, Xprice, type="p",xlab = "ETF insur", ylab = "ETF autos")

#regression
LinearR.lm = lm(Yprice ~ Xprice, data = data)
coeffs = coefficients(LinearR.lm);coeffs

predict(LinearR.lm)

summary(LinearR.lm)$r.squared
summary(LinearR.lm)

#선형회귀모델의 신뢰구간
Predictdata = data.frame(Xprice = 8000)
predict(LinearR.lm, Predictdata, interval = "confidence") # 예측값 신뢰구간

#잔차도
LinearR.res = resid(LinearR.lm)
plot(Xprice, LinearR.res, ylab = "Residuals",xlab = "Xprice",main = "Residual Plot")
#표준화잔차도
LinearRSTD.res = rstandard(LinearR.lm)
plot(Xprice, LinearRSTD.res, ylab = "Standardized Residuals", xlab = "Xprice", main = "Residual Plot")

#오차의 정규분포
qqnorm(LinearRSTD.res, ylab = "Standardized Residuals", xlab = "Normal Scores", main = "Error Normal Distribution plot")
qqline(LinearRSTD.res)

### 2. Multiple Regression ###

data2 <- read.csv("C:/Users/Shinhyunjin/Dropbox/data/rdata1.csv")
Yprice <- data2$etf.kospi
X1price <- data2$price.etf.autos
X2price <- data2$price.etf.insurance
X3price <- data2$price.etf.energy.chemical
X4price <- data2$price.etf.bank
MultipleR.lm = lm(Yprice ~ X1price + X2price + X3price + X4price, data = data2)
summary(MultipleR.lm)

#예측값
newdata = data.frame(X1price = 13000, X2price = 7900, X3price = 11200, X4price = 7400)
predict(MultipleR.lm, newdata)
predict(MultipleR.lm, newdata, interval = "confidence")#confidence level prediction

### 3. Multicollinearity "

install.packages("car")
library(car)
vif(MultipleR.lm)

### 4. ANOVA ###
data3 <- read.csv("C:/Users/Shinhyunjin/Dropbox/data/rdata1.csv")
Yprice <- data3$etf.kospi
X1price <- data3$price.etf.autos
boxplot(Yprice ~ X1price)

oneway.test(data3$etf.kospi ~ data3$price.etf.autos, var.equal = TRUE)

### 4. Feature Selection ###
#상관관계
data4 <- read.csv("C:/Users/Shinhyunjin/Dropbox/data/rdata1.csv")
Yprice <- data4$etf.kospi
X1price <- data4$price.etf.autos
X2price <- data4$price.etf.insurance
X3price <- data4$price.etf.energy.chemical
X4price <- data4$price.etf.bank
correlationMatrix <- cor(data4[,2:5])
correlationMatrix

### 5. 단계식 변수선택 ###

install.packages("MASS")
library(MASS)
MultipleR.lm = lm(Yprice ~ X1price+X2price+X3price+X4price, data = data4)
step <- stepAIC(MultipleR.lm, direction = "both")
step$anova

#분류에의한 변수선택
install.packages("mlbench")
install.packages("caret")
library(mlbench)
library(caret)
library(randomForest)

control <-rfeControl(functions = rfFuncs, method = "cv", number = 10)
Output <- rfe(data4[,3:6],data4[,2:3], sizes = c(3:6), rfeControl = control)
predictors(Output)
plot(Output, type = c("g","o"))

###6. 웨이블릿 분석 ###

install.packages("wavelets")
library(wavelets)
library(quantmod)

getSymbols("HSPX", from = "2018-01-01", to = "2018-12-29")
HSPX

hspx <- HSPX$HSPX.Close
ret_hspx <- Delt(hspx, k=1)
par(mfrow = c(2,1))
plot(xts(hspx), type = "l")
plot(xts(ret_hspx), type = "l")

head(hspx)
tail(hspx)

#웨이블릿 변환
hspx2 <- as.ts(hspx)
model <- wavelets::dwt(hspx2, filter = "la8", n.levels = 3)
model
model@W #웨이블릿계수 
model@V #척도화계수
plot(model)

#하르필터
model2 <- wavelets::dwt(hspx2, filter = "haar", n.levels = 3)
plot(model2)

#역이산 웨이블릿
imodel <- idwt(model2, fast = TRUE)

#다중해상도분석
model3 <- mra(hspx2, filter = "la8", n.levels = 3)

#최대중첩이산웨이블릿(MODWT)
model4 <- modwt(hspx2, filter = "la8",n.levels = 5)
plot.modwt(model4)

### 7. 고속푸리에변환 (FFT) ###

model5 <- fft(hspx2)
rp = Re(model5) #실수부real part
ip = Im(model5) #허수부imaginary part
absmodel <- abs(model5) #절대값
plot(absmodel)

norm_absmodel <- absmodel[1:(length(hspx2)/2)]
Angle = atan2(ip,rp)

spec_density <- spectrum(hspx2, method = c("pgram", "ar"))

### 8. 힐버트 변환 ###

install.packages("seewave", repos="http://cran.at.r-project.org/")수
library(seewave)
model <- hilbert(hspx2,1) #1은 주파수
summary(model)

rp<-Re(model)
ip <-Im(model)
ifreq(hspx2, 1, ylim = c(0,0.0001))
ifreq(hspx2, 1, phase = "TRUE", ylim=  c(-0.5,1)) #위상을 준다 phase
phase_diff <- phase1 - phase2
#값도출
output = ifreq(hspx2, 1, plot =FALSE)
freq <- output$f
phase <- output$p
