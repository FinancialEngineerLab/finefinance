#### Ch7. Risk Management ####

library(quantmod)
library(tseries)
library(PerformanceAnalytics)
install.packages("SACCR") #바젤규제 패키지
library(SACCR)
library(caret)
library(caTools)
library(ROCR)
library(randomForest)
### 7-1 시장리스크 ###
## Beta로 측정 ##
#Data -> KOSDAQ ETF와 KOSPI ETF
getSymbols("232080.KS", from = "2016-01-01")
getSymbols("277630.KS", from = "2016-01-01")
#Variables
KOSDA <- `232080.KS`
KOSPI <- `277630.KS`
KOSDA <- KOSDA$`232080.KS.Close`
KOSPI <- KOSPI$`277630.KS.Close`
KOSDA <- na.omit(KOSDA)
KOSPI <- na.omit(KOSPI)
KOSDA_m <- to.monthly(KOSDA)[,"KOSDA.Close"]
KOSPI_m <- to.monthly(KOSPI)[,"KOSPI.Close"]
KOSDA_r_m <- Delt(KOSDA_m)
KOSPI_r_m <- Delt(K
                  OSPI_m)

#기간
KOSDA_r_m <- KOSDA_r_m[(index(KOSDA_r_m)>="2017-10")]
KOSPI_r_m <- KOSPI_r_m[(index(KOSPI_r_m)>="2017-10")]

#결과
betafit <- lm(KOSPI_r_m ~ KOSDA_r_m)
result <- summary(betafit)
result
result$coefficients[2,1]

### 7-2 포트폴리오 리스크 ###

getSymbols("099140.KS")
getSymbols("117680.KS")
getSymbols("226490.KS")
getSymbols("140710.KS")

China <- `099140.KS`
Steel <- `117680.KS`
KOSPI <- `226490.KS`
Trans <- `140710.KS`

China <- na.omit(China)
Steel <- na.omit(Steel)
KOSPI <- na.omit(KOSPI)
Trans <- na.omit(Trans)

China_m <- to.monthly(China)[,"China.Close"]
Steel_m <- to.monthly(Steel)[,"Steel.Close"]
KOSPI_m <- to.monthly(KOSPI)[,"KOSPI.Close"]
Trans_m <- to.monthly(Trans)[,"Trans.Close"]

China_m_r <- Delt(China_m)
Steel_m_r <- Delt(Steel_m)
KOSPI_m_r <- Delt(KOSPI_m)
Trans_m_r <- Delt(Trans_m)

China_m_r <- China_m_r[(index(China_m_r) >= "2016-01")]
Steel_m_r <- Steel_m_r[(index(Steel_m_r) >= "2016-01")]
KOSPI_m_r <- KOSPI_m_r[(index(KOSPI_m_r) >= "2016-01")]
Trans_m_r <- Trans_m_r[(index(Trans_m_r) >= "2016-01")]

China_m_r_p <- China_m_r - 0.0175
Steel_m_r_p <- Steel_m_r - 0.0175
KOSPI_m_r_p <- KOSPI_m_r - 0.0175
Trans_m_r_p <- Trans_m_r - 0.0175

MeanSD <- rbind(cbind("China ETF", mean(China_m_r_p), sd(China_m_r_p)),
                cbind("Steel ETF", mean(Steel_m_r_p), sd(Steel_m_r_p)),
                cbind("KOSPI ETF", mean(KOSPI_m_r_p), sd(KOSPI_m_r_p)),
                cbind("Trans ETF", mean(Trans_m_r_p), sd(Trans_m_r_p)))
MeanSD

lm1 <- lm(China_m_r_p ~ KOSPI_m_r_p)
lm2 <- lm(Steel_m_r_p ~ KOSPI_m_r_p)
lm3 <- lm(Trans_m_r_p ~ KOSPI_m_r_p)

return_avg <- matrix(c((lm1$coefficients[2]*mean(KOSPI_m_r_p)),
                       (lm2$coefficients[2]*mean(KOSPI_m_r_p)),
                       (lm3$coefficients[2]*mean(KOSPI_m_r_p))),nrow=1)
covariance <- cov(Data) # 원래는 Single Index Model대로 해야하나 일단 이렇게함 
covariance <- matrix(c(covariance), nrow=3)
sol <- portfolio.optim(x = return_avg, covmat = covariance, shorts = F)
sol$pw # Weight


### 7-3 Value at Risk (VaR) ###

## 7-3-1 파라미터 VaR - 분산공분산법 ##

mean = 2
sigma = 4
alpha = 0.05
Var_paramatic = qnorm(alpha, mean, sigma)
Var_paramatic

#Expected Short fall
alpha_z = qnorm(alpha)
ES_paramatic = mean + sigma * (dnorm(alpha_z)/(1-alpha))
ES_paramatic

## 7-3-2 역사적 VaR ##
#Data
symbollist = c("AMD", "AAPL", "ORCL")
getSymbols(symbollist, form = "2017-01-01", to = "2019-01-10")
AMD = AMD[,"AMD.Adjusted", drop = F]
AAPL = AAPL[,"AAPL.Adjusted", drop = F]
ORCL = ORCL[,"ORCL.Adjusted", drop = F]
AMD_return = CalculateReturns(AMD, method = "log")
AAPL_return = CalculateReturns(AAPL, method = "log")
ORCL_return = CalculateReturns(ORCL, method = "log")
AMD_return = AMD_return[-1,]
AAPL_return =AAPL_return[-1,]
ORCL_return = ORCL_return[-1,]
bind_return <- cbind(AMD_return, AAPL_return, ORCL_return)
head(bind_return)
# 역사적 VaR
HVAR <- VaR(bind_return, p=0.95, method = "historical")
HVAR
# ES
HCVAR <- ES(bind_return, p =0.95, method = "historical")
HCVAR
# component VaR
VaR(bind_return, p=0.95, portfolio_method = "component")
# Margninal VaR
VaR(bind_return, p=0.95, portfolio_method = "marginal")

### 7-4 몬테카를로 시뮬레이션 ###

Sample_Size <- 2000
set.seed(2345)
Z <- rnorm(Sample_Size)
mean <- 0.2
sigma <- 0.25
deltat <- 0.0833333
returns <- mean*deltat + sigma*Z*sqrt(deltat)
hist(returns, breaks =50)
#
Mean_new <- mean(returns) *12
Mean_new
std_new <- sd(returns)*(12)^(0.5)
std_new
#
VaR(returns, p = 0.95, method = "historical")

### 7-5 바젤 ###

CalcEAD(50, 400) # 대체비용, 예상미래익스포져 순

### 7-6  개인 신용리스크 ###
data("GermanCredit")
LRData <- GermanCredit[,1:10]
str(LRData)
summary(LRData)
#
set.seed(100)
res = sample.split(LRData$Class, 0.6)
Train_data = subset(LRData, res == TRUE)
Test_data = subset(LRData, res == FALSE)

lgfit = glm(Class ~. , data = Train_data, family  = "binomial")
summary(lgfit)
#유의한 변수 따로
lgfit = glm(Class ~ Duration + InstallmentRatePercentage + Age, family = "binomial", data = Train_data)
summary(lgfit)
#
Train_data$predicted.risk = predict(lgfit, newdata = Train_data, type  = "response")
table(Train_data$Class, as.numeric(Train_data$predicted.risk >=0.05))
#
pred= prediction(Train_data$predicted.risk, Train_data$Class)
as.numeric(performance(pred, "auc")@y.values)
# 그래프
predict_Train = predict(lgfit, type = "response")
ROCpred = prediction(predict_Train, Train_data$Class)
ROCperf = performance(ROCpred, "tpr", "fpr")
plot(ROCperf)

### 7-7 사기탐지 ###

data("GermanCredit")
FraudData <- GermanCredit[,1:10]
head(FraudData)
#
len <- dim(FraudData)[1]
train <- sample(1:len, 0.8*len)
TrainData <- FraudData[train,]
TestData <- FraudData[-train,]
fraud_model <- randomForest(Class ~. , data=TrainData, ntree =50, proximity = TRUE)
# 결과
print(fraud_model)
plot(fraud_model)
importance(fraud_model)
#
TestPred <- predict(fraud_model, newdata = TestData)
table(TestPred, TestData$Class)
