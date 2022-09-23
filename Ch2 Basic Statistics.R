### Ch2. Basic Statistics : Modeling ###


### 1. Probability Distribution ###

#1. Normal Distribution #

Sampledata = read.csv("C:/Users/Shinhyunjin/Dropbox/data/WTI etf.csv")
head(Sampledata)

#누적밀도함수 dnorm
y <- dnorm(Sampledata$price, mean = mean(Sampledata$price), sd = sd(Sampledata$price, na.rm= FALSE))
plot(Sampledata$price, y)

#누적분포함수 pnorm
pnorm(.02, mean= mean(Sampledata$price), sd = sd(Sampledata$price, na.rm=FALSE))

#분위수함수 qnorm
qnorm(0.159837, mean= mean(Sampledata$price), sd=sd(Sampledata$price, na.rm=FALSE),lower.tail=FALSE)

#난수행성함수 rnorm
rnorm(5, mean=mean(Sampledata$price), sd = sd(Sampledata$price, na.rm =FALSE))

#2. Lognormal Distribution

#dlnorm 로그정규분포의 밀도함수

y <- dlnorm(Sampledata$volume, meanlog = mean(Sampledata$volume), sd=sd(Sampledata$volume, na.rm=FALSE)
            )
plot(Sampledata$volume, y)

#plnorm 로그정규분포의 누적확률분포함수
y <- plnorm(Sampledata$volume, meanlog = mean(Sampledata$volume), sdlog = sd(Sampledata$volume, na.r=FALSE))
plot(Sampledata$volume,y)

#qlnorm 로그정규분포의 q분위수 구하기
#rlnorm 로그정ㅂ분포의 난수생성 


##3. 포아송 ##

ppois(15, lambda = 10)
ppois(15, lambda = 10, lower= FALSE) #오른쪽 꼬리 확률 

##4. 균등분포 이론- 연속균등분포 ##

runif(10, min = 1, max= 5)

##5. 극단값 이론 ##

install.packages("POT") # peaks over threshold by generalized pareto distribution
library(POT)
#
data(ardieres)
abc <- ardieres[1:10000,]
events <- clust(abc,u=1.5, tim.cond=8/365, clust.max= TRUE)
par(mfrow = c(2,2))
mrlplot(events[,"obs"])
diplot(events)
tcplot(events[,"obs"], which = 1)
tcplot(events[,"obs"], which = 2)
#
obs <- events[,"obs"]
ModelFit <- fitgpd(obs, thresh= 5, "pwmu")
ModelFit


### 3. 표본 추출 ###

##3-1 무작위 표본추출 ##
#비복원 -> 중복을 불가하게한 무작위표본 추출 
RandomSample <- Sampledata[sample(1:nrow(Sampledata), 10, replace = FALSE),]
RandomSample
#복원 -> 중복을 허용한 무작위표본 추출 
RandomSample <- Sampledata[sample(1:nrow(Sampledata), 10, replace = TRUE),]
RandomSample

## 3-2 층화 표본추출 ##

install.packages("sampling")
library(sampling)
table(Sampledata$volume, Sampledata$price)

#다른 그룹에서 표본 추출

Stratsubset = strata(Sampledata, c("volume", "price"), size = c(2,2,2,2), method="srswor")
Stratsubset

### 4. 통계량 ###

#평균값
mean(Sampledata$price)
#중앙값
median(Sampledata$price)
#최빈값
findmode <- function(x){
  uniqx <- unique(x)
  uniqx[which.max(tabulate(match(x, uniqx)))]
}
findmode(Sampledata$price)
#요약
summary(Sampledata$price)
#적률
library(e1071)
moment(Sampledata$price, order =3, center=TRUE)
#첨도 -> 뾰족한 정도 
kurtosis(Sampledata$price)
#왜도 -> 분포의 대칭성, 평균이 중앙갑보다 작으면 left-skewed, 크면 right-skewed.
skewness(Sampledata$price) #크다 -> right-skewed네

### 5. 상관관계 ###

install.packages("Hmisc")
library(Hmisc)

x <- Sampledata[,2:3]
x<-na.omit(x)
rcorr(as.matrix(x), type = "pearson")
acf(Sampledata$price)
pacf(Sampledata$price)
ccf(Sampledata$price, Sampledata$volume, main = "ccf plot")

### 6. 가설검정 ###

## 6-1 분산을 아는 모평균의 왼쪽꼬리검정 ##
xbar = 9.9
mu0 = 10
sig = 1.1
n = 30
z= (xbar-mu0)/(sig/sqrt(n))
z

alpha = 0.05
z.alpha = qnorm(1-alpha)
-z.alpha

pnorm(z) #0.05보다 크므로 채택 

## 6-2 분산을 아는 모평균의 오른쪽꼬리검정 ##

xbar = 5.1
mu0 = 5
sig = 0.25
n = 30
z = (xbar-mu0)/(sig/sqrt(n))
z
alpha=0.5
z.alpha = qnorm(1-alpha)
z.alpha
pnorm(z, lower.tail=FALSE) #0.05보다 작으므로 기각 

## 6-3 분산을 아는 모평균의 양측검정 ##

xbar = 1.5
mu0 = 2
sig = 0.1
n = 30
z= (xbar-mu0)/(sig/sqrt(n))
z

alpha=0.05
z.half.alpha=qnorm(1-alpha/2)
c(-z.half.alpha, z.half.alpha)
2*pnorm(z) # 0.05보다 작으므로 귀무가설을 기각한다. 

## 6-4. 분산을 모르는 t 모평균의 왼쪽꼬리검정 

xbar = 0.9
mu0 = 1
sig = 0.1
n = 30
t = (xbar-mu0)/(sig/sqrt(n))

alpha= 0.05
t.alpha = qt(1-alpha, df= n-1)
-t.alpha

pt(t, df=n-1) # 0.05보다 작으므로 귀무가설을 기각한다 .

## 6-5. 분산을 모르는 t 모평균의 오른쪽꼬리검정

xbar = 3.1
mu0 = 3
sig = 0.2
n = 30
t = (xbar-mu0)/(sig/sqrt(n))
t
alpha= 0.05
t.alpha = qt(1-alpha, df= n-1)
t.alpha

alpha = 0.05
t.alpha = qt(1-alpha, df = n-1)
t.alpha
pt(t, df = n-1, lower.tail = FALSE) #0.05보다 작으므로 귀무가설을 기각한다 .

## 6-6. 분산을 모르는 모평균의 양측검정 \

xbar = 1.9
mu0 = 2
sig = 0.1
n = 30
t = (xbar-mu0)/(sig/sqrt(n))
t

alpha= 0.05
t.half.alpha = qt(1-alpha, df= n-1)
c(-t.half.alpha, t.half.alpha)

### 7. 파라미터 추정 ###

## 7-1 MLE (Maximum Likelihood Estimation ##) ##
install.packages("stats4")
library(stats4)
set.seed(100)
NO_values <- 100
Y <- rnorm(NO_values, mean =5, sd =1)
mean(Y)
sd(Y)
LogL <- function(mu, sigma){
  A = dnorm(Y, mu, sigma)
  - sum(log(A))
}

mle(LogL, start = list(mu=2, sigma=2), method = "L-BFGS-B", lower = c(-Inf, 0), upper=c(Inf,Inf))

## 7-2 선형모델 

Y <- Sampledata$price
X <- Sampledata$volume
fit <- lm (Y~X)
summary(fit)

### 8 이상치검출 ###

## 8-1 상자 
boxplot(Sampledata$volume, main = "Volume", boxwex=0.1)

## 8-2 Local outlier factor dkfrhflwma

install.packages("DMwR")
library(DMwR)
outlier.scores <- lofactor(Sampledata$volume, k =4)
plot(density(outlier.scores))
order(outlier.scores, decreasing = T)[1:5] #상위 5개의 이상치 , 행번호가 출력됨 

### 9. 표준화와 정규화 ###

## 9-1 표준화 
scale(Sampledata$volume, center =TRUE, scale =FALSE) # 중심화
scale(Sampledata$volume, center = TRUE, scale =TRUE) # 표준화 

## 9-2 정규화

normalized = (Sampledata$volume-min(Sampledata$volume))/
  (max(Sampledata$volume)-min(Sampledata$volume))
normalized
