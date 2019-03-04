##### Ch2. 기본사용 #####

#### 2-1 기본사용 ####

### 2-1-1 변수 ###

x <-1 
y <- x^2 +1
z <- "hello R"
w <- sqrt(2)

x
y
w
z

### 2-1-2 연산자 ###

2->a
100->>b
c = 3
d<-4
e <<-5
print(c(a,b,c,d,e)) #c는 하나로 묶어서 벡터로 만들어주는 함수
# 가감승제 
-2
3-2
10^2
3**5
7*6
9/3
pi+3
# 비교 
c(T,T)&c(T,T)
c(T,T)&&c(T,F)
3==1
3!=1
3<=1
3>=1
!(3==1)
#
x<-1:3
x
2%in%x
4%in%x # %in%는원소체크 
7%%3 #나머지 
7%/%3 #정수나누기
#
x <-1:5
x[4]
x <- 1:3
x
y<- -1:-3
y
x<-1:5
y<-x/2
y
#논리
x <- c(TRUE, TRUE, FALSE)
y <- c(TRUE,FALSE,TRUE)
!x
x&y
x|y
x||y
xor(x,y) #x,y 둘다 True면 False 

#### 2-2 내장함수 ####
### 2-2-1 수학함수 ###

1+2
"+"(1,2)
abs(-10)
sqrt(10)
10^2
ceiling(10.5)
floor(10.5)
round(10.12, digit=1) #반올림
cos(2*pi)
sin(2*pi)
tan(2*pi)
log(10)
log10(10)
log2(10)
exp(10)
prod(1:8) #곱셈함수
choose(10,2) #조합함수 nCr
factorial(8) 

### 2-2-2 문자함수 ###
substr("abcdef", 2,4) #2~4문자추
strsplit("abcdef", "c") # 자르기 
paste("abc","de")
toupper("abcde") #대문자 변
tolower("ABC") #소문자 변환
str <- "Now is the time"
sub(" +$", "", str)

### 2-2-3 통계함수 ###

rnorm(2) # 표준정규분포 난수 생성
mean(rnorm(2)) #평균함수
sd(rnorm(2))
var(rnorm(2))
median(rnorm(2))
range(rnorm(2))
#
y <- c(100,200,300,400)
diffy <- diff(y, 1)
diffy
#
min(rnorm(2))
max(rnorm(2))

### 2-2-4 확률분포함수 ###
dnorm(0) # 확률밀도함수
pnorm(0) # 분포함수 

#### 2-3 변수 ####
### 2-3-1 변수이름 ###

### 2-3-2 숫자 ###

1
2
3
1e+06
3.1415

### 2-3-3 문자 ###

"1"
""
"    "
"hello, world!"
'2+3'
#
a1 <- 'test'
a2 <- "test2"
a <- paste(a1,a2)
a[1]
b <- paste(a1,a2,sep="") #공백없이 붙이는 법 
b

### 2-3-4 날짜와 시간 ###
### 2.3.4 Date and Time ###

today <- as.Date("2015-6-9")
today

typeof(today) #double

xdates <- as.Date(c("2015-01-22", "2015-02-11"))
date_diff <- xdates[2] - xdates[1]
date_diff

tmp <- "20150122"
date_01 <- as.Date(tmp, "%Y%m%d")
tmp <- "2015-01-22"
date_02 <- as.Date(tmp, origin = "1900-01-01")
date_raw <- as.Date(tmp, origin = "1900-01-01") # excel import

cur_time <- Sys.time()
cur_time

#

install.packages("lubridate") # split time
library(lubridate)
year(cur_time)
month(cur_time)
hour(cur_time)
minute(cur_time)
second(cur_time)

options(digits.secs= 6)
tnow <- Sys.time()
tnow

### 2.3.5 Data transformation

as.character(x)
as.numeric(x)
#...

### 2.3.6 Special character ###

NA+2
sum(1,2,3, NA, na.rm=T)
#
x<-NULL #initilaizing

#typeof : variable format
x<-1
typeof(x)
vv 
#format : variable format determining
x <-c(1234.54,58358.1432)
x
format(x, nsmall =4)
format(x, big.mark = ",")

### 2.3.7 Variable Deletion ###

a <- 1
b <- 2
ls()
rm(list=ls())
ls()

a<-1
foo<-function(x) x^2
foo(2)

ls()
rm(list=lsf.str())
ls()

rm(list=setdiff(ls(), lsf.str()))


#### 2.4 Data ####

## 2.4.1 Vector ##

x <- c(1,2,5)
typeof(x)
x <- c(1,2,5,"six")
typeof(x)
#
x <- 1:5
length(x)
length(x) <- 7
x
length(x) <- 4
x

c(1,2,4) + c(3,5,8,2,0)

#append
x <- 1:5
y <- 5:1
z <- c(x,y)
w <- append(x,y)
z
w

## 2.4.2 Vector Function ##

data.x <- rep(3,5) #repeat
data.x

vec <- c(rep(2,4), 1:3, c(8,1,9)) # by c, connecting
vec

data.x <- seq(2,10,2)
data.x

weight.x <- c(10,20,30)
length(weight.x)

set.seed(1)
x<- rnorm(1:5)
y<- rnorm(1:5)
x
y
pmin(x,y) #parallel min
min(x,y)

x <- 1:5
rev(x) # reverse

### 2.4.2 Matrix ###

A = matrix(c(2,4,3,1,5,7), nrow = 2, ncol = 3, byrow = TRUE)

M <- matrix(1:6, nrow=2)
M
#
M[2,3]
M[,3]
M[2,]
M[1,1:2]
#
x <- matrix(1:10, nrow = 2, ncol = 5)
x
x[2,3]
x[2,]

x[c(1,2), c(1,2,3)] #row and col detemrining
x[-c(1),] # 1 row delete
#
rownames(x) <- c("a","b")
colnames(x) <- c("a","b","c","d","e")
x

### 2.4.3 Matrix Function ###

m <- matrix(1,2,3)
m
#
dim(m)
ncol(m)
#
x <- matrix(1:4, nrow = 2)
x
#
t(x) #transpose
#
tmp <- matrix(0,2,2) # element, row, col
tmp
#
tmp <- matrix(,2,2)
tmp
#
x <- matrix(1:4, nrow = 2)
y <- matrix(-1:2, nrow=2)
x+y
#
x <- matrix(1:4, nrow =2 )
y <- matrix(-1:2, nrow=2)
x%*%y # multiply matrix
#
a <- 1:3
b <- 3:5
a%*%b
#
I <- diag(5)  # iinentity matrix + diagonal funcion
I
#
x <- matrix(1:4, nrow = 2 )
det(x) # determinant
#
c <- rbind(c(1,-1/4), c(-1/4,1))
c
solve(c) # inverse matrix
#
B <- matrix(c(2,4,3,1,5,7), nrow = 3, ncol = 2, byrow =F)
B
C <- c(7,4,2)
C
cbind(B,C) # additive right col by C
#
D <- t(C)
cbind(B,D)
#
F <- t(c(3,2))
B
F
rbind(B,F) # aditive bottom row by F
#
A = matrix(c(2,4,3,1,5,7), nrow= 2, ncol = 3, byrow=TRUE)
A
c(A) # Deconstrunction


### 2.4.4 Data Manipulation ###

x <- 1:5
x
x[3]
x[-2] ##음수는 제외!

x[c(1,3,5)]
x[2:4]

## 2.4.4.1 Indexing ##

x <- 1:10
x[1]
#
x[c(TRUE,FALSE)] #홀
x[c(FALSE, TRUE)] #짝
x[x%%3 == 0] #3의 배수
x[!(x%%3 ==0 )] #3의 배수 아닌 것
x[-c(2,7)] #음수는 제외!
x <- c(1,2,3)
x[-1]
x[-1:-2]
#
x1 <- 1:8
boolx <- c(T,T,T,F,T,T,F,F)
x2 <- x1[boolx]
x2
#

## 2.4.4.2 Logical indexing ##

set.seed(1)
x <- rnorm(5)
x > 0
x
#
nmb <- sum(x>0)
nmb

idx <- x>0
z <- x[idx]
z
#which로 위와 같게
idx2 <- which(x>0)
x <- x[idx2]
z
# which.min & which.max
set.seed(1)
x <- rnorm(5)
x
which.max(x)
which.min(x)
#
x <- c(0.1, 0.2, 0.7, 0.9)
which.max(x>0.65)
which.min(x>0.65)
#
v <- c('a','b','c','d')
'b'%in%v # 잇으면 T없으면 F
match('b', v) # 원소, 집, index 추출
#
x <- sample(1:10)
x
match(c(4,8), x)
#
sample(c("H", "T"), 5, replace= T)  # replace는 복원/비복원 추출을 의미한다
#
x <- sample(1:4, 10, replace = T)
x
#
which(x %in% c(2,4))
#
x <- 1:5
x
ifelse(x%%2, "ev", "od")
# Table보기
x <- c(1,2,3)
View(x) #보기 
#
a <- 1:60
b <- a[seq(1, length(a),6)] # seq는 매 n번째항을 추출한다. (여기서 6)
print(b)

### 2.5 Function ###

## 2.5.1 Personal Function ##

f <- function(x,y){x+y*2}
f(1,2)
f(y=2, x=1)
#
f2 <- function(){return(list(first=1, second=2))}
r <- f2()
r$first
r$second

## 2.5.2 Range of Function and Variables ##

x <- 1
f <- function(){print(x)}
f()
#
x <- 1
f <- function(){x<-2
print(x)}
x
f()
#
x <- 1
f <- function(){x<<-2 # 재선언
print(x)}
x
f()

### 2.6 제어문 ###

## 2.6.1 If Else ##

f <- function(x){
  if(x>0){
    ans <- "call"}
  else {
    ans <- "put"
  }
  return(ans)
}
#
f <- function(x,y){
  ans <- ifelse(x>y, x,y)
  return(ans)
}
#
x<-c(1,20)
y<-c(20,5)

## 2.6.2 Switch : 특정 변수(x)에 대한 실행문 실행  ##

centre <- function(x, type){
  switch(type, 
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = 0,1))
}

x<-runif(100) #uniform distribution
centre(x,  'median')

## 2.6.3 For ##
x <- 1:10
k <- 0
for (i in 1:length(x)){
  if(x[i]%%2 == 1) k<- k+1 ## a%%b는 a를 b로나눈 나머지를 의미한다
}

#
x <- seq(0,1,0.05)
plot(x,x,ylab = "y", type = "l")
for (j in 2:18) lines(x,x^j)

## 2.6.4 While ##

set.seed(1)
x <- rnorm(1)
counter <- 1
while(x<2){
  x <- rnorm(1)
  counter <- counter+1
}

print(counter)

# Newton - Raphson의 제곱근 구하기 #

y <- c(5,7)
y
x <- y/2
while(any(abs(x*x-y)>1e-10)) x<- (x+y/x)/2
x
x^2

# any
x <- 1:5
any(x>2)
all(x>2)

## 2.6.5 Break ##

set.seed(1)
i <- 1
while(TRUE){
  x <- runif(1)
  if(x>0.9){
    print(i)
    break
  }
  i <- i+1
}

## 2.6.6 Memory 할당 ##
# 생략 #
## 2.7 패키지활용 ##
# 생략 #

### 2.8 디버깅 ###
# 생략 #


