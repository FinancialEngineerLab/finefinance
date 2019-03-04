#### Intermediate of R Programming in Finance ####

### 3.1 Data Structure ###

## 3.1.1 Mode and Class ##
x <- as.Date("2010-01-01")
mode(x)
class(x)
#
x<-1:5
y<-x^2
m <- lm(y~x)
class(m)
mode(m)
names(m)
m$coefficients #regression result summary

## 3.1.2 Array ##

x <- array(1:3, c(2,2,3)) # element, dimension
# array -> vector
attr(x, "dim") <- NULL
is.array(x)
is.vector(x)

## 3.1.3 List ##

x <- list(1,2,"3", "good")
x
typeof(x[2])
typeof(x[[2]]) #list accessing [[]]
typeof(x[[3]]) #list accessing [[]]
#
x <- list(name = "chang", age = 10, title = "manager")
x$name

#

class(x[2])
class(x[[2]])
x <- list(2,4,7)
x

# Delete
x[[2]] <- NULL
x

x<-c(2,4,7)
x
x[2] <- NULL

x<-list(1,2,"good")
typeof(x)
y<-unlist(x)
typeof(y)
y

## 3.1.3 DataFrame ##

df <- data.frame(id = c("Kim", "lee", "park"), score = c(50,55,43))
names(df)
dim(df)
str(df)
summary(df)
nrow(df)
ncol(df)
df
df$id

df <- data.frame(id = c("kim", "lee", "park"), score = c(50,55,43))
df
x <- df[1,2] #1row 2col
x
#
v1 <- 1:5
v2 <- 6:10
v1
v2
mylist <- list(dataA = v1, dataB = v2)
mylist$dataA
mylist$dataB
data.frame(mylist)
#
df <- data.frame(mylist)
rownames(df)
colnames(df)
colnames(df) <- c("kor", "math")
df
#
MFVdata <- NULL
MFVdata <- data.frame(as.numeric(), as.numeric(), as.Date(character()))
x<-100
y<-200
z<-as.Date("2001-01-01")
MFVdata <- rbind(MFVdata, data.frame(x,y,z))
MFVdata

x <- matrix(1:6, nrow=2)
df <- as.data.frame(x)
x
tmp <- edit(df) ## Data Editor ##

#

df <- data.frame(NULL)
df <- rbind(df,c(1,2,3))
df <- rbind(df,c(2,3,3))
df <- rbind(df,c(4,2,1))
colnames(df) <- c("kor", "math", "eng")
tmp01 <- subset(df, math ==2)
print(tmp01)

# Stack
require(stats)
formula(PlantGrowth)
pg <- unstack(PlantGrowth)
pg
stack(pg)
#
x <- data.frame(bad=1:3, worse = rnorm(3))
x
colnames(x) <- c("good", "better") #열 이름 정하기 
x
colnames(x)[2] <- "super" #2열 이름바꾸기
x
#

## 3.1.4. Factor ##

data = c(1,2,2,3,1,2,3,3,1,2,3,3,1)
fdata= factor(data)
fdata

xv <- c("A","C","B","A","A","C","B")
x <- factor(xv, levels = c("A","B","C"))
xv
x

as.numeric(x)
as.numeric(xv)

## 3.1.5 Data Structrue Transformation ##

fac <- factor(c("1","2","1","2"))
as.numeric(as.character(fac)) ## as.~는 ~로 데이터구조 변환시킴 

## 3.1.6 Merger ##

# 1. 불완전 merger
merge(x,y,by="index") # index 별로 

# 2. 완전 merger
merge(xy,by="index", all = TRUE) # all 기준

### 3.2 Vectorization ###

## 3.2.1 개념 ##

# vector화 하지 않은 경우 
a <- 1:10
b <- 21:30
c <- 0
for (i in 1:10){
  c[i] <- a[i] + b[i]
}
c

v <- 1:1000
for(i in 1:1000){
  v[i] <- v[i]+1
}
v
# vector한 경우
a <- 1:10
b <- 21:30
c <- a+b
c

v <- 1:1000
v <- v+1

#

weight.x <- c(10,20,30)
weight.sq <-weight.x^2
#
a<-1:10
b <- 21:30
a+b
a-b
a*b
a/b
a%%b

## 3.2.3 벡터화구현 ##

#1. apply

m <- matrix(1:10, nrow= 5)
m
apply(m,1,mean) # 행 , 렬, 형태 
apply(m,2,mean)
apply(m, 1:2, mean)

#2. lapply : list
x <- list(a=1:5, b=6:10)
lapply(x, mean)
lapply(x,mean)$a
lapply(x,mean)$b

#3. sapply : simple, vector

x <- list(a=1, b=1:3, c=1:50)
sapply(x, length)
sapply(x, sum)

#4. mapply : various 

mapply(sum, 1:5, 1:5,1:5)

#5. vectorize : scalar -> vector

f <- function(x=1:3, y) c(x,y)
f(1:3, 1:3)
vf <- Vectorize(f)
vf(1:3, 1:3)
vf(y=1:3)

## 3.2.3 Tip for Vectorize ##

f <- function(x,y) x+2*y
a <- 1:3
b <- 10
f(a,b)
#
f <- function(x,y) x+2*y
f(1:3,5)
#
f <- function(x,y) x-y
x <- 1:3
y <- 4:5
f(x,y)

x<-2
a*x

f <- function(vx,y) sum(vx)-y
vx <- 1:3
y <- 4:5
f(vx,y)
#=
f <- function(vx,y) {sum(vx) - y}
vf <- Vectorize(f, "y")
vx <- 1:3
y <- 4:5
vf(vx,y)

# apply reverse
x <- matrix(1:6, nrow = 2, ncol = 3)
x
y <- apply(x, 2, rev)
y
z<- apply(x, 1, rev)
z
t(z)


### 벡터화 효율성 ###

x <- sample(c(0,1),1)
x
x<- sample(c(0,1),5, replace =TRUE)
x
x<-sample(c(0,1), 5, replace = TRUE)
x

coin_toss <- function(n, steps= 100){
  tosses <- cumsum(sample(c(0,1), n, replace =TRUE))
  steps <- seq(step, n, by=step)
  t(sapply(steps, function(x) c(tosses[x]/x-.5, tosses[x] - x/2)))
}


