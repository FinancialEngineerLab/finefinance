##### 채권 및 이자율 상품 #####

### 7.4.1 채권 ###

bond.price <- function(T,c,y,m=2){
  r <- 1/(1+y/m)
  n <- T*m
  dfs <- r^(1:n)
  cfs <- c(rep(c/m, n-1), c/m+1)
  ans <- sum(dfs*cfs)
}

a <- bond.price(3,2.5/100, 1.8/100)
a

bond_price <- function(T,c,y,m=2){
  x <- 1+y/m
  n <- T*m
  ans <- c/y*(1-x^-n)+x^-n
}
yseq <- seq(0, 0.2, length.out=100)
yseq

price <- c()
b<- bond_price(5,0.05, 0.1, 0.1)
b
for (i in yseq){
  price2 <- bond_price(5,0.05,0.1, m=i)
  price <- c(price, price2)
}
price
plot(yseq, price, type='l')

### Duration and Convexity ###

bond_price <- function(T,c,y,m=2){
  x <- 1+y/m
  n <- T*m
  ans <- c/y*(1-x^-n)+x^-n
}
duration <- function(T,c,y, m =2){
  nt <- T*m
  p <- bond_price(T,c,y)
  tmp <- sum((1:nt)/m*c/m*1/(1+y/m)^(1:nt))+T/(1+y/m)^nt
  ans <- tmp/p
}

md <- function(T,c,y,m=2) duration(T,c,y,m=2)/(1+y/m)
p1 <- function(y) 1-md(T,c=0.1,y)*(y-0.1)

c = 0.1
yseq <- seq(0.05, 0.4, length.out=100)
price <- bond_price(10,c=0.1, yseq)
plot(yseq, price, type= 'l')
curve(p1, add=TRUE, col = 'red')
abline(v=0.1, lty =2)
abline(h=1.0, lty = 2)
abline(v=0.2, lty =2,col='blue')
abline(h=p1(0.2), lty=2, col='blue')
abline(h=bond_price(T,c,0.2), lty=2, col='blue')


### 7.4.2 이자율 스왑 ###

