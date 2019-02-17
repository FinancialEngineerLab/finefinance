### Bond ###
rm(list=ls())
#등비수열#
2^(1:3)

#채권가격 계산기 (c : 액면, y는 수익률, m은 freq)
bond.price <- function(T,c,y,m=2){
  r <- 1/(1+y/m)
  n <- T*m
  dfs <- r^(1:n)
  cfs <- c(rep(c/m, n-1), c/m+1)
  ans <- sum(dfs*cfs)
}

x <- bond.price(3,2.5/100, 1.8/100)
x

#채권가격과 수익률의 관계 도출 함수 #

bond_price <- function(T,c,y,m=2){
  x <- 1 + y/m
  n <- T*m
  ans <- c/y*(1- 1/x^n)+ 1/x^n
}

yseq <- seq(0, 0.2, length.out =100)
price <- bond_price(5,0.1, yseq)
plot(yseq, price, type = 'l', main = "bond and rate")

# 채권의 볼록성과 리스크매니지먼트 #

bond_price <- function(T,c,y,m=2){
  x <- 1+y/m
  n <- T*m
  ans <- (c/y)*(1-1/x^n)+1/x^n
}

duration <- function(T,c,y,m=2){
  nt <- T*m
  p <- bond_price(T,c,y)
  tmp <- sum((1:nt)/m*c/m*1/(1+y/m)^(1:nt)) + T/(1+y/m)^nt
  ans <- tmp/p
}

md <- function(T,c,y,m=2) {duration(T,c,y,m=2)/(1+y/m)}
p1 <- function(y) {1-md(T,c, y)*(y-0.1)}

yseq<-seq(0.05, 0.4, length.out=100)
price <- bond_price(10, 0.10, yseq, m=2)
plot(yseq, price, type = 'l')
curve(p1, add= TRUE, col = 'red')
abline(v=0.1, lty=2)
abline(h=1.0, lty=2)
abline(v=0.2, lty=2, col='blue')
abline(h=p1(0.2), lty=2, col='blue')
abline(h=bond_price(T,c,0.2), lty=2, col = 'blue')

p1(0.2)