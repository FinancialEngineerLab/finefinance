#### Ch5 Numerical Methods ####

### 5.1 Basic Numerical Methods ###

## 5.1.1 plotting function ##

curve(x^2+2*x,-2,1)

x  <- seq(-2,2,0.001)
plot(x, sin(1/x), type= 'l')

## 5.1.2 미분 ##

fx = expression(x^2, "x")
D(fx,'x') #1차 미분
2*x

fx = expression(x^2, "x")
D(D(fx, 'x'),'x') #2차 미분 

fx = expression(x^5 -1/x+cos(x)^x, 'x')
D(fx, 'x') # 복잡한 미분 

fxyz = expression((x*y)^5-1/x^z+cos(x)^x)
deriv(fxyz, c('x','y','z')) #다변수 미분 

D(fxyz, 'y')

## 5.1.3 적분 ##

integrand <- function(x) 1/((x+1)*sqrt(x))
curve(integrand)

integrate(integrand, lower =0, upper=Inf) #적분 인테그랄 

install.packages("Ryacas")
library(Ryacas)
x <- Sym("x")
Integrate(sin(x),x)
#or
yacas("Integrate(x)Sin(x)")


### Intermedidate Numerical Mehtod ##

## 5.2.1 Simulation ##

n <- 1000
f <- function(x) x^2
x <- runif(n,0,2)
y <- runif(n,0,4)
xin <- x[y<f(x)]
yin <- y[y<f(x)]
plot(xin, yin, xlim = c(0,2.5), ylim = c(0, 2.5^2))
curve(x^2, 0, 2.5, add =T)
abline(h=0, col= 'red')
abline(h=4, col= 'red')
abline(v=0, col = 'blue')
abline(v=2, col = 'blue')

#

set.seed(123)
f <- function(x) x^2
a <- 0
b <- 2
c <- 0
d <- 4
n <- 10^5
x <- runif(n,a,b)
y <- runif(n,c,d)
A*sum(y<f(x))/n
integrate(f,a,b)

## 5.2.2 Optiminzation ##

curve(x^2 + x*2-2, -2,2)

#1. Non-Linear Minimization #

f <- function(x) x^2+x*2-2
x <- nlm(f,0)
x

f <- function(x) x^2+x*2-2
optimize(f, lower = -20, upper = 20)

f <- function(x) 10*sin(0.3*x)*sin(1.3*x^2)+x^4/100000+0.2*x+80
x <- seq(-50, 50, length = 1500)
plot(x, f(x), type='l')
nlm(f,3)

x <- optim(3, f, method = "SANN", control = list(maxit = 20000, temp = 20, parscale =20 ))
x

fx <- function(x){x[1]^2 + x[2]^2}
optim(c(1,1), fx) #최소값
optim(x, f, control = list(fnscale= -1)) # 최대값 -> control 인수 조정 

## 5.2.3 해찾기 ##

polyroot(c(1,2,1)) # 1+ 2x + x^2

f <- function(x) 2*x + x^2 -2
uniroot(f, c(-2,2)) #$root가 해

## 5.2.4 interpolation ##

x <- 1:10
y <- rnorm(10)
f <- approxfun(x,y)
curve(f(x), 0, 11 , col = "green2")
points(x,y)

# 보외법 
f <- approxfun(x,y, rule = 2:1)
f(0.5)
f(11)

# spline 
f <- splinefun(x,y)
curve(f(x), 0, 11, col = "black")
points(x,y)

# polynomial
install.packages("polynom")
library(polynom)
x<-c(1,2,3)
y<-c(2,6,4)
f <- poly.calc(x,y)
g <- as.function(f)
curve(g, 0,4)
points(x,y)

# polynom 함수는 뒤부분 tol인수가 중요하다. 주어진 점을 지나도록 한다

a <- c(1129.976, 1152.878, 1173.103, 1193.695, 1217.796)
b <- c(0.10125, 0.08925, 0.08750, 0.08925, 0.10075)
g <- poly.calc(a,b,tol = 0)
f <- as.function(g)
plot(a,b)
curve(f(x),1120,1220,add=T)
