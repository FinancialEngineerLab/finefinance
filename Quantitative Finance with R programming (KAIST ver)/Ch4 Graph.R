#### Ch4 Graph ####

### 4.1 Advanced ###

##4.1.1 plot  ##


x<--10:10
y <- 3*x^3 + x/2-1
plot(x,y, type = "l") #3차 방정식 

graphics.off()


## 4.1.2 Graph 설정 ##

#1. main
set.seed(1)
x <- runif(10)
y <- runif(10)
plot(x,y, main = "uniform rand")

#2. type

xp <- 1:100 / 100
yp <- 3*xp^2 - 2*xp + rnorm(100, sd = 0.2)
par(mfrow = c(3,2)) #그래프 그리는 칸 설정
for ( i in c("l","b","o", "h")){
  plot(xp, yp, type = i, main = paste("Plottype:", i))
}

plot(xp, yp, type = "o", xlab= "index", ylab = "values", main = "Rsimpleplot")
plot(xp, yp, type = "l", axes = FALSE)
axis(1)
axis(2, at = c(-0.6, 0, 0.6, 1.2), col = "blue")
axis(3, at = c(0, 0.25, 0.5, 0.75, 1), col = "red")
axis(4, col = "violet", col.axis="darviolet", lwd  =2)

#3. pch, cex

# 4. lty, lwd #

plot(0,0, type ='n', xlim=c(0,1), ylim=c(1,6))
for(i in 0:6) abline(h=i+1, lty = i, lwd =i) #lty : line shape, lwd : width

# 5. xlim, ylim #
x <- seq(-1,1, 0.01)
y <- sin(pi*x)*x*x
par(mfrow = c(1,2))
plot(x,y, type = 'l')
plot(x,y, type = 'l', xlim =c(-0.5,0.5))


# 6. xlab, ylab #

x <- seq(-3,3, 0.01)
y <- pnorm(x)
par(mfrow = c(1,2))
plot(x,y, type = 'l')
plot(x,y, type = 'l', xlab = 'normal', ylab = 'density')

## 4.1.3 Hist Function ##

count <- rnorm(200)
hist(count)

x<-c(3, 0.5, 0.5, 0.5, 3, 2, 0.5, 0.5,3,3,1)
tmp <- hist(x)

#

par(mfrow = c(1,2))
tmp <- hist(x, main = "default bin")
tmp <- hist(x, breaks = seq(0,3,0.5), main = "user defined bin")

#

par(mfrow = c(1,2))
tmp <- hist(x, breaks=seq(0,3,0.5))
tmp <- hist(x, breaks=seq(0,3,0.5), xaxt = 'n', main = "x axis label")
axis(1, at = tmp$mids, label = seq(0.5,3,0.5))

#

x <- c(3, 0.5, 0.5, 0.5, 3, 2, 0.5, 0.5,3,3,1)
plot(table(x), type = 'h', lwd = 5, ylab = "Freq")

## 4.1.4 Box plot ##

graphics.off()
par(mfrow = c(2,2))
nmbCol <-5
nmbRow <- 100
mat <- matrix(0, nmbRow, nmbCol)
for (i in 1:nmbCol){
  mat[,i] <- t(rnorm(nmbRow))
}
boxplot(mat)
boxplot(mat, range=0) # range -> whiskers(tail)

## 4.1.5 Par ##

#1. mar  : margin (bottom4, left3, right1, upper2)

par("mar") # default mar
x <- rnorm(200)
y <- 25 -22*x + 5*x^2 + rnorm(200)
par(mfrow=c(1,2))
plot(x,y)
par(mfrow = c(4,3,2,1))
plot(x,y)

#2. mgp : location of labels, tick-mark labels, tick-marks  #

par("mgp") # default mgp
x <- rnorm(200)
y <- 25 -22*x + 5*x^2 + rnorm(200)
par(mfrow=c(1,2))
plot(x,y)
par(mgp =c(2,1,0)) # bottom-left2, bottom-right3
plot(x,y)

#3. las : direction of tick-marks labels

x <- rnorm(200)
y <- 25 -22*x + 5*x^2 + rnorm(200)
par(mfrow=c(1,2))
plot(x,y)
par(las = 2) # right graph xaxis label direction reverse
plot(x,y)

#4. mfrow : various graphs

par(mfrow = c(2,2))
plot(x1, y1)
plot(x2, y2)
curve(dnorm(x))
curve(sin)

### 4.2 Intermediate Graph ###
## 4.2.1 points and lines ##

attach(cars)
plot(cars, xlab = "speed[mph]", ylab = "distant[ft]")
points(speed[speed<15], dist[speed<15], pch = "s", col = "blue")
points(speed[speed>=15], dist[speed>=15], pch = "f", col = "green")
lines(lowess(cars), col = "red")

## 4.2.2 abline ##
#abline(h=y) : horizental, abline(v=x) : vertical

curve(x^2-0.5, -1,1, bty = 'n')
abline(h=0, v=0, col= 'red')

## 4.2.3 legend ##

x <- seq(-1,1,0.05)
plot(x,x, type = 'l', col = 1, lty = 1)
for(i in 2:4) lines(x, x^i, col = i, lty= i)
legend('bottomright', c("1","2", "3", "4"), col = 1:4, lty= 1:4)

## 4.2.4 title(main, sub) ##

title(main = "legend function", sub = "test graph")

## 4.2.5 axis  ##

mat <- c(0, 5, 10, 15, 20)
plot(mat, xaxt = 'n')
lab <- c("a","b","c","d","end")
axis(side=1, at=seq(1,5,1), labels = lab)

## 4.2.6 text ##

x <- 1:5
y <- sin(x)
plot(x,y, col= 'red')
curve(sin(x), add= TRUE)
text(x[3]+0.7, y[3], "<- this is (x[3], y[3])")

### 4.3 appendix (graph) ###

##4.3.1 Grid ##

x <- 1:20
y <- sin(x) * abs(x)
plot(x,y, col = 'red')
curve(sin(x)*abs(x), add= TRUE)
grid(col = 'blue')

## 4.3.2 coloring ##

#1. dot

x <- seq(-3,3,0.02)
nx <- length(x)
dy <- 1/length(x)
y <- seq(0,1-dy, dy)
ny <- length(y)
vy <- rep(y,ny)
vx <- NULL
for(i in 1:nx){
  vx <- c(vx, rep(x[i], ny))
}
xin <- vx[vy<dnorm(vx)]
yin <- vy[vy<dnorm(vx)]
plot(xin, yin, cex = 0.1, col= 'gray')
curve(dnorm(x), -3, 3, add=TRUE)
grid()

#2. polygon

plot(dnorm, xlim=c(-3,3))
x <- seq(0, 1.5, 0.1)
polygon(c(0,x,1.5), c(0, dnorm(x),0), density=10)
abline(h=0, col = 'gray')

polygon(x, dnorm(x), col='wheat')

curve(x*x*sin(1/x), -0.1, 0.1)

x <- seq(-3,3,0.1)
plot(x, dnorm(x,1,0.5), xlim=c(-3,3), type= 'l')
curve(dnorm, add= T)
y <- pmin(dnorm(x,1,0.5), dnorm(x,0,1))
polygon(x,y,col='gray')

## 4.3.3 regression graph ##

set.seed(1)
err <- rnorm(100)
x<-1:100
y<-x/10  + err
m <- lm(y~x)
plot(y~x)
abline(m, col='red')

x<-seq(0,2,0.01)
y<-sin(x) + rnorm(201)/20
par(mfrow = c(2,1))
plot(x,y)
plot(x~y)

## 4.3.4 Export ##

png("test.png")
x <- seq(0,2,0.01)
y <- sin(x)*x + rnorm(201)/20
plot(x,y)
dev.off()

pdf("test.pdf")
hist(rnorm(100))
dev.off()

x<-1:100
y <- x^2
tiff("test.tiff", width= 4*2, height = 4*2, units = 'in', res = 300)
plot(x,y, type = 'l')
dev.off()

## 4.3.5 겹쳐서 그리기 ##

#problem o
set.seed(1)
x1 <- rnorm(20, mean = 0, sd = 1)
y1 <- dnorm(x1, mean = 0.2, sd = 1)
plot(x1,y1)
x2 <- rnorm(20, mean = 0, sd = 1)
y2 <- dnorm(x2, mean = 0, sd = 1)
par(new = TRUE)
plot(x2, y2, col = "red", pch = 2)

#solution
set.seed(1)
par(mfrow =c(1,2))
x <- 1:10
x1 <- rnorm(10)
y = x+x1
plot(y~x, pch = 1)
y2 = x
points(y2 ~ x, pch = 4, col = "red")

set.seed(1)
x <- 1:10
x1 <- rnorm(10)
y = x+x1
plot(y~x, pch = 1)
f <- function(x) x
curve(f, add= TRUE, col='red')

#solution 2

x <- seq(-3,3,0.02)
y1 <- dnorm(x)
y2 <- dt(x, df=1)
ymat <- cbind(y1,y2)
matplot(x, ymat, type = 'l', bty = 'n')

## 4.3.6 수식 입력 ##

par(mfrow = c(1,2))
curve(dnorm, from = -3, to = 3, n=1000, main = "NormalProbabilityDensityFunction")
text(-2,0.3, expression(f(x)==paste(frac(1, sqrt(2*pi*sigma^2)),"",e^{frac(-(x-mu)^2, 2*sigma^2)})), cex=1.2)
x <- dnorm(seq(-3,3,0.001))
plot(seq(-3,3,0.001),cumsum(x)/sum(x), type = "l", col = "blue", xlab= "x", main = "NormalCumulativeDistribitionFunction")
text(-1.5, 0.7, expression(phi(x) == paste(frac(1, sqrt(2*pi)), "", integral(e^(-t^2/2)*dt, -infiniy,x))), cex=0.8)

## 4.3.7 Greek Letter input ##

h <- rnorm(mean=5, sd= 1, n=1000)
hist(h, main = expression(paste("sampledvalues:", mu, "=5,", sigma , "=1")))

#

x <- seq(-1,50,0.01)
y <- sin(x)*x
plot(x,y, main = substitute(y==Psi*z-sum(beta^gamma)), type = 'l')
text(0,10, substitute(Delta[k]==1))
text(30,40,substitute(Delta[k]==epsilon))

## 4.3.8 list를 plot하기 ##

dat <- list(a=1:5, b= 2:7, c= 3:10)
plot(unlist(dat), type = "n", xlim = c(1, max(sapply(dat, length)))) #빈공간 
mapply(lines, dat, col = seq_along(dat), lty = 2) #x축 범위
legend("topleft", names(dat), lty = 2, col = seq_along(dat))


## 4.3.9 DataFrame을 plot하기 ##

df <- data.frame(NULL)
newrow <- 1:3
df <- rbind(df, newrow)
newrow <- c(7,1,9)
df <- rbind(df, newrow)
colnames(df) <- c("a","b","c")
x <- matplot(df, type = 'l')
legend("topleft", legend = colnames(df), lty =1:3, bty = "n")
x <- matplot(df, type  = 'l', lty = 1:3, col= 1:3)

## 4.3.10 색깔 ##

set.seed(1)
x <- 1:100/20
y <- sin(x) + rnorm(100)/5
cols <- ifelse(y>0, "black", "gray")
par(mfrow=c(1,2))
plot(x,y, type = 'h', lwd = 2)
plot(x,y, type='h', lwd = 2, col= cols)

### 4.4 3차원 그래프 ###
install.packages("rgl")
library('rgl')
open3d()
x <- sort(rnorm(1000))
y<-rnorm(1000)
z<-rnorm(1000) + atan2(x,y)
plot3d(x,y,z,col=rainbow(1000))

#

data(volcano)
z <- 2*volcano
x<-10*(1:nrow(z))
y<-10*(1:ncol(z))
zlim <- range(y)
zlen <- zlim[2]-zlim[1]+1
colornut <- terrain.colors(zlen)
col <- colornut[z-zlim[1]+1]
open3d()
surface3d(x,y,z,color=col, back = "lines")
