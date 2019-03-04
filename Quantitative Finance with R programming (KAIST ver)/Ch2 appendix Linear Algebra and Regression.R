## Ch2 Appendix, Linear Algebra and Regression ##

set.seed(1)
x <- 1:10
y <- 2+x+rnorm(1)
print(cbind(x,y))
plot(x,y)
abline(lm(y~x), col = 'red')
#
b <- solve(t(x)%*%x)%*%t(x)%*%y
b
#
y_hat <- x%*%b
y_hat
#
e <- y-y_hat #잔차 residual
e
#
H <- x%*%solve(t(x)%*%x)%*%t(x) #Hat Matrix H
y_hat2 <- H%*%y # 추정된 종속변수(vector of fitted values Yhat)
y_hat2
#
# Sum of squared residuals
SSE <- t(y)%*%y-t(b)%*%t(x)%*%y
SSE
# Degree of Freedom
df <- nrow(x) - ncol(x)
df

df <- length(x)-1
df

MSE <- SSE/df
MSE
# var - cov matrix
VC_b <- MSE[1,1]*solve(t(x)%*%x)
VC_b

# var - cov matrix of residuals
VC_e <- MSE[1,1] * (diag(nrow(H))-H)
VC_e
