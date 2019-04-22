##### ch12. Capital Adeequacy #####

## 1. 분석적 VaR ##
install.packages("quantmod")
library(quantmod)

getSymbols("^KS11", from = "2015-01-01")
KS11 <- na.omit(KS11)

r <- log(head(KS11$KS11.Close,-1) / tail(KS11$KS11.Low,-1))
m <- mean(r)
s <- sd(r)
VaR1 <- -qnorm(0.05, m, s)
print(VaR1)
hist(KS11$KS11.Close)
