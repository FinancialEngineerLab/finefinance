#### Ch2. Factor Model ####

install.packages("Quandl")
install.packages("matrixStats")
install.packages("raster")


### Arbtriage Pricing Theory ###

### Fama-French 3 Factor Models ###
library(Quandl)
library(matrixStats)
library(raster)

stocks <- read.table("C:/Users/shinhyunjin/dropbox/data/stocks.csv", header = TRUE, sep=";")
stocks

str(stocks)

stocks[1:5, c(1, 3:4, ncol(stocks))] # date, market value, book value

d <- read.table("C:/Users/shinhyunjin/dropbox/data/data.csv", header = TRUE, sep = ";")
d[1:7, c(1:5, (ncol(d)-6):ncol(d))] # row / col 

## data cleaning 

d <- d[, colSums(is.na(d))==0]
d1 = as.matrix(d[, 2:ncol(d)])
d <- d[, c(T, colMins(d1)>0)]

d1
d

### PCA(Principal component analysis) ###

p <- d[, 3:ncol(d)]
p
r <- log(p[2:nrow(p),] / p[1:nrow(p)-1,])
r

# random sampling for PCA
r <- r[, runif(nrow(r))<0.1]
pca <- princomp(r)
plot(pca$sdev, main  = "PC's Standard Deviation")
factanal(r,5)

### Fama French Model Estimation ###

d1 <- d[, 2:ncol(d)]
d2 <- log(tail(d1, -1)/head(d1,-1))
LIBOR <- Quandl('FED/RILSPDEPM01_N_B',start_date = '2010-06-01', end_date = '2014-06-01')

d <- cbind(d[2:nrow(d), 1], d2)
d[,1] = as.character(d[,1])
LIBOR[,1] = as.character(LIBOR[,1])
names(LIBOR)[2] = 'LIBOR'
d <- merge(LIBOR, d, by=1)
print(d[1:5, 1:5])


## daily return
d$LIBOR <- d[,2] / 36000
stocks[1:5, c(1, 3:4, ncol(stocks))]
stocks = stocks[stocks$Symbol%in% colnames(d),]
stocks$BookToMarketRatio <- stocks$BookValuePerShare / stocks$LastSale
str(stocks)

#SMB
avg_size <- mean(stocks$MarketCap)
BIG <- as.character(stocks$Symbol[stocks$MarketCap >avg_size])
SMALL <- as.character(stocks[stocks$MarketCap < avg_size,1])
d$SMB <- rowMeans(d[, colnames(d)%in%SMALL]) - rowMeans(d[,colnames(d)%in%BIG])
#HML
avg_btm <- mean(stocks$BookToMarketRatio)
HIGH <- as.character(stocks[stocks$BookToMarketRatio > avg_btm,1])
LOW <- as.character(stocks[stocks$BookToMarketRatio < avg_btm,1])
d$HML <- rowMeans(d[, colnames(d)%in%HIGH]) - rowMeans(d[,colnames(d)%in%LOW])

#Market
d$Market <- d$SP500 - d$LIBOR

# FFM test
d$C <- d$C - d$LIBOR
model <- glm(formula = "C ~ Market + SMB+HML", data=d)
summary(model)

## Citi Group Famafrench Risk Premium ##

estimation <- model$coefficients[1]+
  model$coefficients[2] * d$Market +
  model$coefficients[3] * d$SMB +
  model$coefficients[4] * d$HML
plot(estimation, d$C, xlab = "estimaed risk-premium", ylab = "observed risk premium",
     main = "FFM for citigroup")
lines(c(-1,1), c(-1,1), col = "red")

# outlier
outlier <- which.max(d$C)
d$C[outlier] <- 0

# after outlier processed model
model_new <- glm(formula = "C ~ Market + SMB + HML", data = d)
summary(model_new)

# citi plotting
estimation2 <- model_new$coefficients[1]+
  model_new$coefficients[2]*d$Market+
  model_new$coefficients[3]*d$SMB +
  model_new$coefficients[4]*d$HML
dev.new()
plot(estimation2, d$C, xlab = "estimated risk-premium", ylab="observed risk premium",
     main = "FFM for citi 2")
lines(c(-1,1), c(-1,1), col = "red")
    
### EXEL FamaFrench Risk Premium ###

d$EXEL <- d$EXEL - d$LIBOR
model3 <- glm(formula = "EXEL ~ Market + SMB  + HML", data = d)
summary(model3)

#
estimation3 <- model$coefficients[1]+
  model3$coefficients[2]*d$Market +
  model3$coefficients[3]*d$SMB+
  model3$coefficients[4]*d$HML
plot(estimation3, d$EXEL, xlab = "estimated risk-premium", ylab = "obsereved risk-premium",
     main = "FFM for EXEL")
lines(c(-1,1), c(-1,1), col = "red")

