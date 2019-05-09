# ======================================================= #
# Fintech Final Project 
# ======================================================= #
library(dplyr)
# =================================== #
# Q1. (Understand and clean the data)
# =================================== #
# Data load
load("C:/Users/code5/OneDrive/바탕 화면/미국 가는것/Fintech/Final project/features_training.Rdata")
head(training_sample)
class(training_sample)

# Cleaning Data
new_dat <- training_sample[complete.cases(training_sample),]
new_dat2 <- new_dat[order(new_dat$PERMNO, new_dat$yyyy, new_dat$mm),]

# Set Y as returns
Y <- new_dat2$RET
X <- new_dat2[,-(ncol(new_dat2))]
head(X)
names(X)
str(X)

# scaling X variables group by year

var_list <- names(X)[17:ncol(X)]
year_list <- sort(unique(X$yyyy))

for( i in year_list)
{
  sub_set <- X[X$yyyy == i,]
  v <- sub_set[,names(sub_set) %in% var_list]
  X[X$yyyy==i,][,names(X) %in% var_list] <- scale(v,center=F,scale=T)
  
}

head(X) # scaled group by year. This means that I'd like to look year as a fixed effect.
nrow(X)

# Remove outlier
# truncate

X_var <- X[,17:ncol(X)]
head(X_var)
summary(X_var) # Exist outlier. so, need to replace outlier to bound group by year.

for ( v in year_list)
{
  sub_set2 <- X[X$yyyy == v,]
  v2 <- sub_set2[,names(sub_set2) %in% var_list]
  
  for (i in (1:ncol(v2)))
  {
    boxp <- boxplot(v2[,i])$stats
    lower <- boxp[1]
    upper <- boxp[5]
    IQR <- upper-lower
    
    # winsorize
    v2[,i] <- ifelse( v2[,i] <= (lower-IQR*1.5),lower,ifelse(v2[,i] >= (upper+IQR*1.5),upper,v2[,i]))
  }
  X[X$yyyy == v,][,names(X) %in% var_list] <- v2
}
head(X)
nrow(X) # remain 219428


# =================================== #
# Q3. ( Construct models to optimize 
#       in-sample performance )
# =================================== #
# 1. Multi-regression vs. Random Forest
# Create new dataframe
X_var2 <- X[,17:ncol(X)]
info <- c("yyyy","mm","PERMNO","TICKER")
X_info <- X[,names(X) %in% info]
new_data <- cbind(X_info,X_var2,Y)

# Set yyyy as a factor, and set mm to seasonal effects 
# 1 <= mm <= 3 -> 1
# 4 <= mm <= 6 -> 2
# 7 <= mm <= 9 -> 3
# 10 <= mm <= 12 -> 4

new_data$yyyy <- as.factor(new_data$yyyy)
new_data$mm.cat <- ifelse(10 <=new_data$mm & new_data$mm <= 12, 4,
                          ifelse(7 <=new_data$mm & new_data$mm <=9,3,
                                 ifelse(4 <=new_data$mm & new_data$mm <= 6 ,2,1)))
new_data$mm <- as.factor(new_data$mm)
new_data$mm.cat <- as.factor(new_data$mm.cat)
head(new_data)
str(new_data)

# Multi-regression
names(new_data)
Multi_model <- lm(Y~yyyy+PRC+VOL+SHROUT+lag_ME+FFbm+AFbm+FF_Momentum+DecME+R61+Dmq+
                    +Blq+Olq+Tanq+Roe+Roa+Glaq+Iaq1+dRoe+dRoa+BEa+Investment+Ig+Ig2+Ig3+Ivg+Gla+Hn
                  +Hn+Robust+Dm+CFp+Op+Nop+Gpa+Rdm+Tan+Bmq+Am+Amq+mm.cat,data=new_data)
summary(Multi_model) # remove yyyy


Multi_model2 <- lm(Y~PRC+VOL+SHROUT+lag_ME+FFbm+AFbm+FF_Momentum+DecME+R61+Dmq+
                     +Blq+Olq+Tanq+Roe+Roa+Glaq+Iaq1+dRoe+dRoa+BEa+Investment+Ig+Ig2+Ig3+Ivg+Gla+Hn
                   +Hn+Robust+Dm+CFp+Op+Nop+Gpa+Rdm+Tan+Bmq+Am+Amq+mm.cat,data=new_data)
summary(Multi_model2)# 5.573%

# check Multi_collinearity
require(MASS)
require(car)
mc <- vif(Multi_model2) 
mc[mc > 10] # lag_Me, DecME

# stepwise
result_model <- stepAIC(Multi_model2,direction='both')
result_model$anova
Multi_fin.model<-lm(Y ~  PRC + VOL + SHROUT + lag_ME + FFbm + AFbm + FF_Momentum + 
                      DecME + R61 + Dmq + Blq + Olq + Tanq + Roa + Glaq + Iaq1 + 
                      dRoe + dRoa + BEa + Investment + Ig2 + Ig3 + Ivg + Gla + 
                      Hn + Robust + Dm + CFp + Op + Nop + Gpa + Rdm + Tan + Bmq + 
                      Am + Amq + mm.cat,data=new_data)
summary(Multi_fin.model) # adj R^2 = 0.05573
vif(Multi_fin.model) # remove multi_col

# Random Forest
require(randomForest)
require(caret)
# Error: cannot allocate vector of size 1.1 Gb
# Need to reduce size = > 1/3 
set.seed(10)
rsample <- new_data[sample(1:nrow(new_data),size = nrow(new_data)/3, replace = F),]


rand_ind <- sample(c("train","valid"),size=nrow(rsample),replace=T,prob=c(7,3))
train_set <- rsample[rand_ind == "train",]
valid_set <- rsample[rand_ind == "valid",]

ranfo <- randomForest(Y~mm.cat+PRC+VOL+SHROUT+lag_ME+FFbm+AFbm+FF_Momentum+DecME+R61+Dmq+
                        +Blq+Olq+Tanq+Roe+Roa+Glaq+Iaq1+dRoe+dRoa+BEa+Investment+Ig+Ig2+Ig3+Ivg+Gla+Hn
                      +Hn+Robust+Dm+CFp+Op+Nop+Gpa+Rdm+Tan+Bmq+Am+Amq,data=train_set,mtry=40/3,ntree=500,importance=T)

print(ranfo) # 21.11%
var_list2 <- c("mm.cat",var_list)
valid_X <- valid_set[,names(valid_set) %in% var_list2]
valid_Y <- valid_set$Y

# predict
pred.Y <- predict(ranfo,valid_X)
# adj.R^2
N <- length(valid_Y) 
MSE.oob <- sum((valid_Y - pred.Y)^2)/(N - ncol(valid_X) - 1)
SST <- sum((valid_Y - mean(valid_Y))^2) / (N-1)
R.square <- 1 - (MSE.oob/SST)
R.square # 21.61%


