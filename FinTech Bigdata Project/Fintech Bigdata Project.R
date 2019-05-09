#### Fintech Project ####

library(stringr)
library(randomForest)
library(caret)
library(tree)
library(Epi)
library(rpart)
library(ggplot2)
library(dplyr)
library(rpart)
library(dplyr)
library(tidyverse)
library(MASS)
library(car)
library(glmnet)
library(MASS)
attach(Boston)

### data ###

data <- training_sample
names(data)
names(training_sample)

training_sample <- data.frame(training_sample)

data_sample2 <- training_sample[order(training_sample$PERMNO, training_sample$yyyy, training_sample$mm),]
training_sample <- data.frame(training_sample)
training_sample <- training_sample[order(training_sample$PERMNO, training_sample$yyyy, training_sample$mm),]
training_sample['RET'] <- lead(data_sample2$RET)
training_sample <-na.omit(training_sample)
### Lasso Methods ###

head(training_sample)
nrow(training_sample) # Universe check : 219420

#install.packages('tidyverse')
# 1. Stepwise var selection ( Traditional Linear Model Method )

lm_model.1 <- lm(RET ~ 1+PRC + VOL + SHROUT +lag_ME + FFbm+AFbm+FF_Momentum+DecME+
                   R61 + Dmq + Blq + Olq + Tanq + Roe + Roa + Glaq + Iaq1 + dRoe+dRoa+
                   BEa + Investment + Ig + Ig2 + Ig3 + Ivg + Gla + Hn + Robust+Dm+CFp+Op+Nop+
                   Gpa + Rdm + Tan + Bmq + Am + Amq, data = training_sample)
summary(lm_model.1) # Exist multicollinearity
vif(lm_model.1)
step1 <- stepAIC(lm_model.1,direction="both")
step1$anova # select => Gla, lag_ME, Tan, Dm, Dmq, Ig, Op, BEa, FF_Momentum


lm_model.2 <- lm(RET ~ 1 +Gla+lag_ME+Tan+Dm+Dmq+ Ig+Op+BEa+FF_Momentum, data=training_sample)
summary(lm_model.2) # adh.R2 : 0.07 (7% 설명력)...?
vif(lm_model.2) # < 10 : no Multicollinearity


### LASSO ###
Var_list = c("Gla","lag_ME","Tan","Dm","Dmq","Ig","Op","BEa", "FF_Momentum")


Var.own.scale <- as.matrix(scale(training_sample[,names(training_sample)%in% Var_list],center=F,scale=T))
Var.M.scale <- as.matrix(training_sample[,17:54])
Target.Y <- as.matrix(training_sample[,names(training_sample)=='RET'])

## Var Own 
lasso.mod <- glmnet(Var.own.scale, Target.Y, alpha = 1)
cv.out <- cv.glmnet(Var.own.scale, Target.Y,alpha=1)
plot(cv.out, label=TRUE)
bestlam=cv.out$lambda.min
bestlam

dim(coef(lasso.mod ))
summary(lasso.mod)
plot(lasso.mod)

lasso.coef  <- predict(lasso.mod, type = 'coefficients', s = bestlam)[1:6,]
lasso.coef
 
## Var M

lasso.mod2 <- glmnet(Var.M.scale, Target.Y, alpha = 1)
cv.out2 <- cv.glmnet(Var.M.scale, Target.Y,alpha=1)
plot(cv.out2, label=TRUE)
bestlam2=cv.out2$lambda.min
bestlam2

dim(coef(lasso.mod2))
summary(lasso.mod2)
plot(lasso.mod2)

lasso.coef2  <- predict(lasso.mod2, type = 'coefficients', s = bestlam)[1:6,]
lasso.coef2

### Fama French Factor Model ###

ff_simple <- glm(RET ~ FFbm + AFbm + FF_Momentum, data=training_sample)
summary(ff_simple)


### Multifactor Modeling - fixed effect ###

