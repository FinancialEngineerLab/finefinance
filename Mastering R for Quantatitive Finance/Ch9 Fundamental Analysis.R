#### Ch9 Fundamental Analysis ####

library(MASS)
library(stats)
library(matrixStats)
library(rpart)

## DATA ##

d <- read.csv("C:/Users/shinhyunjin/dropbox/data/da_new_data.csv")
d <- na.omit(d)
## Sector Analysis ##

for(i in c(3:21)) {d[,i] =as.numeric(d[,i])}

boxplot_data <- split(d$RETURNS/100, d$SECTOR)
par(mar = c(10,4,4,4))
boxplot(boxplot_data,las = 2, col = "grey", ylim=c(-5,5))

## Scatter diagram ##

model <- lm(d$RETURNS/100 ~ d$CAP, data =d)
a <- model$coefficients[1]
b <- model$coefficients[2]

#windows()
plot(d$CAP, d$RETURNS/100,xlab = "CAP",ylim = c(0,3), ylab ="Total Return 3M")
abline(a,b, col = "red")

## pearson correlatoion ##

d_filt <- (d[,3:21])
cor_mtx <- cor(d_filt)
round(cor_mtx, 3) # 0.439 ~ 0.425  and abs vlaue >0.2 -> suckingmodel

## backward method ##

vars <- colnames(d_filt)
m <- length(vars)
lin_formula <- paste(vars[m], paste(vars[-m], collapse =" + "), sep  =" ~ ")
fit <- lm(formula = lin_formula, data = d_filt)
fit <- stepAIC(object = fit, direction = "backward", k=4)
summary(fit) #suckingmodel 

### 1. Split Strategy Goals ###

h_clust <- hclust(dist(d$OPM))
plot(h_clust, labels = F, xlab = "")

# k clustering

k_clust <- kmeans(d$RETURNS/100, 3)
k_means_results <- cbind(k_clust$centers, k_clust$size)
colnames(k_means_results) = c("Cluster Center", "Cluster size")
k_means_results

# ANOVA Table #

for(i in c(3:17,19:21)){
  print(colnames(d)[i]);
  print(summary(aov(d[,i] ~ k_clust$cluster, d)))}


### 2. Decision or Claissfication Trees ###


d_tree <- d[,c(3:17,19:21)]
vars <- colnames(d_tree)
m <- length(vars)
tree_formula <- paste(vars[m], paste(vars[-m], collapse = " + "), sep = " ~ ")
tree <- rpart(formula = tree_formula, data = d_tree, maxdepth = 5,cp = 0.001)
tree <- prune(tree, cp = 0.003)
par(xpd = T)
plot(tree)
text(tree, cex = 0.5, use.n = T, all = T)

### 3. Backtesting ###

#filter1
d$condition1 <- (d[,20] < 0.4683)
d$condition2 <- (d[,16] < 8.365)
d$condition3 <- (d[,3] > 13.59)
d$condition4 <- (d[,5] <1.666e+07)
d$selected1 <- d$condition1 & d$condition2 & d$condition3 & d$condition4

#filter2
d$condition5 <- (d[,16] >  8.365)
d$condition6 <- (d[,13] >6.94e+09)
d$condition7 <- (d[,9]<1.96)
d$selected2 <- d$condition1 & d$condition5 & d$condition6 & d$condition7

#filter 1 + filter2
d$tree <- d$selected1 | d$selected2

## ANOVA table ##

f <- function(x) c(mean(x), length(x), sd(x), median(x))
report <- aggregate(x = d[,18], by=list(d$tree), FUN =f)$x
colnames(report) =c("mean", "N", "SD", "Median")
report <- rbind(report, f(d[,18]))
rownames(report) <- c("Not Selected", "Selected", "Total")
print(report)

## 3 Clusters Analysis ; Under Mid Over ##

d$cluster = k_clust$cluster
z <- round(cbind(t(aggregate(d[,c(3:17,19:21)], list(d$selected1), function(x) mean(x, na.rm=T))),
                 t(aggregate(d[,c(3:17,19:21)], list(d$selected1), function(x) median(x, na.rm=T))))[-1,],2)
colnames(z) <- c("1mean","1median","2mean","2median")
z

d$cluster = k_clust$cluster
w <- round(cbind(t(aggregate(d[,c(3:17,19:21)], list(d$selected2), function(x) mean(x, na.rm=T))),
                 t(aggregate(d[,c(3:17,19:21)], list(d$selected2), function(x) median(x, na.rm=T))))[-1,],2)
colnames(z) <- c("1mean","1median","2mean","2median")
w

max(d$RETURNS & d$selected1)

## benchmark
d$bench1 <- (d[,20] >2.278)
d$bench2 <- (d[,4] < 1.513)
d$bench3 <- (d[,9] <3.453)
d$benchmark <- d$bench1 & d$bench2 & d$bench3

d$selected1[is.na(d$selected1)] <- FALSE
h <- function(x) c(mean(x, na.rm=T), length(x[!is.na(x)]), sd(x, na.rm = T), median(x,na.rm=T))

backtest <- aggregate(d[,18], list(d$selected1),h)
backtest <- backtest$x
backtest <- rbind(backtest, h(d[,18]))
colnames(backtest) = c("mean", "N", "Stdev", "Median")
rownames(backtest) = c("Not selected", "Selected", "total")
print(backtest)

## Tree vs ANOVA ##

crosstable <- table(d$selected1, d$tree)
rownames(crosstable) = c("cluster-0", "cluster-1")
colnames(crosstable) = c("tree-0", "tree-1")
crosstable <- addmargins(crosstable)
crosstable

mean(d[d$selected1 & d$tree, 18])
median(d[d$selected1 & d$tree, 18])

#### Sector Investment ####

d2 <- d[,21] ==3
d_sec1<-d[d2,c(3:17,19:20)]
vars <- colnames(d_sec1)
m <- length(vars)
tree_formula <- paste(vars[m], paste(vars[-m], collapse = " + "), sep = " ~ ")
tree <- rpart(formula = tree_formula, data =d_sec1, maxdepth = 5, cp = 0.01, control=rpart.control(minsplit=100))
tree <- prune(tree, cp = 0.006)
par(xpd = T)
plot(tree)
text(tree, cex = 0.5, use.n = T, all = T)
print(tree)

## Selected & Sector ##

cross <- table(d[,21], d$selected1)
colnames(cross) <- c("Not selected", "selected")
cross
prop.table(cross)

## Result ##

t1<-aggregate(d[d$tree, 18], list(d[d$tree, 21]), function(x) c(mean(x), median(x)))
t2<-aggregate(d[!d$tree, 18], list(d[!d$tree, 21]), function(x) c(mean(x), median(x)))
t1 <- rbind(c("2",NA,NA),t1)
t1 <- rbind(c("5", NA,NA),t1)
t1 <- t1[order(t1$Group.1),]
t2 <- t2[order(t2$Group.1),]
industry_crosstab <- merge(t1,t2, by = 'Group.1')
rownames(industry_crosstab) <- t1[,1]
industry_crosstab
