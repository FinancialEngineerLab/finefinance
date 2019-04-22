#### 1-3 Extreme Value Theory ####

install.packages("evir")
library(evir)

data(danish)
summary(danish)

hist(danish, breaks = 200, xlim = c(0,20))

sum(danish >20) / length(danish)
sum(danish[danish>20])/sum(danish)

emplot(danish)
emplot(danish, alog = "xy")
qplot(danish, trim=100)
meplot(danish, omit=4)


## Fitting a GPD distribution to tails ##

gpdfit <- gpd(danish, threshold = 10)
gpdfit$converged
gpdfit$par.ests
gpdfit$par.ses

## Quantile estimation using the fitted GPd model ##

tp <- tailplot(gpdfit)
gpd.q(tp, pp = 0.999, ci.p = 0.95)
quantile(danish, probs = 0.999, type = 1)

## Calculation of expected loss using the fitted GPd model ##
## Expected Shortfall ##

tp <- tailplot(gpdfit)
gpd.q(tp, pp=0.99)
gpd.sfall(tp, 0.99)

#### 1-4 Financial Networks ####

library(igraph)

set.seed(7)
e <- erdos.renyi.game(100, 0.1)
