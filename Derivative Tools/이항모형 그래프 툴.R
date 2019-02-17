install.packages("AmericanCallOpt")
library(AmericanCallOpt)

install.packages("derivmkts")
library(derivmkts)



binomplot(100, 105, 0.3, 0.1, 0.2, 0, nstep=100, american=TRUE, putopt=TRUE,plotvalues = TRUE,
          plotarrows=TRUE)

binomplot(100, 105, 0.3, 0.1, 0.2, 0, nstep=100, american=TRUE, putopt=TRUE)
binomplot(100, 105, 0.3, 0.1, 0.2, 0, nstep=100, american=TRUE, putopt=TRUE,ylimval=c(85,95))
binomplot(100, 105, 0.3, 0.1, 0.2, 0, nstep=100, american=TRUE,putopt=TRUE)


a<-binomplot(100,105,0.3,0.1,0.2,0,nstep=50, american=TRUE, putopt=TRUE, returntrees=TRUE,returnparams =TRUE)
a$price


a<- binomopt(100,105,0.3,0.1,0.2,0,50,american=TRUE,putopt=TRUE,returnparams=TRUE)
a[1]
a[1,]
      
a<-c()

binomopt(100,105,0.3,0.1,0.2,0,1000,american=TRUE,putopt=TRUE)

for (i in 50:1000){
  a <- binomopt(100,105,0.3,0.1,0.2,0,i,american=TRUE,putopt=TRUE)
}

a
bs <- 7.362142104395894


error <- a-bs
plot(error,type='l')
