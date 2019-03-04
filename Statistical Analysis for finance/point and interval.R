rm(list=ls())

### L1 : Point and interval esimatiokn ####
#Generate normal random variable

n=20

mu=10

sig=1



X=rnorm(n, mean=mu, sd=sig)



#CI for t distribution

CI.t=function(X, alpha){
  
  
  
  n=length(X)
  
  x.bar=mean(X)
  
  S= var(X)
  
  
  
  t=qt(1-alpha/2, n-1)
  
  
  
  CI=x.bar+ c(-t*sqrt(S/ n), t*sqrt(S/ n))
  
  
  
  CI
  
}



CI.t(X, 0.05)







#Construct CI 100 times





run=function(n, mu, sig, alpha){
  
  X=rnorm(n, mean=mu, sd=sig)
  
  
  
  CI=CI.t(X,alpha)
  
  CI
  
}





alpha=0.05

result=matrix(0, 100,2)

for(i in 1:100){
  
  result[i,]=run(n, mu, sig, alpha)
  
}



setwd("C:/Users/Donggyu/Dropbox/KAIST Course/2017 Fall/FE502/Lecture/Lecture1")

pdf("CI.pdf", height=10, width=10)



plot(result[1,], c(100, 100), type="l", ylim=c(-10, 110), 
     
     xlim=c(min(result), max(result)), ylab="", xlab="CI", 
     
     main="Confidence Interval", cex.main=2, cex.lab=1.8)



for(i in 2:100){
  
  
  
  if(mu <= result[i,2] & mu >= result[i,1]){
    
    col="black"
    
  }else{
    
    col="blue"
    
  }
  
  points(result[i,], c(101-i, 101-i), type="l", col=col)
  
}



abline(v=mu, col="red", lty=2)



legend("topright", "True mean", lty=2, col="red", cex=2)



dev.off()







#Bootstrap



Boost.CI=function(X,B,alpha){
  
  n=length(X)
  
  bar.x=mean(X)
  
  
  
  T.b=rep(0,B)
  
  
  
  for(i in 1:B){
    
    x.b=sample(X,n, replace=T)
    
    T.b[i]=(mean(x.b)-bar.x)/(sd(x.b)/sqrt(n))
    
  }
  
  
  
  temp=order(T.b)
  
  
  
  cu=T.b[temp[B*(1-alpha/2)]]
  
  cl=T.b[temp[B*(alpha/2)]]
  
  
  
  CI=bar.x -c(cu, cl)*sd(X)/sqrt(n)
  
  CI
  
}







X=rexp(20, rate=2)

B=1000

alpha=0.05



Boost.CI(X, B, alpha)

CI.t(X,alpha)





X=rnorm(20)

B=1000

alpha=0.05



Boost.CI(X, B, alpha)

CI.t(X,alpha)

