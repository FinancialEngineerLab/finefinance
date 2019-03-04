rm(list=ls())



### L2 LLN : 대수론 ###

n=10000

mean=10

sd=1



Index=c(10, 50, 100, 200,  500, 700,  1000, 2000, 3000, 4000, 5000, 10000)



Result=matrix(0, length(Index), 1000)



for(j in 1:1000){
  
  
  
  X=rnorm(n, mean, sd)
  
  
  
  Result1=rep(0,length(Index))
  
  
  
  for(i in 1:length(Index)){
    
    m=Index[i]
    
    Result1[i] = mean(X[1:m])
    
  }
  
  
  
  Result[,j] =Result1
  
}



plot(Index,Result[,1], type="l", xlab="Sample size", ylab="Estimated value",
     
     ylim=c(9,11) )



for(i in 2:1000){
  
  points(Index,Result[,i], type="l")
  
}





#--------------------------------

#CLT

#--------------------------------

rm(list=ls())



n=1000

prob=0.3



Index=c(10, 20, 30, 100)



Result=matrix(0, length(Index), 1000)

Result.SD=matrix(0, length(Index), 1000)



for(j in 1:1000){
  
  
  
  X=rbinom(n,1, prob)
  
  
  
  Result1=rep(0,length(Index))
  
  Result.SD1=rep(0,length(Index))
  
  
  
  
  
  for(i in 1:length(Index)){
    
    m=Index[i]
    
    Result1[i] = mean(X[1:m])
    
    Result.SD1[i] = sd(X[1:m])
    
  }
  
  
  
  Result[,j] =Result1
  
  Result.SD[,j] =Result.SD1
  
}







x   = seq(-3,3,length=1000)

y   = dnorm(x,mean=0, sd=1)

mean=prob





par(mfrow=c(2,2))

for(index in 1:4){
  
  
  
  main.name=paste("n=", Index[index], sep="")
  
  temp=sqrt(Index[index])*(Result[index,]-mean)/ Result.SD[index,]
  
  plot(density(temp), col="red", lwd=2, xlab="", main=main.name)
  
  points(x,y, type="l")
  
  if(index==1){
    
    legend("topright", c("N(0,1)", "Empirical"), col=c("black", "red"), lty=c(1,1))
    
  }
  
}

