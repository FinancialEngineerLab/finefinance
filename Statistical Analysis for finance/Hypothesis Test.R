rm(list=ls())



#Generate hypothesis tests 1000 times under H0

#

# H0: mu = 10 vs H1: mu neq 10

# sigma =2

# n= 16



n=16

sig=2

mu=10

alpha=0.05





run=function(n, sig, mu, alpha){
  
  #Generate random variables from N(mu, 2)
  
  
  
  x=rnorm(n, mu, sig) 
  
  
  
  #calculate p-value
  
  Tobs= (mean(x) - mu)/(sig/sqrt(n))
  
  pvalue=2*(1- pnorm(abs(Tobs),0,1))
  
  
  
  
  
  Result=pvalue<alpha
  
  
  
  Result
  
}



result=rep(0, 1000) 

for(i in 1:1000){
  
  result[i]=run(n,sig,mu,alpha)
  
}



mean(result)

