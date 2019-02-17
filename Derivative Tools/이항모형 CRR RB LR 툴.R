Extra <- function(M,method)
{
  
  if ( (method == "CRR") | (method=="RB"))
  {
    N = 2 * M
    V_M = main(M)
    V_N = main(N)
    
  }
  else
  {
    N = 2 * M -1
    V_M = main(M)
    V_N = main(N)
  }
  
  V_exact = ((V_M * M) - (V_N * N))/(M - N)
  return(V_exact)
}

D1 <- function(S,K,delta=0,r,sigma,Large_T)
{
  result = (log(S/K) + ( r - delta + (sigma^2)/2) * Large_T) / (sigma*sqrt(Large_T))
  return (result)
}

D2 <- function(D1,sigma,dt)
{
  result2 <- D1 - (sigma*sqrt(dt))
  return(result2)
}

material_LR <- function(r,D1,D2,N)
{
  delta_t <- 1/N
  sign_d1 <- ifelse(D1 > 0,1,0)
  sign_d2 <- ifelse(D2 > 0,1,0)
  h_d1 <- 0.5 + sign_d1*sqrt(0.25 - 0.25*exp(-(D1/(N+(1/3)))^2*(N+(1/6))))
  h_d2 <- 0.5 + sign_d1*sqrt(0.25 - 0.25*exp(-(D2/(N+(1/3)))^2*(N+(1/6))))
  q <- h_d2
  q_star <- h_d1
  u <- exp(r * delta_t)*(q_star / q)
  d <- (exp(r* delta_t) - q*u) /(1-q)
  return(c(u,d,q,q_star))
}


CRR_RB_N_P <- function(r,Large_T,u,d,tree_size)
{
  delta_t <- Large_T/tree_size
  p <- (exp(r*delta_t) - d ) / (u-d)
  return(p)
}

CRR_RB_U <- function(r,Large_T,sigma,tree_size,method)
{
  if ( method == "CRR")
  {
    delta_t <- Large_T / tree_size
    U <- exp(sigma*sqrt(delta_t))
    D <- 1 / U
  }
  else if( method == "RB")
  {
    delta_t <- Large_T / tree_size
    U<- exp((r-0.5*sigma^2)*delta_t + sigma*sqrt(delta_t))
    D<- exp((r-0.5*sigma^2)*delta_t - sigma*sqrt(delta_t))
  }
  
  return(c(U,D))
}

# CRR Stock price
Stock_ji <- function(S,r,u,i,j,method,sigma,Large_T,tree_size,D1,D2)
{
  if ((method == "CRR") |(method=="RB"))
  {
    u <- CRR_RB_U(r,Large_T,sigma,tree_size,method)[1]
    d <- CRR_RB_U(r,Large_T,sigma,tree_size,method)[2]
  }
  else
  {
    u <- material_LR(r,D1,D2,tree_size)[1]
    d <- material_LR(r,D1,D2,tree_size)[2]
  }
  stockprice <- S * u^((2*i) - j)
  return(stockprice)
}


Option_ji <- function(S,K,r,sigma,delta_t,Cu,Cd,method,N,Large_T)
{
  d1 <-D1(S,K,delta=0,r,sigma,Large_T)
  d2 <-D2(d1,sigma,Large_T)
  if((method == "CRR")|(method=="RB"))
  {
    u <- CRR_RB_U(r,Large_T,sigma,N,method)[1]
    d <- CRR_RB_U(r,Large_T,sigma,N,method)[2]
    N_P <- CRR_RB_N_P(r,Large_T,u,d,N)
    option <- exp(-r*delta_t)*(N_P*Cu + (1-N_P)*Cd)
  }
  else
  {
    u <- material_LR(r,d1,d2,N)[1]
    d <- material_LR(r,d1,d2,N)[2]
    N_P <- material_LR(r,d1,d2,N)[3]
    option <- exp(-r*delta_t)*(N_P*Cu + (1-N_P)*Cd)
  }
  return(option)
}


# Main option Pricing

main <- function(tree_size,method)
{
  
  S = 100
  K = 105
  r = 0.1
  D = 0
  sigma = 0.3
  Large_T = 0.2
  N <- tree_size
  delta_t <- Large_T / N
  method <- method
  
  Stock_matrix <- matrix(numeric((N+1)*(N+1)),nrow = (N+1), ncol = (N+1))
  Option_matrix<- matrix(numeric((N+1)*(N+1)),nrow = (N+1), ncol = (N+1))
  
  D1 <- D1(S,K,delta=0,r,sigma,Large_T)
  D2 <- D2(D1,sigma,Large_T)
  
  # Calculate Stock price
  for ( j in (0:N))
  {
    for( i in (0:j))
    {
      Stock_matrix[i+1,j+1] = Stock_ji(S,r,u,i,j,method,sigma,Large_T,N,D1,D2)
    }
  }
  
  # Calculate Option price
  for( j in (N:0))
  {
    if( j == N){
      for( i in (j:0))
      {
        Option_matrix[i+1,j+1] = max( K - Stock_matrix[i+1,j+1] ,0)
      }
    }
    else{
      for( n in (j:0))
      {
        Option_matrix[n+1,j+1] <- Option_ji(S,K,r,sigma,delta_t,Option_matrix[n+2,j+2],Option_matrix[n+1,j+2],method,N,Large_T)
      }
    }
    
  }
  
  return(Option_matrix[1,1])
}

main(50,"LR")
main(50,"RB")
main(50,"CRR")


# b. 

time_seq <- seq(from=51,to=1000,by=2)

LR_put <-c()

for ( t in time_seq)
{
  put <- main(t,"LR")
  LR_put <- c(LR_put,put)
}

LR_put

RB_put =c()

for ( tt in time_seq)
{
  put <- main(tt,"RB")
  RB_put <- c(RB_put,put)
}


CRR_put<-c()

for ( ttt in time_seq)
{
  put <- main(ttt,"CRR")
  CRR_put <- c(CRR_put,put)
}

