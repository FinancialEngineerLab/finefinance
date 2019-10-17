#### Optimal Delta Hedging ####

### Reference : Hull and White (2017)
### Auhtor : Kim Kyokwon and Shin Hyunjin (KAIST)

library(quantmod)
library(TTR)
library(repr)
library(fOptions)
library(RQuantLib)
library(RND)
library(xlsx)
library(quantmod)
library(TTR)
library(repr)
library(fOptions)
library(RQuantLib)
library(RND)
library(xlsx)

## 하나금융 : 086790 
##  
## 
## 

### 하나금융지주 ###
### stock code change as you want
getSymbols("086790.KS", src = "yahoo")
stock <- `086790.KS`[,'086790.KS.Close']
stock_ret <- na.omit(Delt(stock))
stock_real <- stock[index(stock) >= '2018-07-12' & index(stock) <= '2018-08-10',]
stock_ret_hist <- stock_ret[index(stock_ret) >= '2018-01-11' & index(stock_ret) <= '2018-07-12',]
stock_ret_real <- stock_ret[index(stock_ret) >= '2018-07-12' & index(stock_ret) <=  '2018-08-10',]

stock_vol_hist <- sd(stock_ret_hist) * sqrt(252) 
stock_vol_real <- sd(stock_ret_real) * sqrt(252) 
stockprice <- as.numeric(stock_real[1])

cat("hist_vol = " , stock_vol_hist)
cat("real_vol = ", stock_vol_real)
cat("S = ", stock_real[1])


#data1 : 하나금융투자

data_coef <- read.csv("c:/users/shinhyunjin/dropbox/data/Hull and White/data1.csv")

#######

BSMcall <- function(S,X,r,vol,mat){
  d1 = 1/(vol*sqrt(mat))*(log(S/X)+((r+((vol^2)/2))*mat))
  d2 = d1 - vol*sqrt(mat)
  PVX = X/exp(r*mat)
  
  return((pnorm(d1)*S) - (pnorm(d2)*PVX))
}

#####

term_2 <- function(s,vol,mat=1/12,r=0.02, long_short = 'short', stock_path, underlying_amount = 100000){
  
  x <- s * 1.1
  dt <- mat/length(stock_path)
  
  fair_call <- BSMcall(s,x,r,vol,mat)
  
  call_price <- ifelse(long_short == 'short',
                       GBSOption("c", s, x, mat, r, 0, vol*1.15)@price,
                       GBSOption("c", s, x, mat, r, 0, vol*0.85)@price)
  
  fv_call <- ifelse(long_short == 'short', 
                    BSMcall(s,x,r,vol*1.15,mat),
                    BSMcall(s,x,r,vol*0.85,mat))*((1+r)^mat)
  expected_profit <- abs((fair_call - call_price)*((1+r)^mat)) * underlying_amount
  
  print(paste0("expected_profit = ", expected_profit))
  
  Stock_path <- c(s)
  
  delta <- pnorm(1/(vol*sqrt(mat))*(log(s/x)+((r+((vol^2)/2))*mat)))
  delta_path <- c(delta)
  
  vega <- GBSGreeks("Vega", "c",s, x,mat,r,0, vol)
  vega_path <- c(vega)
  
  #  impvol <- compute.implied.volatility(r,mat, s, x, 0, call_price, 0.01, 1)
  # impvol_path <- c(impvol)
  #  mindelta <- delta_path + vega_path*GBSVolatility(call_price,"c",s,x, mat,r,0)
  mindelta <-delta + vega*(data_coef$beta1[1]*delta-data_coef$beta2[1]*delta*delta)/(sqrt(mat)*(s))
  mindelta_path <- c(mindelta)
  
  share_purchased <- ifelse(long_short == "short", delta * underlying_amount,
                            -delta * underlying_amount)
  
  share_path <- c(share_purchased)
  time_left <- mat
  time_path <- c(time_left)
  cost <- ifelse(share_purchased < 0,s * share_purchased - (s * share_purchased*0.001),
                 s*share_purchased)
  cost_path <- c(cost)
  interest <- dt*cost*r
  interest_path <- c(interest)
  cumm_cost <- cost
  option_path <- c(call_price)
  share_holding_path <- c(0)
  PNL_path <- c()
  
  
  
  for (z in c(2:length(stock_path))) {
    S <- as.numeric(stock_path[z])
    Stock_path <- c(Stock_path,S)
    
    time_left <- ifelse(z != length(stock_path), time_left - dt, 0)
    time_path <- c(time_path,time_left)
    
    delta <- pnorm(1/(vol*(time_left^0.5))*(log(S/x)+((r+((vol^2)/2))*time_left)))
    delta_path <- c(delta_path,delta)
    
    #Call_Value <-ifelse(long_short == 'short',
    #  numeric(GBSOption("c", S, x, time_left, r, 0, vol*1.15)@price),
    # numeric(GBSOption("c", S, x, time_left, r, 0, vol*0.85)@price))
    Call_Value <- BSMcall(S,x,r,vol*1.15,time_left)
    option_path <- c(option_path, Call_Value)
    
    vega <- GBSGreeks("Vega", "c",S, x, time_left,r,0, vol)
    vega_path <- c(vega_path, vega)
    
    # impvol <- compute.implied.volatility(r,time_left, S, x, 0, Call_Value, 0.01, 1)
    #impvol_path <- c(impvol_path, impvol)
    
    # GBSVol
    mindelta <- ifelse(S != Stock_path[z],as.numeric(delta + vega*(((data_coef$Intercept[z]/((S-stock_path[z-1])*vega)*sqrt(time_left)*S+data_coef$beta1[z]*delta-data_coef$beta2[z]*delta*delta))/(sqrt(time_left)*(S)))),
                       mindelta)
    mindelta <- ifelse(z == length(stock_path), delta,mindelta)
    mindelta_path <- c(mindelta_path, mindelta)
    
    share_purchased <- ifelse(long_short == "short", (delta - delta_path[z-1]) * underlying_amount
                              ,(delta - delta_path[z-1]) * underlying_amount)
    
    share_path <- c(share_path,share_purchased)
    
    share_holding <- sum(share_path)
    share_holding_path <- c(share_holding_path,share_holding)
    
    cost <- ifelse(share_purchased < 0,
                   S * share_purchased - (S * share_purchased*0.001),
                   S*share_purchased)
    cost_path <- c(cost_path, cost)
    
    cumm_cost <- c(cumm_cost,cumm_cost[z-1]+ cost+interest_path[z-1])  
    
    interest <- ifelse(z != length(stock_path), dt*cumm_cost[z]*r, 0)
    interest_path <- c(interest_path,interest)
    
    
  }
  
  
  path_table <- data.frame(time_path,Stock_path,option_path,mindelta_path, delta_path, vega_path,share_path, cost_path,interest_path,cumm_cost)
  
  Finalcost <- cumm_cost[length(stock_path)]
  hedgecost <- ifelse(long_short == 'short',
                      ifelse(S >= x, Finalcost - (underlying_amount * x *0.999),Finalcost),
                      ifelse(S >= x, Finalcost + (x * underlying_amount) ,Finalcost))
  hedge_profit <- ifelse(long_short == 'short', fv_call * underlying_amount - hedgecost
                         , -(hedgecost + call_price * underlying_amount))
  cat("real profit = ", hedge_profit)
  return(path_table)
}



#long short only change please, 
#underlying amount = 100000
result <- term_2(s =  stockprice, vol = stock_vol_hist, stock_path = stock_real, long_short = "short", underlying_amount = 10)
