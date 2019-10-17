
#### Optimal Delta Hedging ####
### Modified Delta hedging
### Reference : Hull and White (2017)
### Auhtor : Kim Kyokwon and Son Yejun (KAIST)

library(quantmod)
library(TTR)
library(repr)
library(quantmod)
library(TTR)
library(repr)
library(fOptions)
library(RQuantLib)
library(RND)
library(xlsx)

par(mfrow = c(2,2))

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


BSMcall <- function(S,X,r,vol,mat){
  d1 = 1/(vol*sqrt(mat))*(log(S/X)+((r+((vol^2)/2))*mat))
  d2 = d1 - vol*sqrt(mat)
  PVX = X/exp(r*mat)
  
  return((pnorm(d1)*S) - (pnorm(d2)*PVX))
}


term_2 <- function(s,vol,mat=1/12,r=0.02, long_short = 'short', stock_path, underlying_amount = 10){
  
  x <- s * 1.1
  dt <- mat/length(stock_path)
  
  fair_call <- as.numeric(GBSOption("c", s, x, mat, r, 0, vol)@price)
  
  call_price <- ifelse(long_short == 'short',
                       as.numeric(GBSOption("c", s, x, mat, r, 0, vol*1.15)@price),
                       as.numeric(GBSOption("c", s, x, mat, r, 0, vol*0.85)@price))
  
  fv_call <- ifelse(long_short == 'short', 
                    as.numeric(GBSOption("c", s, x, mat, r, 0, vol*1.15)@price),
                    as.numeric(GBSOption("c", s, x, mat, r, 0, vol*0.85)@price))*((1+r)^mat)
  
  expected_profit <- abs((fair_call - call_price)*((1+r)^mat)) * underlying_amount
  
  print(paste0("expected_profit = ", expected_profit))
  
  Stock_path <- c(s)
  
  delta <- pnorm(1/(vol*sqrt(mat))*(log(s/x)+((r+((vol^2)/2))*mat)))
  delta_path <- c(delta)
  
  share_purchased <- ifelse(long_short == "short", delta * underlying_amount,
                            -delta * underlying_amount)
  share_path <- c(share_purchased)
  time_left <- mat
  time_path <- c(time_left)
  cost <- ifelse(share_purchased < 0,s * share_purchased + (s * share_purchased*0.001),
                 s*share_purchased)
  cost_path <- c(cost)
  interest <- dt*cost*r
  interest_path <- c(interest)
  cumm_cost <- cost
  option_path <- c(call_price)
  share_holding_path <- c(share_purchased)
  PNL_path <- c()
  
  
  
  for (z in c(2:length(stock_path))) {
    S <- as.numeric(stock_path[z])
    Stock_path <- c(Stock_path,S)
    
    time_left <- ifelse(z != length(stock_path), time_left - dt, 0)
    time_path <- c(time_path,time_left)
    #modified delta
    Call_plus <- as.numeric(GBSOption("c", S*1.001, x, mat, r, 0, vol)@price)
    Call_minus <- as.numeric(GBSOption("c", S*0.999, x, mat, r, 0, vol)@price)
    
    Call_Value <- as.numeric(GBSOption("c", S, x, mat, r, 0, vol*1.15)@price)
    option_path <- c(option_path, Call_Value)
    
    delta <- 0.5*((Call_plus)/(0.001*S)+(Call_minus)/(-0.001*S))
    delta_path <- c(delta_path,delta)
    
    share_purchased <- ifelse(long_short == "short", (delta - delta_path[z-1]) * underlying_amount
                              ,-(delta - delta_path[z-1]) * underlying_amount)
    share_path <- c(share_path,share_purchased)
    
    share_holding <- sum(share_path)
    share_holding_path <- c(share_holding_path,share_holding)
    
    cost <- ifelse(share_purchased < 0,
                   S * share_purchased + (S * share_purchased*0.001),
                   S*share_purchased)
    cost_path <- c(cost_path, cost)
    
    cumm_cost <- c(cumm_cost,cumm_cost[z-1]+ cost+interest_path[z-1])  
    
    interest <- ifelse(z != length(stock_path), dt*cumm_cost[z]*r, 0)
    interest_path <- c(interest_path,interest)

    
  }
  
  path_table <- data.frame(time_path,Stock_path, delta_path, share_path, share_holding_path,cost_path,interest_path,cumm_cost)
  
  Finalcost <- tail(cumm_cost,1)
  hedgecost <- ifelse(long_short == 'short',
                      ifelse(S >= x, Finalcost - (underlying_amount * x *0.999),Finalcost),
                      ifelse(S >= x, Finalcost + (x * underlying_amount) ,Finalcost))
  hedge_profit <- ifelse(long_short == 'short', fv_call*underlying_amount - hedgecost
                         , -(hedgecost + fv_call*underlying_amount))
  cat("real profit = ", hedge_profit)
  plot(path_table$Stock_path)
  plot(path_table$share_holding_path)
  plot(path_table$cumm_cost)
  plot(path_table$delta_path)
  return(path_table)
  
  
}

#long short only change please, 
#underlying amount = 100000
#result1 <- term_2(s =  stockprice, vol = stock_vol_hist, stock_path = stock_real, 
#                  underlying_amount = 10,long_short = "long")
result2 <- term_2(s =  stockprice, vol = stock_vol_hist, stock_path = stock_real, 
                  underlying_amount = 10,long_short = "short")
