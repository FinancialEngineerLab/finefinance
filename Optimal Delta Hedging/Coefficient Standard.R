
### Expected Implied Volatility Coeff Estiamtes ####

### Reference : Hull and White (2017)
### Author : Kim Kyokwon and Shin Hyunjin (KAIST)

### Coeffieicnet ####

library(quantmod)
library(TTR)
library(repr)
library(fOptions)

par(mfrow = c(2,2))
reg_short_table <- c()
reg_long_table <- c()
#code change!!!!!!!!!!!!!!!!!!!!!!!!
getSymbols("000660.KS", src = "yahoo")

#date change!!!!!!!!!!!!!!!!!!!!!!!!!
for (startdate in seq(as.Date('2015-07-13'), as.Date('2016-01-11'), by = "day")){
  
  
  ### stock code change as you want
  stock <- `000660.KS`[,'000660.KS.Close']
  stock_ret <- na.omit(Delt(stock))
  stock_real <- stock[index(stock) >= startdate & index(stock) <= as.Date('2016-01-11') + (365*3),]
  stock_ret_hist <- stock_ret[index(stock_ret) >= '2018-01-11' & index(stock_ret) <= '2018-07-12',]
  stock_ret_real <- stock_ret[index(stock_ret) >= '2015-07-13' & index(stock_ret) <= '2018-07-12',]
  stock_vol_hist <- sd(stock_ret_hist) * sqrt(252) 
  stock_vol_real <- sd(stock_ret_real) * sqrt(252) 
  stockprice <- as.numeric(stock_real[1])
  
  BSMcall <- function(S,X,r,vol,mat){
    d1 = 1/(vol*sqrt(mat))*(log(S/X)+((r+((vol^2)/2))*mat))
    d2 = d1 - vol*sqrt(mat)
    PVX = X/exp(r*mat)
    
    return((pnorm(d1)*S) - (pnorm(d2)*PVX))
  }
  
  
  term_2 <- function(s,vol,mat=0.5,r=0.02, long_short = 'short', stock_path, underlying_amount = 100000){
    
    x <- s * 1.1
    dt <- mat/length(stock_path)
    
    fair_call <- BSMcall(s,x,r,vol,mat)
    
    call_price <- ifelse(long_short == 'short', 
                         BSMcall(s,x,r,vol*1.15,mat),
                         BSMcall(s,x,r,vol*0.85,mat))
    
    fv_call <- ifelse(long_short == 'short', 
                      BSMcall(s,x,r,vol*1.15,mat),
                      BSMcall(s,x,r,vol*0.85,mat))*((1+r)^mat)
    
    expected_profit <- abs((fair_call - call_price)*((1+r)^mat)) * underlying_amount
    
    Stock_path <- c(s)
    delta <- ifelse(long_short=='short',BSMdelta(s,x,r,vol*1.15,mat),BSMvega(s,x,r,vol*0.85,mat))
    delta_path <- c(delta)
    
    
    vega <- ifelse(long_short=='short',BSMvega(s,x,r,vol*1.15,mat),BSMvega(s,x,r,vol*0.85,mat))
    vega_path <- c(vega)
    
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
      S <- stock_path[z]
      Stock_path <- c(Stock_path,S)
      
      time_left <- ifelse(z != length(stock_path), time_left - dt, 0)
      time_path <- c(time_path,time_left)
      
      Call_Value <- ifelse(long_short == "short", BSMcall(S,x,r,vol*1.15,time_left),
                           BSMcall(S,x,r,vol*0.85,time_left)) #long  : 0.85
      option_path <- c(option_path, Call_Value)
      
      delta <- ifelse(long_short=='short',BSMdelta(S,x,r,vol*1.15,time_left),BSMdelta(S,x,r,vol*0.85,time_left))
      delta_path <- c(delta_path,delta)
      
      vega <- ifelse(long_short=='short',BSMvega(S,x,r,vol*1.15,time_left),BSMvega(S,x,r,vol*0.85,time_left))
      vega_path <- c(vega_path, vega)
      
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
    
    path_table <- data.frame(time_path,Stock_path, option_path,delta_path,vega_path, share_path, share_holding_path,cost_path,interest_path,cumm_cost)
    
    Finalcost <- tail(cumm_cost,1)
    hedgecost <- ifelse(long_short == 'short',
                        ifelse(S >= x, Finalcost - (underlying_amount * x *0.999),Finalcost),
                        ifelse(S >= x, Finalcost + (x * underlying_amount) ,Finalcost))
    
    return(path_table)
    
    
  }
  
  #long short only change please, 
  #underlying amount = 10
  result1 <- term_2(s =  stockprice, vol = stock_vol_hist, stock_path = stock_real, 
                    underlying_amount = 10,long_short = "long")
  result2 <- term_2(s =  stockprice, vol = stock_vol_hist, stock_path = stock_real, 
                    underlying_amount = 10,long_short = "short")
  
  
  # long -> 주의 : 옵션가격 0.85 vol
  #result1$
  y1 <- (result1$option_path-Lag(result1$option_path,1)) - result1$delta_path *(result1$Stock_path- Lag(result1$Stock_path,1))
  b1 <- result1$delta_path* result1$vega_path /sqrt(result1$time_path)*(result1$Stock_path- Lag(result1$Stock_path,1))/result1$Stock_path
  c1 <- result1$delta_path * result1$delta_path*result1$vega_path /sqrt(result1$time_path)*(result1$Stock_path- Lag(result1$Stock_path,1))/result1$Stock_path
  
  reg1 <- coefficients(lm(y1~b1+c1))
  reg_long_table <- rbind(reg_long_table,reg1)
  
  # short-> 주의 : 옵션가격 1.15 vol
  
  #result1$
  y2 <- (result2$option_path-Lag(result2$option_path,1)) - result2$delta_path *(result2$Stock_path- Lag(result2$Stock_path,1))
  b2 <- result2$delta_path* result2$vega_path /sqrt(result2$time_path)*(result2$Stock_path- Lag(result2$Stock_path,1))/result2$Stock_path
  c2 <- result2$delta_path * result2$delta_path*result2$vega_path /sqrt(result2$time_path)*(result2$Stock_path- Lag(result2$Stock_path,1))/result2$Stock_path
  
  reg2 <- coefficients(lm(y2~b2+c2))
  reg_short_table <- rbind(reg_short_table,reg2)
  
}

row.names(reg_long_table) <- seq.Date(as.Date('2015-07-13'), as.Date('2016-01-11'), by = "day")
reg_long_table <- as.data.frame(reg_long_table)
row.names(reg_short_table) <- seq.Date(as.Date('2015-07-13'), as.Date('2016-01-11'), by = "day")
reg_short_table <- as.data.frame(reg_short_table)   