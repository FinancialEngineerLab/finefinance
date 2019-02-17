
####  Interest Swap ####

term.raw <- c(0.25,0.5,0.75,1,1.5,2,3,4,5,6,8,9,10,11,12,15,20)
term.ma <- matrix(term.raw, nrow = 20, ncol= 1)
term.ma
rate.raw <- c(1.8,1.7325,1.7175,1.715,1.745,1.7225,1.8675,1.9525, 2.04, 2.1125, 2.155, 2.195, 2.2325, 2.275,2.31,2.345,2.395,2.51)
rate.ma <- matrix(rate.raw, nrow=20, ncol=1)
term.raw
rate.raw

term <- term.ma[,1]
rate<-rate.ma[,1]/100
f <- approxfun(term,rate)

tenor.set <- seq(0,20,0.25)
df <- NULL
df[1] <- 1
for ( i in 2:length(tenor.set)){
  irs_rate <- f(i*0.25-0.25)
  if(i==2) bunja <- 1
  else bunja <- 1- sum(df[2:(i-1)])*0.25*irs_rate
  df[i] <- bunja/(1+irs_rate*0.25)
}
dfx <- approxfun(tenor.set,df)
plot(tenor.set, df, xlab= "tenor(year)", ylab="discount factor", ylim = c(dfx(20),1))

### 2. valuation of interest swap ###

swap.value <- function(Tswap, fix_rate, payrec = 1, effective =0){
  payments <- seq(effective + 0.25, Tswap, 0.25)
  pv_fix_leg <- sum(dfx(payments)*fix_rate*0.25) #fixed-leg pv 
  if(effective==0) pv_float_leg <- 1-dfx(Tswap) #float leg pv(FRN(t=0) = FAceValue)
  else pv_float_leg <- dfx(effective)-dfx(Tswap)
  ans <- (pv_fix_leg - pv_float_leg)*payrec
  return(ans)
}

x <- swap.value(4, 1.9625/100)*10000
x

### 3. Forward Swap rate ###

calc.fsr <- function(Tfwd, Tswap){
  tset <- seq(Tfwd+0.25, Tfwd+Tswap, 0.25)
  bunja = 0
  bunmo = 0
  for (t in test){
    d0 = dfs(t-0.25)
    d1 = dfs(t)
    frate = (d0/d1-1)*4
    bunja = bunja + 0.25*frate*dfs(t)
    bunmo = bunmo + 0.25*dfs(t)
    
  }
  ans = bunja/bunmo
  return(ans)
}

