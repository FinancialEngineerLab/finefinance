### Bootstrap Curve ###

rm(list=ls())

raw.data <- read.csv('C:/Users/Shinhyunjin/Dropbox/data1(creditrisk hw1).csv', header=T, fileEncoding="UTF-8-BOM")
#nrow(raw.data)
raw.data <- raw.data[1:12,5:6]
#raw.data

term <- raw.data[,1]
rate <- raw.data[,2]/100

# discount factor bootstrap

tenor.set <- seq(0,12)
#df <- NULL
z <- NULL
z[1] <- 1/((1+term[1]*rate[1])*(1+(term[2]-term[1])*rate[2]))
libor <- NULL

for(i in 2:length(tenor.set)){
  if (i==1) z[1] <- 1/((1+term[1]*rate[1])*(1+(term[2]-term[1])*rate[2]))
  else z[i]  <- z[1] / (1+(term[i] - term[i-1])*rate[i])
 # df[i] <- z[i]
  libor[i] <- rate[i] * z[i]
}

#source('test.r')
plot(tenor.set, libor, type = "s", xlab= "years", ylab = "rate(%)",main ="bootstrapped Libor Curve")