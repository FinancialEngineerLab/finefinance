# -*- coding: utf-8 -*-
"""
Created on Thu Oct 24 11:58:13 2019

@author: Shinhyunjin
"""


#----------------------------------------#
###### Simulation Methods for Fiance ####
#----------------------------------------#

## Author : Shin Hyunjin (KAIST)
## Main Reference : Yuxing Yang(2017)

## Contents

# 1. American Option

# 2. Binary Option

# 3. Barrier Option
# 3-1 Down and out Call
# 3-2 Down and in Call
# 3-3 Up and in Call
# 3-4 Up and out Call
# 3-5 Down and out Put
# 3-6 Down and in Put
# 3-7 Up and in Put
# 3-8 Up and out Put

# 4. Double Rebate Option

# 5. Asian Option
# 5-1 Asian Call
# 5-2 Asian Put

# Further Study : Lookback Option ,


#----------------------------------------------------------------------------#

# Load Library

import random
import scipy as sp
from scipy import log, exp, sqrt, stats

#------------------------------------------------------------------------------#

# 2. Binary Option

def terminalStockPrice(S, T, r, sigma):
    
    tao = random.gauss(0, 1.0)
    terminalPrice = S * sp.exp((r-0.5*sigma**2)*T+sigma*sp.sqrt(T)*tao)
    return terminalPrice

def binaryCallPayoff(x, sT,payoff):
    
    if sT >=x:
        return payoff
    else:
        return 0.0

def binaryPutPayoff(x, sT,payoff):
    
    if sT >=x:
        return 0
    else:
        return payoff
    
# input var
S = 40
x = 41
T = 0.5
r = 0.01
sigma = 0.2
fixedPayoff = 10.0
nSimulations = 10000
payoffs = 0 #initial payoff

# binary Call Option Simulations
for i in range(nSimulations):
    
    sT = terminalStockPrice(S, T, r, sigma)
    payoffs += binaryCallPayoff(x, sT, fixedPayoff)
    
# result
    
price = sp.exp(-r*T) * (payoffs/float(nSimulations))
print('Binary options call = %.8f' % price)


#-----------------------------------------------------------------------------#

# 5. Asian Option #
# 쓸데없는 팁 : 평균가 옵션이 아시안 옵션인 이유
# 홍콩시장에선 선물과 옵션의 행사가가 평균가로 정해진다.
# 그래서 HSI, HSCEI 선물 옵션을 거래할때는 행사가를 평균가, VWAP 등을 활용해 접근한다.
# 선물 옵션 만기일에 종가를 조작하는 세력을 방지할 수 있어 진일보된 방법.

# input

s0 = 30
x = 32
T = 3/12
r = 0.015
sigma = 0.18
sp.random.seed(123)
nSimulations = 1000
nSteps = 500
dt = T/ nSteps

# 5-1 Asian Call

asianCall = sp.zeros([nSimulations], dtype = float)

for j in range(0, nSimulations):
    
    sT = s0
    total = 0
    for i in range(0, int(nSteps)):
        e = sp.random.normal()
        sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sp.sqrt(dt))
        total += sT
        price_average = total / nSteps
    
    asianCall[j] = max(price_average - x, 0)
   
# result

asianCallPrice = sp.mean(asianCall)*sp.exp(-r*T)
print('Call price based on average price = ', round(asianCallPrice, 3))#

# 5-1 Asian Put

asianPut = sp.zeros([nSimulations], dtype = float)

for j in range(0, nSimulations):
    
    sT = s0
    total = 0
    for i in range(0, int(nSteps)):
        e = sp.random.normal()
        sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sp.sqrt(dt))
        total += sT
        price_average = total / nSteps
    
    asianPut[j] = max(x-price_average, 0)
   
# result

asianPutPrice = sp.mean(asianPut)*sp.exp(-r*T)
print('Put price based on average price = ', round(asianPutPrice, 3))#
















