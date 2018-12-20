# -*- coding: utf-8 -*-
"""
Created on Thu Dec 20 12:50:14 2018

@author: Shinhyunjin
"""

### 몬테카를로 시뮬레이션 ###

import scipy as sp
import scipy.stats as stats
import matplotlib.pyplot as plt
import numpy as np
from scipy import sqrt,exp,log,pi
from scipy import zeros, sqrt, shape
import random
import pandas as pd
import os
from datetime import datetime as dt
from scipy.optimize import minimize
from scipy.stats import norm

## 1. 정규분포 ##

#표준 정규분포로부터 난수 생성
x = sp.random.standard_normal(size=10)
print(x)
x = sp.arange(-3,3,0.01)
y = stats.norm.pdf(x)
plt.plot(x,y)
plt.title("standard normal disstribution")
plt.xlabel('x')
plt.ylabel('y')
plt.show()

#시드 난수
sp.random.seed(12345)
x = sp.random.normal(0,1,20)
print(x[:5])

#정규분포 난수 생성
mean = 0.1
std = 0.2
n = 1000
x = sp.random.normal(mean,std,n)
print(x[:5])
#
plt.hist(x, 15, normed=True)
plt.title("Histogram for random numbers drawn from a normal distribution")
plt.annotate("mean="+str(mean), xy=(0.6, 1.5))
plt.annotate("std="+str(std), xy=(0.6, 1.4))
plt.show()

# 로그정규분포
x = np.linspace(0.001, 3, 200)
mu = 0
sigma0 = [0.25, 0.5, 1]
color = ['blue','red','green']
target = [(1.2, 1.3),(1.7, 0.4),(0.18,0.7)]
start = [(1.8, 1.4), (1.9, 0.6), (0.18, 1.6)]
#
for i in sp.arange(len(sigma0)):
    sigma = sigma0[i]
    y = 1/(x*sigma*sqrt(2*pi))*exp(-(log(x)-mu)**2/(2*sigma*sigma))
    plt.annotate('mu='+str(mu)+', sigma='+str(sigma), xy = target[i], xytext = start[i], arrowprops=dict(facecolor=color[i], shrink=0.01),)
    plt.plot(x,y, color[i])
    plt.title('Lognormal distribution')
    plt.xlabel('x')
    plt.ylabel('lognormal density distribution')
plt.show()

## 2.유니폼분포(주사위, 파이 값 계산) ##

x = sp.random.uniform(low=1, high=100, size=10)
print(x[0:5])

#주사위
def rollDice():
    roll = random.randint(1,6)
    return roll
i = 1
n=10
result=[]
random.seed(123)
while i<n:
    result.append(rollDice())
    i+=1
print(result)

#파이값 계산(다트 응용)

n = 1000000
x = sp.random.uniform(low=0, high=1, size = n)
y = sp.random.uniform(low=0, high=1, size=n)
dist = sp.sqrt(x**2 + y**2)
in_circle = dist[dist<=1]
our_pi = len(in_circle)*4./n
print('pi=', our_pi)
print('error (%)=', (our_pi-sp.pi)/sp.pi)

## 3. 포아송 분포 ##

x = sp.random.poisson(lam=1, size=100)
a = 5.
n = 10000
s = np.random.power(a,n)
count,bins,ignored = plt.hist(s, bins=30)
x = np.linspace(0,1,100)
y = a*x**(a-1.)
normed_y = n*np.diff(bins)[0]*y
plt.title("Poisson distribution")
plt.ylabel("Y")
plt.xlabel("X")
plt.plot(x, normed_y)
plt.show()


## 4. 파생상품과 몬테카를로 시뮬레이션 ##

#4-1 주가 변동 시뮬레이션
stock_price_today = 9.15
T =1.
n_steps=100
mu = 0.1
sigma= 0.2
sp.random.seed(12345)
n_simulation = 5
dt = T/n_steps
#
S=sp.zeros([n_steps],dtype=float)
x = range(0, int(n_steps),1)
for j in range(0, n_simulation):
    S[0] = stock_price_today
    for i in x[:-1]:
        e = sp.random.normal()
        S[i+1] = S[i] + S[i]*(mu-0.5*pow(sigma,2))*dt + sigma*S[i]*sp.sqrt(dt)*e
    plt.plot(x,S)
plt.figtext(0.2,0.8, 'S0='+str(S[0])+',mu='+str(mu)+',sigma='+str(sigma))
plt.figtext(0.2,0.76, 'T='+str(T)+',steps='+str(int(n_steps)))
plt.title('Stock Price ( number of simulations = %d' %n_simulation+')')
plt.xlabel('Total number of steps='+str(int(n_steps)))
plt.ylabel('stock price')
plt.show()

#4-2 옵션 만기일의 주가 그래프
S0 = 9.15
T =1.
n_steps=100
mu = 0.15
sigma= 0.2
sp.random.seed(12345)
n_simulation = 1000
dt = T/n_steps
S= zeros([n_simulation], dtype = float)
x = range(0,int(n_steps),1)
for j in range(0, n_simulation):
    tt = S0
    for i in x[:-1]:
        e = sp.random.normal()
        tt+=tt*(mu-0.5*pow(sigma,2))*dt + sigma*tt*sqrt(dt)*e
        S[j]=tt
plt.title('Histogram of terminal price')
plt.ylabel('number of frequencies')
plt.xlabel('Terminal price')
plt.figtext(0.5,0.8, 'S0='+str(S0)+',mu='+str(mu)+',sigma='+str(sigma))
plt.figtext(0.5,0.76, 'T='+str(T)+',steps='+str(int(n_steps)))
plt.figtext(0.5,0.72,'Number of terminal prices ='+str(int(n_simulation)))
plt.hist(S)
plt.show()
#상관관계
sp.random.seed(123)
n = 1000
rho = 0.3
x1 = sp.random.normal(size=n)
x2 = sp.random.normal(size=n)
y1=x1
y2 = rho*x1 + sp.sqrt(1-rho**2)*x2
print(sp.corrcoef(y1,y2))

#4-3 시뮬레이션을 이용한 블랙스콜스머톤 콜 복제
S0 = 40.
X = 40.
T = 0.5
r = 0.05
sigma = 0.2
n_steps = 100
sp.random.seed(12345)
n_simulation = 5000
dt = T/n_steps
call = sp.zeros([n_simulation], dtype = float)
x = range(0, int(n_steps),1)
for j in range(0, n_simulation):
    sT = S0
    for i in x[:-1]:
        e = sp.random.normal()
        sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sqrt(dt))
        call[j]  = max(sT-X,0)
call_price = sp.mean(call)*sp.exp(-r*T)
call_price

## 4. 포트폴리오 이론과 몬테카를로 시뮬레이션 ##

#4-1 임의의 주식 선택 
n_stocks = 10
x = pd.read_pickle('c:/users/shinhyunjin/dropbox/data/yanMonthly.pkl')
x2=sp.unique(np.array(x.index))
x3=x2[x2<'ZZZZ']
sp.random.seed(1234567)
nonStocks = ['GOLDPRICE','HML','SMB','Mkt_Rf','Rf','Russ3000E_D','US_DEBT','Russ3000E_X','US_GDP2009dollar','US_GDP2013dollar']
x4 = list(x3)
#
for i in range(len(nonStocks)):
    x4.remove(nonStocks[i])
#
k=sp.random.uniform(low=1, high=len(x4), size=n_stocks)
y,s = [],[]
for i in range(n_stocks):
    index=int(k[i])
    y.append(index)
    s.append(x4[index])
#
final = sp.unique(y)
print(final)
print(s)

#4-2 복원 비복원 시뮬레이션

def boots_f(data,n_obs, replacement=None):
    n = len(data)
    if (n<n_obs):
        print("n is less than n_obs")
    else:
        if replacement==None:
            y = np.random.permutation(data)
            return y[0:n_obs]
        else:
            y=[]
    for i in range(n_obs):
        k = np.random.permutation(data)
        y.append(k[0])
    
    return y

#5. 시뮬레이션 VaR
position = 1e6
std = 0.2
mean = 0.08
confidence = 0.99
nSimulations =50000
ret2 = sp.random.normal(mean, std, nSimulations)
ret3 = np.sort(ret2)
m = int(nSimulations*(1-confidence))
VaR2 = position*(ret3[m])
VaR2

#6. n개 주식 효율적 투자선
 
nStocks = 20
sp.random.seed(1234)
n_corr = nStocks*(nStocks-1)/2
corr_0  = sp.random.uniform(0.05, 0.25, 190)
mean_0 = sp.random.uniform(-0.1, 0.25, nStocks)
std_0 = sp.random.uniform(0.05,0.35,nStocks)
nSimulations = 1000
#촐레스키
corr_=sp.zeros((nStocks,nStocks))
for i in range(nStocks):
    for j in range(nStocks):
        if i==j:
            corr_[i,j]=1
        else:
            corr_[i,j]=corr_0[i+j]
#
R0 = np.zeros((nSimulations, nStocks))
for i in range(nStocks):
    for j in range(nStocks):
        R0[i,j] = sp.random.normal(loc=mean_0[j],scale=std_0[j], size=1)
if(R0.any()<=-1.0):
    print('Error:return is <=-100%')
# 상관 수익률 행렬 촐레스키
R = np.array(R0)
# 최종함수 정의
def objFunction(W,R,target_ret):
    stock_mean = np.mean(R, axis=0)
    port_mean = np.dot(W, stock_mean)
    cov = np.cov(R.T)
    port_var = np.dot(np.dot(W,cov),W.T)
    penalty = 2000*abs(port_mean-target_ret)
    return np.sqrt(port_var) + penalty
#최적 포트폴리오
out_mean, out_std, out_weight = [],[],[]
stockMean = np.mean(R, axis=0)
#
for r in np.linspace(np.min(stockMean), np.max(stockMean), num=100):
    W = sp.ones([nStocks])/nStocks #시작 W
    b_ = [(0,1) for i in range(nStocks)] #한계
    c_ = ({'type':'eq', 'fun': lambda W: sum(W)-1.}) # 제약조건
    result = minimize(objFunction, W, (R,r), method = 'SLSQP', constraints=c_, bounds=b_)
    if not result.success:
        raise BaseException(result.message)
    out_mean.append(round(r,4))
    std_ = round(np.std(np.sum(R*result.x, axis=1)),6)
    out_std.append(std_)
    out_weight.append(result.x)
# Graph
plt.title('Simulation for an Efficient Frontier : '+str(nStocks)+' stocks')
plt.xlabel('Standard deviation of the portfolio')
plt.ylabel('return of the n-stock portfolio')
plt.plot(out_std, out_mean, '--', linewidth = 3)
plt.show()


#7. 자본예산 #

#input
nYear=5
costEquipment = 5e6 #5백만
n=nYear+1 # 첫해 더하기
otherCost = 100000
sellingCost = 1500
R_and_D = 200000
costRawMaterials=0.3
tax = 0.38
R  = 0.15
thousand = 1e3 # 천단위
million  = 1e6 #백만단위
#불확실성 변수
nSimulation=100
lowPrice =10 #주식가격
highPrice= 30
lowUnit = 50*thousand # 판매량
highUnit = 200 * thousand
lowRate = 0.15 #할인률
highRate = 0.25
#
n2 = nSimulation
sp.random.seed(123)
price0 = sp.random.uniform(low=lowPrice, high=highPrice, size = n2)
units0 = sp.random.uniform(low=lowUnit, high=highUnit, size=n2)
#
npv = []
for i in sp.arange(nSimulation):
    units = sp.ones(n)*units0[i]
    price = price0[i]
    sales = units*price
    sales[0] = 0
    cost1 = costRawMaterials*sales
    cost2 = sp.ones(n)*otherCost
    cost3 = sp.ones(n)*sellingCost
    cost4 = sp.zeros(n)
    cost4[0] = costEquipment
    RD = sp.zeros(n)
    RD[0] = R_and_D #시각0의 R&D
    D = sp.ones(n)*costEquipment/nYear #정액법
    D[0] = 0
    EBIT = sales-cost1-cost2-cost3-cost4-RD-D
    NI = EBIT*(1-tax)
    FCF = NI + D
    npvProject = sp.npv(R, FCF)/million # NPV
    npv.append(npvProject)
#표시
print("mean NPV of project=",round(sp.mean(npv),0))
print("min NPV of project", round(min(npv),0))
print("max NPV of project", round(max(npv),0))
plt.title("NPV of the project : 3 uncertaintties")
plt.xlabel("NPV (in million)")
plt.hist(npv, 50, range = [-3,6], facecolor = 'blue', align = 'mid')
plt.show()
