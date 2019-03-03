# -*- coding: utf-8 -*-
"""
Created on Wed Dec 19 22:49:19 2018

@author: Shinhyunjin
"""

#### 이색옵션 Exotic Option Pricing ####

from math import exp, sqrt
import numpy as np
from scipy import log, exp, sqrt,stats
import random
import scipy as sp
import statsmodels.sandbox.distributions.extras as extras
from scipy import zeros, sqrt, shape
import matplotlib.pyplot as plt

### 1. 버뮤다식 옵션 ###
# 버뮤다식 옵션이란 미리 정해진 날짜에 한 번 이상의 권리를 행사할 수 있다.

def callBermudan(s,x,T,r,sigma, T2, n=100): #T2는 조기행사 가능일
    n2 = len(T2) 
    deltaT = T/n
    u = exp(sigma*sqrt(deltaT))
    d = 1.0  / u
    a = exp(r*deltaT)
    p = (a-d)/(u-d)
    v = [[0.0 for j in np.arange(i+1)] for i in np.arange(n+1)]
    for j in np.arange(n+1):
        v[n][j] = max(s*u**j * d ** (n-j) - x, 0.0)
    for i in np.arange(n-1, -1,-1):
        for j in np.arange(i+1):
            v1 = exp(-r*deltaT)*(p*v[i+1][j+1]+(1.0-p)*v[i+1][j])
            for k in np.arange(n2):
                if abs(j*deltaT-T2[k])<0.01:
                    v2 = max(v[i][j]-x,0) #잠재적 조기 행사
                else:
                    v2= 0
            v[i][j] = max(v1,v2)
    return v[0][0]

s = 40
x = 40
T = 6./12
r = 0.05
sigma = 0.2
n = 1000
T2 = (3./12., 4./12.)
callBermudan(s, x, T, r, sigma, T2, n)

### 2. 선택옵션 ###
#선택옵션이란 옵션 구매자가 미리 정한 특정 기한 이전에 유럽식 콜옵션 or 풋옵션으로 결정할 수 있게함

def callAndPut(S,X,T,r,sigma,tao,type='C'):
    d1 = (log(S/X)+r*T+0.5*sigma*sigma*tao)/(sigma*sqrt(tao))
    d2 = d1 -sigma*sqrt(tao)
    if type.upper()=='C':
        c= S*stats.norm.cdf(d1) - X*exp(-r*T)*stats.norm.cdf(d2)
        return c
    else :
        p = X*exp(-r*T)*stats.norm.cdf(-d2)-S*stats.norm.cdf(-d1)
    return p
#
def chooserOption(S,X,T,r,sigma, tao):
    call_T = callAndPut(S,X,T,r,sigma,T)
    put_tao = callAndPut(S,X,T,r,sigma,tao,type='P')
    return call_T - put_tao

tao = 1./12.0

chooserOption(s,x,T,r,sigma,tao)

### 3. 샤우트옵션 ###
# long position이 판매자에게 만기 전에 최소 수익 Stao-X를 확정할 수 있다(샤우트)

def shoutCall(s,x,T,r,sigma,shout,n=100):
    deltaT = T/n
    u = exp(sigma*sqrt(deltaT))
    d = 1.0/u
    a = exp(r*deltaT)
    p = (a-d) / (u-d)
    v = [[0.0 for j in np.arange(i+1)] for i in np.arange(n+1)]
    for j in np.arange(n+1):
        v[n][j] = max(s*u**j * d**(n-j)-x, 0.0)
    for i in np.arange(n-1, -1,-1):
        for j in np.arange(i+1):
            v1 = exp(-r*deltaT)*(p*v[i+1][j+1]+(1.0-p)*v[i+1][j])
            v2 = max(v[i][j]-shout, 0)
            v[i][j] = max(v1,v2)
    return v[0][0]

shout = (1+0.03)*s #shout 

shoutCall(s,x,T,r,sigma,shout, n)

### 4.바이너리옵션 - asset or nothing option ###
#옵션이 내가격으로 만기되면 정해진 고정 금액을 지불하고 외각으로 만기되면 소멸해버림

def terminalStockPrice(S,T,r,sigma):
    tao = random.gauss(0, 1.0)
    terminalPrice = S*sp.exp((r-0.5*sigma**2)*T + sigma*sp.sqrt(T)*tao)
    return terminalPrice

def binaryCallPayoff(x,sT,payoff):
    if sT >= x:
        return payoff
    else:
        return 0.0
    
#시뮬레이션
        
fixedPayoff = 10.0 #수익
payoffs = 0.0 
nSimulations = 10000
S =40
for i in np.arange(nSimulations):
    sT = terminalStockPrice(S,T,r,sigma)
    payoffs += binaryCallPayoff(x,sT,fixedPayoff)

BinaryOptionPrice = sp.exp(-r*T)*(payoffs / float(nSimulations))

### 5. 레인보우 옵션 
#최소값과 최대값의 연계옵션, 이변수 정규분포를 따름, 주식 간 상관관계 존재
#callpayoff = max(max(st1,st2,st3)-x,0)

#시뮬레이션
#1. 사전에 정한 상관관계 구하기
sp.random.seed(123)
n = 1000
rho = 0.3
x1 = sp.random.normal(size = n)
x2 = np.random.normal(size = n)
y1 = x1
y2 = rho*x1 + sp.sqrt(1-rho**2)*x2
print(sp.corrcoef(y1,y2))
#2.촐레스키분해
nSimulation= 5000
c = np.array([[1, 0.5, 3],[0.5,1,0.4],[0.3,0.4,1]])
np.random.seed(123) #난수고정
x = np.random.normal(size=3*nSimulation) #상관되지 않은 난수
U = np.reshape(x, (nSimulation,3))
L = np.linalg.cholesky(c) #촐레스키분해
r = np.dot(U,L) #상관된 난수 발생
print(np.corrcoef(r.T))


##Basic Func
def dOne(s,k,r,sigma,T):
    a = log(s/k) + (r-0.5*sigma**2)*T
    b = (sigma*sqrt(T))
    return a/b
def sigmaA_f(sigma1, sigma2, rho):
    return sqrt(sigma1**2-2*rho*sigma1*sigma2+sigma2**2)
def dTwo(d1, sigma, T):
    return d1+sigma*sqrt(T)
def rhoTwo(sigma1, sigma2, sigmaA,rho):
    return (rho*sigma2-sigma1)/sigmaA
def N2_f(d1,d2,rho):
    muStandardNormal =0.0
    varStandardNormal = 1.0
    upper = ([d1,d2]) #상한
    v = varStandardNormal # 단순화
    mu = muStandardNormal
    covM = ([v,rho],[rho,v])
    return extras.mvnormcdf(upper,mu,covM)
def dOneTwo(s1,s2,sigmaA,T):
    a = log(s2/s1) - 0.5*sigmaA**2*T
    b = sigmaA*sqrt(T)
    return a/b
##Essentail Func
def rainbowCallOnMinimum(s1, s2, k, T, r, sigma1, sigma2, rho):
    d1 = dOne(s1,k,r,sigma1,T)
    d2 = dOne(s2,k,r,sigma2,T)
    d11 = dTwo(d1, sigma1,T)
    d22 = dTwo(d2,sigma2,T)
    sigmaA=sigmaA_f(sigma1,sigma2,rho)
    rho1 = rhoTwo(sigma1, sigma2, sigmaA, rho)
    rho2 = rhoTwo(sigma2, sigma1, sigmaA, rho)
    d12 = dOneTwo(s1, s2, sigmaA,T)
    d21 = dOneTwo(s2, s1, sigmaA, T)
    #
    part1 = s1 * N2_f(d11,d12, rho1)
    part2 = s2 * N2_f(d21,d22, rho2)
    part3 = k*exp(-r*T)*N2_f(d1,d2,rho)
    return part1 + part2- part3

#변수
s1=100
s2=95.
k=102.0
T = 8./12.
r = 0.08
rho = 0.75
sigma1 = 0.15
sigma2 = 0.20
price = rainbowCallOnMinimum(s1, s2, k, T,r, sigma1, sigma2, rho)
price

## 최소값에 대하 레인보우 옵션
#상관된 난수
sp.random.seed(123) #난수고정
s1=100
s2=95.
k=102.0
T = 8./12.
r = 0.08
rho = 0.75
sigma1 = 0.15
sigma2 = 0.20
nSteps = 100.0
nSimulation = 1000
dt = T/nSteps
call = sp.zeros([nSimulation], dtype=float)
x = range(0, int(nSteps), 1)
#콜옵션
for j in range(0, nSimulation):
    x1 = sp.random.normal(size = nSimulation)
    x2 = sp.random.normal(size = nSimulation)
    y1 = x1
    y2 = rho*x1 + sp.sqrt(1-rho**2)*x2
    sT1=s1
    sT2=s2
    for i in x[:-1]:
        e1 = y1[i]
        e2 = y2[i]
        sT1*=sp.exp((r-0.5*sigma1**2)*dt + sigma1*e1*sqrt(dt))
        sT2*=sp.exp((r-0.5*sigma2**2)*dt + sigma2*e2*sqrt(dt))
        minOf2 = min(sT1, sT2)
        call[j] = max(minOf2-k,0)
#최종가격
RainMincall = sp.mean(call) * sp.exp(-r*T)
RainMincall


### 6.평균가옵션###
#평균가 옵션은 아시안옵션으로 기초 자산의 평균가를 기초자산으로 한다 

s0=30.
x = 32.
T = 3.0/12.0
r= 0.025
sigma = 0.18
sp.random.seed(123) 
n_simulation =1000
n_steps=500
#
dt=T/n_steps
call = sp.zeros([n_simulation], dtype = float)
for j in range(0, n_simulation):
    sT = s0
    totla = 0
    for i in range(0, int(n_steps)):
        e = sp.random.normal()
        sT*=sp.exp((r-0.5*sigma*sigma)*dt+sigma*e*sp.sqrt(dt))
        totla+=sT
        price_average=totla/n_steps
    call[j] = max(price_average-x,0)

avg_call_price = sp.mean(call)*sp.exp(-r*T)
avg_call_price

### 7. 배리어옵션 가격 ###

## 7-1 Up and Out 배리어옵션 ##
def bsCall(S,X,T,r,simga):
    d1 = (log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*sqrt(T))
    d2 = d1 - sigma*sqrt(T)
    return S*stats.norm.cdf(d1) - X*exp(-r*T)*stats.norm.cdf(d2)

def up_and_out_call(s0,x,T,r,sigma,n_simulation,barrier):
    n_steps = 100.
    dt=T/n_steps
    total = 0
    for j in sp.arange(0, n_simulation):
        sT=s0
        out=False
        for i in range(0, int(n_steps)):
            e = sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt + sigma*e*sp.sqrt(dt))
            if sT>barrier:
                out = True
        if out ==False:
            total+=bsCall(s0,x,T,r,sigma)
    return total/n_simulation
#
s0=30.
x=30.
barrier=32 #배리어
T = 6./12.
r = 0.05
sigma = 0.2
n_simulation = 100
sp.random.seed(12)

upandoutcall = up_and_out_call(s0,x,T,r,sigma,n_simulation,barrier)
upandoutcall

# 7-2 Down and In 배리어옵션
def bsPut(S,X,T,r,simga):
    d1 = (log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*sqrt(T))
    d2 = d1 - sigma*sqrt(T)
    return X*exp(-r*T)*stats.norm.cdf(-d2) - S*stats.norm.cdf(-d1)

def down_and_in_put(s0,x,T,r,sigma,n_simulation,barrier):
    n_steps=100.
    dt=T/n_steps
    total =0
    for j in range(0, n_simulation):
        sT=s0
        in_=False
        for i in range(0, int(n_steps)):
            e = sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt + sigma*e*sp.sqrt(dt))
            if sT<barrier:
                in_=True
            total+=bsPut(s0,x,T,r,sigma)
    return total/n_simulation

s0=30.
x=30.
barrier=32 #배리어
T = 6./12.
r = 0.05
sigma = 0.2
n_simulation = 100
sp.random.seed(12)

down_and_in_put(s0,x,T,r,sigma,n_simulation,barrier)

# 7-3 배리어 인아웃 패리티
# 배리어에 도달하면 콜1은 무시, 콜2 활성, 배리어에 도달하지 않으면 콜1 활성,콜2무시

def upCall(s,x,T,r,sigma, nSimulation,barrier):
    n_steps = 100
    dt = T/n_steps
    inTotal = 0
    outTotal = 0
    for j in range(0, nSimulation):
        sT=s
        inStatus=False
        outStatus=True
        for i in range(0, int(n_steps)):
            e = sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt + sigma*e*sp.sqrt(dt))
            if sT>barrier:
                outStatus=False
                inStatus =True
        if outStatus==True:
            outTotal+=bsCall(s,x,T,r,sigma)
        else:
            inTotal+=bsCall(s,x,T,r,sigma)
    return outTotal/nSimulation, inTotal/nSimulation

s=40.
x=40.
barrier=42.0
T=0.5
r=0.05
sigma=0.2
nSimulation = 500

upOutCall, upInCall = upCall(s,x,T,r,sigma,nSimulation,barrier)
print(upOutCall, upInCall, bsCall(s,x,T,r,sigma))

# 시뮬레이션과 그래프

s = 9.25
x = 9.1
barrier = 10.5
T = 0.5
n_step=30
r = 0.05
sigma = 0.2
sp.random.seed(125)
n_simulation = 5
n_steps = 30

dt = T/n_steps
S = sp.zeros([n_steps], dtype=float)
time_= range(0, int(n_steps),1)
c = bsCall(s,x,T,r,sigma)
sp.random.seed(124)
outTotal, inTotal = 0., 0.
n_out, n_in = 0, 0

for j in range(0, n_simulation):
    S[0] = s
    inStatus = False
    outStatus = True
    for i in time_[:-1]:
        e = sp.random.normal()
        S[i+1] = S[i]*sp.exp((r-0.05*pow(sigma,2))*dt+sigma*sp.sqrt(dt)*e)
        if S[i+1] > barrier:
            outStatus=False
            inStatus=True
    plt.plot(time_,S)
    if outStatus ==True:
        outTotal+=c;n_out+=1
    else:
        inTotal+=c;n_in+=1
        S = sp.zeros(int(n_steps))+barrier
        plt.plot(time_, S, '.-')
        upOutCall = round(outTotal/n_simulation, 3)
        upInCall = round(inTotal/n_simulation, 3)
        plt.figtext(0.15,0.8, 'S='+str(s)+',X='+str(x))
        plt.figtext(0.15,0.76,'T='+str(T)+',r='+str(r)+',sigma=='+str(sigma))
        plt.figtext(0.15,0.6,'barrier='+str(barrier))
        plt.figtext(0.4,0.86,'call price='+str(round(c,3)))
        plt.figtext(0.4,0.83,'up_and_out_call'+str(upOutCall)+'(='+str(n_out)+'/'+str(n_simulation)+'*'+str(round(c,3))+')')
plt.title('UpandOut & UpandIn Parity')
plt.xlabel('Total number of steps =' + str(int(n_steps)))
plt.ylabel('stock price')
plt.show()


### 8. 룩백 옵션 ###
#경로의존 옵션
#payoff = sT-min(s)

def lookback_min_price_as_strike(s,T,r,sigma,n_simulation):
    n_steps = 100
    dt = T/n_steps
    total = 0
    for j in range(n_simulation):
        min_price = 100000 #아주큰수
        sT = s
        for i in range(int(n_steps)):
            e = sp.random.normal()
            sT*=sp.exp((r-0.5*sigma*sigma)*dt + sigma*e*sp.sqrt(dt))
            if sT<min_price:
                min_price = sT
                total += bsCall(s,min_price,T,r,sigma)
    return total/n_simulation

s = 40.
T = 0.5
r = 0.05
sigma = 0.2
n_simulation = 1000

lookbackmin = lookback_min_price_as_strike(s,T,r,sigma,n_simulation)
lookbackmin
    
