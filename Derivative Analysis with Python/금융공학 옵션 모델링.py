# -*- coding: utf-8 -*-
"""
Created on Fri Sep  7 00:11:04 2018

@author: Shinhyunjin
"""

#### 금융공학 모델링- 파생상품 ####

from scipy import stats
import scipy as sp
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import p4f
import pylab as pl
from datetime import datetime as dt
import statsmodels.api as sm
import networkx as nx
from math import exp,sqrt
%matplotlib inline


#### 선물 ####
#1 선물 기본가격계산

pv = 100
apr = 0.08
rate = apr /2 # semi annual 환산
n = 2
nper = n*2 #semi annual 환산

fv = sp.fv(rate, nper, 0, pv)
print(fv)

#2 exp와 주기 환산한 이자율 연속복리 구하기

rc = 2 * sp.log(1+0.04)
print(sp.exp(rc/2)-1)

#3 파운드와 미국 달러로 필요한 금액 구하기

amount = 5
r_foreign = 0.02
T = 3./12.
exchangeRateToday = 1.25 #환율
poundToday = 5* sp.exp(-r_foreign*T)
print("Pound needed today = ", poundToday)
usToday = exchangeRateToday * poundToday
print("US dollar needed today=", usToday)

#4 선물 가격 구하는 프로그램 최종 #

def futuresExchangeRate(s0, rateDomestic, rateForeign,T):
    futureEX = s0 * sp.exp((rateDomestic-rateForeign)*T)
    return futureEX

s0 = 1.25
rHome = 0.01
rForeign= 0.02
T=3./12.

futures = futuresExchangeRate(s0, rHome, rForeign, T)
print("futures : ", futures)

#5 선물 차익거래 구하는 프로그램

obligationForeign = 1.0 # 기간 뒤 지불
f = 1.26 # 선물 시장가
s0 = 1.25 #금일 환율
rHome=0.01
rForeign = 0.02
T = 3./12.

todayObligationForeign = obligationForeign*sp.exp(-rForeign*T)
usBorrow = todayObligationForeign*s0
costDollarBorrow = usBorrow*sp.exp(rHome*T)
profit = f * obligationForeign - costDollarBorrow
print("profit in USD : " , profit)

#6 선물 포지션 프로그램

todaySP500index = 2297.42
sp500indexToday = 2297.42
valuePortfolio = 50e6 #5천만
betaPortfolio = 1.1
betaTarget =  0 
priceEachPoint = 250 #s&p 포인트당 가격

contractFuturesSP500 = todaySP500index * priceEachPoint
n = (betaTarget - betaPortfolio)*valuePortfolio/contractFuturesSP500
print("number of contracts SP500 futures :", n) #결과가 음이면 매도, 양이면 매수

#7 선물 헷지 프로그램

sp500indexNmonthsLater = 2200 # 미래 sp예측
mySign = sp.sign(n)
n2 = mySign*sp.ceil(abs(n)) 
print("number of contracts :", n2) #포트폴리오 보험을 위한 선물 수

#헷지결과
v1 = sp500indexToday
v2 = sp500indexNmonthsLater

lossFromPortfolio = valuePortfolio * (v2-v1)/v1
gainFromFutures = n2* (v2-v1)*priceEachPoint # 포트폴리오 보험을 위한 선물 수 헷지(음이라 매도)
net = gainFromFutures + lossFromPortfolio

print("loss from portfolio :", lossFromPortfolio)
print("gain from futures contract :", gainFromFutures)
print("net :", net)



#### 옵션 ####

### 잔존만기 계산 ###

def time_to_maturity(t0, T, y=252):
    t0 = pd.to_datetime(t0)
    T = pd.to_datetime(T) #datetime()은 날짜로 형식바꿈
    return (np.busday_count(t0,T)/y) #평일 카운트 np.busday

time_to_maturity('2018-08-01','2018-12-14')

### 블랙숄즈머튼 모형 ###

def BS_call_price(S,K,ttm,r,sigma):
    d1 = (np.log(S/K)+(r+sigma**2*0.5)*ttm)/(sigma*np.sqrt(ttm))
    d2 = (np.log(S/K)+(r-sigma**2*0.5)*ttm)/(sigma*np.sqrt(ttm))
    val=(S* stats.norm.cdf(d1,0.0,1.0))-K*np.exp(-r*ttm)*stats.norm.cdf(d2,0.0,1.0) #누적밀도함수
    return val

ttm = time_to_maturity('2018-09-01','2018-12-14') #잔존만기설정
r = 0.025 #이자율설정
sigma = 0.15 #변동성 설정

#구현
BS_call_price(350,350,ttm,r,sigma)

### 기초자산 및 행사가격 변화 반영하자 ###

call_space= dict()
S = np.arange(200,400,10) #200~400, 10간격 기초자산 구간 리스트
K = np.arange(250,350,10) #250~350, 10간격 행사가격 구간 리스트

for k in K: #K가 250~350동안 순환하며 밑에 행을 반복 실행하도록 설정함.
    single_space=dict() #각 행사가격에서 사용할 옵션가격 구간을 저장할 딕셔너리
    for s in S: #행사가가 k일때 S가 200~400동안 순환하도록 설정
        single_space[s] = BS_call_price(s,k,ttm,r,sigma) #옵션가격 최종 구한걸 singlespace dict에 저장
    call_space[k] = pd.Series(single_space) #구한 옵션가격을 팬더스 시리즈형태 k열로 저장
call_space

#가독성 높이기위한 데이터 프레임 #
df_BS_call_space = pd.DataFrame(call_space)
df_BS_call_space 

#### 콜옵션 가격 그래프 그리기 ####

#그래프 한글 사용세팅
plt.rcParams['font.family'] = 'Malgun Gothic'
plt.rcParams['font.size'] = 12
plt.rcParams['axes.unicode_minus'] = False

fig = plt.figure() #그래프틀
ax = fig.add_subplot(1,1,1) #서브플롯 ax할당
ax.plot(df_BS_call_space[290], label = 'k=290') #행사가 290
ax.plot(df_BS_call_space[300], label = 'k=300') 
ax.plot(df_BS_call_space[310], label = 'k=310')
ax.set_xlabel('Underlying Asset')
ax.set_ylabel('Call price')
ax.set_title('행사가별 옵션가격')
ax.legend(loc='best')

### 콜옵션가격 곡면 ###
from mpl_toolkits.mplot3d import Axes3D
from matplotlib import cm

S = np.arange(200,400,10)
K = np.arange(250,350,5)

K, S = np.meshgrid(K,S) #매트릭스 만들기
BS_call_price(S,K, ttm, r, sigma)
Z = BS_call_price(S,K,ttm,r,sigma)

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

surf = ax.plot_surface(S,K,Z, cmap=cm.summer,
                       linewidth=1, antialiased=True, alpha=0.8)

ax.set_xlabel('Underlying asset')
ax.set_ylabel('Strike')
ax.set_zlabel('Call price')
ax.set_title('콜옵션 가격 곡면')

fig.colorbar(surf, shrink =0.5, aspect= 5)

### 풋옵션 곡면 그리기 ###

def put_price(S,K,ttm,r,sigma):
    d1 = (np.log(S/K) + (r + sigma**2 * 0.5)*ttm)/(sigma*np.sqrt(ttm))
    d2 = (np.log(S/K)+ (r-sigma**2*0.5)*ttm)/(sigma*np.sqrt(ttm))
    
    val = K * np.exp(-r*ttm) * stats.norm.cdf(-d2, 0.0, 1.0) - (S*stats.norm.cdf(-d1,0.0,1.0))
    return val

S = np.arange(200,400,10)
K = np.arange(250,350,5)

K, S = np.meshgrid(K,S) #매트릭스 만들기
put_price(S,K, ttm, r, sigma)
Z = put_price(S,K,ttm,r,sigma)

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

surf = ax.plot_surface(S,K,Z, cmap=cm.winter,
                       linewidth=1, antialiased=True, alpha=0.8)

ax.set_xlabel('Underlying asset')
ax.set_ylabel('Strike')
ax.set_zlabel('Put price')
ax.set_title('풋옵션 가격 곡면')

fig.colorbar(surf, shrink =0.5, aspect= 5)

#### 옵션 그릭스 시리즈 ####

# 델타 : 기초자산 변화에 따른 콜옵션 가격 변화율 #

def call_delta(S,K,ttm,r,sigma):
    d1 = (np.log(S/K)+(r+sigma**2*0.5)*ttm) / (sigma * np.sqrt(ttm))
    
    val = stats.norm.cdf(d1,0.0,1.0)
    return val


S = np.arange(200,400,10)
K = np.arange(250,350,5)

K, S = np.meshgrid(K,S) #매트릭스 만들기Z = call_delta(S,K,0.38,0.02,0.3) # 잔존만기 이자율 변동성 고정시킴

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

surf = ax.plot_surface(S,K,Z, cmap=cm.coolwarm,
                       linewidth=1, antialiased=True, alpha=0.8)

ax.set_xlabel('Underlying asset')
ax.set_ylabel('Strike')
ax.set_zlabel('Delta')
ax.set_title('델타 곡면')

fig.colorbar(surf, shrink =0.5, aspect= 5)

# 감마 : 기초자산 변화에 따른 델타의 변화율 #

def ndx(x):
    return (np.exp(-1*x**2*0.5)/np.sqrt(2*np.pi))

def gamma(S,K,ttm,r,sigma):
    d1 = (np.log(S/K)+(r+sigma**2*0.5)*ttm)/(sigma*np.sqrt(ttm))
    
    val = (ndx(d1))/(S*sigma*np.sqrt(ttm))
    return val


S = np.arange(200,400,10)
K = np.arange(250,350,5)

K, S = np.meshgrid(K,S) #매트릭스 만들기

Z = gamma(S,K,0.38,0.02,0.3)

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

surf = ax.plot_surface(S,K,Z, cmap=cm.summer,
                       linewidth=1, antialiased=True, alpha=0.8)

ax.set_xlabel('Underlying asset')
ax.set_ylabel('Strike')
ax.set_zlabel('Gamma')
ax.set_title('감마 곡면')

fig.colorbar(surf, shrink =0.5, aspect= 5)

# 세타 : 시간가치의 크기 #ㄸ

def call_theta(S,K,ttm,r,sigma):
    d1 = (np.log(S/K)+(r+sigma**2*0.5)*ttm) / (sigma*np.sqrt(ttm))
    d2 = (np.log(S/K)+(r-sigma**2*0.5)*ttm) / (sigma*np.sqrt(ttm))
    
    val = -1*((S*ndx(d1)*sigma)/(2*np.sqrt(ttm))) - r*K*np.exp(-r*ttm)*stats.norm.cdf(d2,0.0,1.0)
    return val


S = np.arange(350,355)
T = np.arange(1.0,0.0,-0.01)

T, S = np.meshgrid(T,S) #매트릭스 만들기

Z = call_theta(S,350,T,0.02,0.3)

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

surf = ax.plot_surface(T,S,Z, cmap=cm.summer,
                       linewidth=1, antialiased=True, alpha=0.8)

ax.set_xlabel('Time to Maturity')
ax.set_ylabel('Underlying Asset')
ax.set_zlabel('Theta')
ax.invert_xaxis() #x충글 1부터 0의 순서를 거꾸로
ax.set_title('세타 곡면')

fig.colorbar(surf, shrink =0.5, aspect= 5)

# 베가 : 변동성에 따른 옵션가격의 변화율 #

def vega(S,K,ttm,r,sigma):
    d1 = (np.log(S/K)+(r+sigma**2*0.5)*ttm) / (sigma*np.sqrt(ttm))
    d2 = (np.log(S/K)+(r-sigma**2*0.5)*ttm) / (sigma*np.sqrt(ttm))
    
    val = (S*np.sqrt(ttm)*ndx(d1))
    return val


V = np.arange(0.05,0.95,0.01)
S = np.arange(350,352)

V, S = np.meshgrid(V,S) #매트릭스 만들기

Z = vega(S,350,0.38,0.02,V)

fig = plt.figure()
ax = fig.add_subplot(111,projection = '3d')

surf = ax.plot_surface(V,S,Z, cmap=cm.spring,
                       linewidth=1, antialiased=True, alpha=0.8)

ax.set_xlabel('Volatility')
ax.set_ylabel('Underlying Asset')
ax.set_zlabel('Vega')
ax.set_title('베가 곡면')

fig.colorbar(surf, shrink =0.5, aspect= 5)




## Call option Position Profit
#long max(s-x, 0 ) -c
#short c - max(s-x, 0)

s = sp.arange(30,70,5)
x = 45 ; c=2.5
y = (abs(s-x)+s-x)/2 - c
y2 = sp.zeros(len(s))

plt.ylim(-30,50)
plt.plot(s,y)
plt.plot(s, y2, '-.')
plt.plot(s,-y)
plt.title("Profit / Loss Function")
plt.xlabel('Stock Price')
plt.ylabel('Profit (loss)')
plt.annotate('Call option buyer', xy = (55,15), xytext = (35,20), arrowprops=dict(facecolor = 'blue', shrink=0.01),)
plt.annotate('Call option seller', xy = (55,-10), xytext = (40,-20), arrowprops=dict(facecolor = 'red', shrink=0.01),)
plt.show()

## Put option Position Profit
# long max(x-s,0)-p
# short p - max(x-s,0)

s = sp.arange(30,70,5)
x = 45 ; p=2 ; c=2.5
y = c-(abs(x-s)+x-s)/2
y2 = sp.zeros(len(s))
x3 = [x,x]
y3 = [-30,10]

plt.ylim(-30,50)
plt.plot(s,y)
plt.plot(s,y2, '-.')
plt.plot(s,-y)
plt.plot(x3,y3)
plt.title("Profit/Loss function for a put option")
plt.xlabel('stock price')
plt.ylabel('profit (loss)')
plt.annotate('put option buyer', xy=(35,12), xytext = (35,45), arrowprops = dict(facecolor = 'red', shrink=0.01),)
plt.annotate('put option seller', xy=(35,-10), xytext = (35,-25), arrowprops = dict(facecolor = 'blue', shrink=0.01),)
plt.annotate('Exercise price', xy=(45,-30), xytext = (50,-20), arrowprops = dict(facecolor = 'black', shrink=0.01),)
plt.show()



### 다양한 투자 전략에 따른 프로그래밍 ###

## 1 Covered Call : Long share, Short Call ##

sT= np.arange(0,40,5)
k= 15 ; s0=10 ; c=2

y0 = np.zeros(len(sT))
y1 = sT-s0
y2 = (abs(sT-k)+sT-k)/2 - c
y3 = y1-y2
plt.ylim(-10, 30)
plt.plot(sT,y1)
plt.plot(sT,y2)
plt.plot(sT,y3,'red')
plt.plot(sT,y0,'b-.')
plt.plot([k,k],[-10,10],'black')
plt.title('Covered call')
plt.xlabel('Stock price')
plt.ylabel('Profit (loss)')
plt.annotate('Stock only(long a share)',xy=(24,15), xytext=(15,20), arrowprops=dict(facecolor = 'blue', shrink = 0.01),)
plt.annotate('Covered call',xy=(10,4), xytext=(9,25), arrowprops=dict(facecolor = 'red', shrink = 0.01),)
plt.annotate('Exercise price=' + str(k), xy=(k+0.2, -10+0.5))
plt.show()

## 2. Straddle : Long call and put with same K ##

sT = np.arange(30,80,5)
x=50;c=2;p=1
straddle = (abs(sT-x)+sT-x)/2 - c + (abs(x-sT)+x-sT)/2 - p
y0 = np.zeros(len(sT))
plt.ylim(-6,20)
plt.xlim(40,70)
plt.plot(sT, y0)
plt.plot(sT, straddle, 'r')
plt.plot([x,x], [-6,4], 'g-.')
plt.title("profit loss for a straddle")
plt.xlabel('stock price')
plt.ylabel('profit (loss)')
plt.annotate('Point 1 = '+str(x-c-p),xy=(x-p-c,0), xytext=(x-p-c,10), arrowprops=dict(facecolor = 'red', shrink = 0.01),)
plt.annotate('Point 2 = '+str(x+c+p),xy=(x+p+c,0), xytext=(x+p+c,13), arrowprops=dict(facecolor = 'blue', shrink = 0.01),)
plt.annotate('exercise price', xy=(x+1, -5))
plt.annotate('Buy a call and a put with same K', xy=(45,16))
plt.show()

## 3. Butterfly Call : long c3, short c2 *2 , long c1 ##

sT = np.arange(30,80,5)
x1 = 50; c1 = 10
x2 = 55 ; c2 = 7
x3 = 60 ; c3 = 5
y1 = (abs(sT-x1)+sT-x1)/2 - c1
y2 = (abs(sT-x2)+sT-x2)/2 - c2
y3 = (abs(sT-x3)+sT-x3)/2 - c3
butter_fly=y1+y3-2*y2
y0 = np.zeros(len(sT))

plt.ylim(-20,20)
plt.xlim(40,70)
plt.plot(sT,y0)
plt.plot(sT,y1)
plt.plot(sT,-y2, '-.')
plt.plot(sT,y3)
plt.plot(sT, butter_fly, 'r')
plt.title("Profit loss for a butterfly")
plt.xlabel('Stock price')
plt.ylabel('Profit(loss)')
plt.annotate('Butterfly', xy=(53,3), xytext=(42,4), arrowprops=dict(facecolor = 'red', shrink = 0.01),)
plt.annotate('Buy 2 calls with x1, x3 and sell 2 calls with x2', xy = (45,16))
plt.annotate('x2=(x1+x3)/2', xy=(45,14))
plt.annotate('x1 = 50, x2 =55, x3=60', xy=(45,12))
plt.annotate('c1=10, c2= 7, c3=5', xy=(45,10))
plt.show()

## 입력값과 옵션값의 관계 -> 변동성과 c, p 가격 차이 ##

#변동성이 커지면 옵션값은 올라간다#
#만기가 증가하면 K가 낮아서 콜증가, 풋감소 + 변동성 증가의 가능성을 증가시켜 옵션가격 증가시킨다#
s0 = 30
T0 = 0.5
sigma0 = 0.2
r0= 0.05
x0 = 30
sigma = np.arange(0.05, 0.8, 0.05)
T = np.arange(0.5 , 2.0 , 0.5)
call_0 = p4f.bs_call(s0,x0,T0,r0, sigma0)
call_sigma = p4f.bs_call(s0,x0,T0,r0,sigma)
call_T = p4f.bs_call(s0,x0,T,r0, sigma0)

plt.title("Relationship between sigma and call, T, and call")
plt.plot(sigma, call_sigma,'b')
plt.plot(T, call_T, 'r')
plt.annotate('x=Sigma, y=call price', xy=(0.6,5), xytext=(1,6), arrowprops=dict(facecolor = 'blue', shrink = 0.01),)
plt.annotate('x=T(maturity), y=call price',xy =(1,3), xytext=(0.8,1), arrowprops=dict(facecolor = 'red', shrink = 0.01),)
plt.ylabel("Call premium")
plt.xlabel("Volatiltiy or T")
plt.show()

#### 풋콜패러티 ####

x = 10
sT=np.arange(0,30,5)
payoff_call = (abs(sT-x)+sT-x)/2
payoff_put = (abs(x-sT)+x-sT)/2
cash = np.zeros(len(sT))+x

def graph(text, text2= ''):
    pl.xticks(())
    pl.yticks(())
    pl.xlim(0,30)
    pl.ylim(0,20)
    pl.plot([x,x], [0,3])
    pl.text(x,-2,"X");
    pl.text(0,x,"X")
    pl.text(x, x*1.7, text, ha = 'center', va='center', size = 10, alpha= .5)
    pl.text(-5,10, text2, size=25)
pl.figure(figsize=(6,4))
pl.subplot(2,3,1);graph('Payoff_call');
pl.plot(sT, payoff_call)
pl.subplot(2,3,2); graph('cash', '+');plt.plot(sT,cash)
pl.subplot(2,3,3); graph('Portfolio A','=');
pl.plot(sT, cash + payoff_call)
pl.subplot(2,3,4); graph('payoff of put'); pl.plot(sT,payoff_put)
pl.subplot(2,3,5); graph ('Stock', '+'); pl.plot(sT, sT)
pl.subplot(2,3,6); graph('Portfolio B', '='); pl.plot(sT, sT+payoff_put)
pl.show()

# 트렌드 #

data = pd.read_csv('c:/temp/totalpc.csv', skiprows=2, index_col = 0, parse_dates=True)
data.columns= ('Calls', 'Puts','Total','Ratio')
begdate= datetime(2013,6,1)
enddate = datetime(2013,12,31)
data2 = data[(data.index>=begdate) & (data.index<=enddate)]
x = data2.index
y = data2.Ratio
x2 =range(len(x))
x3 = sm.add_constant(x2)
model = sm.OLS(y,x3)
results = model.fit()
alpha = round(results.params[0], 3)
slope = round(results.params[1], 3)
y3 = alpha + sp.dot(slope,x2)
y2 = sp.ones(len(y))
plt.title('Put-call ratio')
plt.xlabel('Date')
plt.ylabel('Put-call ratio')
plt.ylim(0, 1.5)
plt.plot(x,y,'b-')
plt.plot(x,y2,'r-.')
plt.plot(x,y3, 'y+')
plt.figtext(0.3,0.35, 'Trend : intercept = ' + str(alpha) +', slope = ' + str(slope))
plt.show()

### 이항모형 ###

plt.figtext(0.08,0.6, "stock pirce = 10")
plt.figtext(0.75,0.91, "stock price= 11.5")
plt.figtext(0.75,0.87,"call payoff = 0.5")
plt.figtext(0.75,0.28,"stock price = 8")
plt.figtext(0.75,0.24, "call payoff = 0")
n=1

def binomial_grid(n):
    G=nx.Graph()
    for i in range(0, n+1):
        for j in range(1, i+2):
            if i<n:
                G.add_edge((i,j),(i+1,j))
                G.add_edge((i,j), (i+1, j+1))
    posG={}
    for node in G.nodes():
        posG[node]=(node[0], n+2+node[0]-2*node[1])
    nx.draw(G,pos=posG)
binomial_grid(n)
plt.show()

## 2단계 이항모형

s=10;x=10;r=0.05;sigma=0.2;T=3./12.;n=2;q=0
deltaT = T/n
u = sp.exp(sigma*sp.sqrt(deltaT))
d=1/u
a = sp.exp((r-q)*deltaT)
p=(a-d)/(u-d) #위험중립확률
s_dollar= 'S=$'
c_dollar= ' c=$'
p2 = round(p,2)
plt.figtext(0.15,0.91, 'Note: x='+str(x)+', r='+str(r)+', deltaT='+str(deltaT)+', p='+str(p2))
plt.figtext(0.35,0.61,'p')
plt.figtext(0.65,0.76,'p')
plt.figtext(0.65,0.43,'p')
plt.figtext(0.35,0.36,'1-p')
plt.figtext(0.65,0.53,'1-p')
plt.figtext(0.65,0.21,'1-p')

#레벨2
su=round(s*u,2);suu=round(s*u*u,)
sd=round(s*d,2);sdd=round(s*d*d,)
sud= s
c_suu = round(max(suu-x,0),2)
c_s = round(max(s-x,0),2)
c_sdd=round(max(sdd-x,0),2)
plt.figtext(0.8, 0.94, 's*u*u')
plt.figtext(0.8, 0.91, s_dollar+str(suu))
plt.figtext(0.8, 0.87, c_dollar+str(c_suu))
plt.figtext(0.8, 0.6, s_dollar+str(sud))
plt.figtext(0.8,0.64,'s*u*d=s')
plt.figtext(0.8, 0.57, c_dollar+str(c_s))
plt.figtext(0.8, 0.32, 's*d*d')
plt.figtext(0.8, 0.28, s_dollar+str(sdd))
plt.figtext(0.8, 0.24, c_dollar+str(c_sdd))

#레벨 1
c_01 = round((p*c_suu + (1-p)*c_s)*sp.exp(-r*deltaT),2)
c_02 = round((p*c_s + (1-p)*c_sdd)*sp.exp(-r*deltaT),2)

plt.figtext(0.43, 0.78, 's*u')
plt.figtext(0.43, 0.74, s_dollar+str(su))
plt.figtext(0.43, 0.71, c_dollar+str(c_01))

plt.figtext(0.43, 0.32, 's*d')
plt.figtext(0.43, 0.27, s_dollar+str(sd))
plt.figtext(0.43, 0.23, c_dollar+str(c_02))

#레벨 0 
c_00 = round(p*sp.exp(-r*deltaT)*c_01 + (1-p)*sp.exp(-r*deltaT)*c_02,2)
plt.figtext(0.09, 0.6, s_dollar+str(s))
plt.figtext(0.09, 0.56, c_dollar+str(c_00))
p4f.binomial_grid(n)

p
deltaT
v = (p*1.52 + (1-p)*0)*np.exp(-r*deltaT)
round(v,2)

# 유럽형 이항모형
def binomialCallEuropean(s,x,T,r,sigma,n=100):
    deltaT = T/n
    u = exp(sigma*sqrt(deltaT))
    d = 1.0/u
    a = exp(r*deltaT)
    p = (a-d)/(u-d)
    v = [[0.0 for j in range(i+1)] for i in range(n+1)]
    for j in range(n+1):
        v[n][j] = max(s*u**j * d ** (n-j) - x, 0.0)
    for i in range(n-1, -1,-1):
        for j in range(i+1):
            v[i][j] = exp(-r*deltaT)*(p*v[i+1][j+1]+(1.0-p)*v[i+1][j])
    return v[0][0]

print(binomialCallEuropean(40,42,0.5,0.1,0.2,1000), p4f.bs_call(40,42,0.5,0.1,0.2))

# 미국형 이항모형


def binomialCallAmerican(s,x,T,r,sigma,n=100):
    deltaT = T/n
    u = exp(sigma*sqrt(deltaT))
    d = 1.0/u
    a = exp(r*deltaT)
    p = (a-d)/(u-d)
    v = [[0.0 for j in range(i+1)] for i in range(n+1)]
    for j in range(n+1):
        v[n][j] = max(s*u**j * d ** (n-j) - x, 0.0)
    for i in range(n-1, -1,-1):
        for j in range(i+1):
            v1 = exp(-r*deltaT)*(p*v[i+1][j+1]+(1.0-p)*v[i+1][j])
            v2 = max(v[i][j] - x, 0) #조기 권리 행사
            v[i][j] = max(v1,v2)
    return v[0][0]

print(binomialCallEuropean(52,50,0.5,0.1,0.2,1000), binomialCallAmerican(52,50,0.5,0.1,0.2,1000))

#### 내재적 변동성 ####

# 콜
def implied_vol_call(S,X,T,r,c):
    for i in range(200):
        sigma = 0.005*(i+1)
        d1 = (np.log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*np.sqrt(T))
        d2 = d1-sigma*np.sqrt(T)
        diff=c- (S*stats.norm.cdf(d1) - X*exp(-r*T)*stats.norm.cdf(d2))
        if abs(diff)<=0.01:
            return i, sigma, diff
        
implied_vol_call(40,40,0.5,0.05,3.3)

# 풋

def implied_vol_put_min(S,X,T,r,p):
    implied_vol = 1.0
    min_value = 100.0
    for i in range(1,10000):
        sigma = 0.0001*(i+1)
        d1 = (np.log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*np.sqrt(T))
        d2 = d1-sigma*np.sqrt(T)
        put = X*exp(-r*T)*stats.norm.cdf(-d2)-S*stats.norm.cdf(-d1)
        abs_diff = abs(put-p)
        if abs_diff < min_value:
            min_value= abs_diff
            implied_vol = sigma
            k=i
        put_out = put
    print('k, implied_vol, put, abs_diff')
    return k, implied_vol, put_out, min_value
        
implied_vol_put_min(40,40,1.0,0.1,1.501)

## 이진 탐색을 통한 내재 변동성추정 ##

def binary_search(x, target, my_min=1, my_max=None):
    if my_max is None:
        my_max = len(x) - 1
    while my_min <= my_max:
        mid = (my_min + my_max)//2
        midval = x[mid]
        if midval < target:
            my_min = my_mid +1
        elif midval > target:
            my_max = mid-1
        else:
            return mid
    raise ValueError

# 이진 탐색 응용 #
    
S=42;X=40;T=0.5;r=0.01;c=3.0

def bsCall(S,X,T,r,sigma):
    d1 = (np.log(S/X)+(r+sigma*sigma/2.)*T)/(sigma*sqrt(T))
    d2 = d1 - sigma*np.sqrt(T)
    return S*stats.norm.cdf(d1)-X*exp(-r*T)*stats.norm.cdf(d2)
#
def impliedVolBinary(S,X,T,r,c):
    k = 1
    volLow = 0.001
    volHigh = 1.0
    cLow = bsCall(S,X,T,r,volLow)
    cHigh = bsCall(S,X,T,r,volHigh)
    if cLow > c or cHigh<c:
        raise ValueError
    while k == 1 :
        cLow = bsCall(S,X,T,r,volLow)
        cHigh = bsCall(S,X,T,r,volHigh)
        volMid = (volLow+volHigh)/2.0
        cMid = bsCall(S,X,T,r,volMid)
        if abs(cHigh-cLow)<0.01:
            k=2
        elif cMid>c:
            volHigh=volMid
        else:
            volLow = volMid
    return volMid, cLow, cHigh
print("Vol, cLow, cHigh")
print(impliedVolBinary(S,X,T,r,c))

### Volaitility Smile ###
