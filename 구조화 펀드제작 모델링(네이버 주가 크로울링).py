# -*- coding: utf-8 -*-
"""
Created on Wed Sep 19 13:32:52 2018

@author: Shinhyunjin
"""

exit()

### 사전 준비 ###

import datetime as dt
from urllib.request import urlopen
import bs4
import re
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
%matplotlib inline
import numpy as np

## 20180919 기준 시총  상위 10개 기업 ##
'''
005930 삼성전자
000660 SK하이닉스
068270 셀트리온
005380 현대자동차
005490 POSCO0059
051910 LG화학
035420 NAVER
028260 삼성물산
012330 현대모비스
017670 SK텔레콤
'''
k10_component = ['005930', '000660', '068270', '017670','005380','005490','051910','035420','028260','012330']

### 구성종목 기본정보 클로울링 ###
def stock_info(stock_cd):
    url_float = 'http://companyinfo.stock.naver.com/v1/company/c1010001.aspx?cmp_cd=' + stock_cd
    
    source = urlopen(url_float).read()
    soup = bs4.BeautifulSoup(source, 'lxml')
    ### 주식수와 유통비율 크로울링
    tmp = soup.find(id='cTB11').find_all('tr')[6].td.text # X-path주소
    tmp = tmp.replace('\r','') #X-path주소 클리닝
    tmp = tmp.replace('\n', '') #상동
    tmp = tmp.replace('\t','') #상동
    tmp = re.split('/', tmp) #문자열분리
    ### 상장주식 수 크로울링
    outstanding = tmp[0].replace(',','') #tmp[0]는 상장주식수
    outstanding = outstanding.replace('주','') #상장주식수 클리닝
    outstanding = outstanding.replace(' ', '') #상동
    outstanding = int(outstanding) # 데이터를 정수형으로 변환
    
    floating = tmp[1].replace(' ','')     # 유통비율 데이터클리닝
    floating = floating.replace('%','')
    floating = float(floating) # 데이터를 소수형으로 변환
    ###종목명 크로울링
    name = soup.find(id='pArea').find('div').find('div').find('tr').find('td').find('span').text
    
    k10_outstanding[stock_cd] = outstanding # 발행주식수 딕셔너리저장
    k10_floating[stock_cd] = floating #유동비율 딕셔너리 저장
    k10_name[stock_cd] = name # 종목명 딕셔너리에 저장
    
# 딕셔너리저장
k10_outstanding = dict()
k10_floating = dict()
k10_name = dict()
#
for stock_cd in k10_component:
    stock_info(stock_cd)
    
## 과거시세 반영 데이터프레임워크

tmp = {'Outstanding':k10_outstanding, 'Floating':k10_floating, 'Name':k10_name}
k10_info = pd.DataFrame(tmp) # 딕셔너리를 데이터프레임화
k10_info

### 일자별 주가함수 크로울링 ###

## 날짜 포맷 클리닝 ##
def date_format(d):
    
    d = str(d)
    d = d.replace('/','-')
    d = d.replace('.','-')
    
    yyyy = int(d.split('-')[0]) #포맷 첫번째
    if yyyy < 50:
        yyyy =yyyy + 2000
    elif yyyy >= 50 and yyyy <100:
        yyyy = yyyy + 1900
    
    mm = int(d.split('-')[1]) #포맷 두번째
    dd = int(d.split('-')[2]) #포맷 세번째
    
    return dt.date(yyyy,mm,dd)
    
### 일자별 주가를 수집해오는 함수 ###

def historical_stock_naver(stock_cd, start_date='', end_date='', page_n=1, last_page=0):
    if start_date: 
        start_date = date_format(start_date) # start_date가 있다면 date포맷으로 변환
    else:
        start_date = dt.date.today() #없으면 오늘 날짜
    if end_date:
        end_date = date_format(end_date) #end_date가 있다면 date포맷으로 변환
    else:
        end_date=dt.date.today() #없으면 오늘날짜
        
    naver_stock = 'https://finance.naver.com/item/sise_day.nhn?code='+ stock_cd + '&page=' + str(page_n)
    source = urlopen(naver_stock).read() ## 지정한 페이지에서 코드읽기
    source = bs4.BeautifulSoup(source, 'lxml') ## 뷰티플수프로 태그별로 코드 분류
    dates=source.find_all('span',class_='tah p10 gray03') ##<td class="date">태그에서 날짜 수집
    prices = source.find_all('td', class_='num') ## <td class="number_1"> 태그에서 지수수집
    #날짜처리
    for n in range(len(dates)):
        if len(dates) >0 :
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)
            #종가처리
            if this_date <=end_date and this_date>=start_date: ## 사이 저장                
                this_close = prices[n*6].text ## 종가처리, 위에서 언급한 4배수
                this_close = this_close.replace(',','') #쉼표제거
                this_close = float(this_close)  
                #딕셔너리에 저장
                historical_prices[this_date] = this_close
            
            elif this_date<start_date:
                return historical_prices ##start_date이전이면 함수종료
            #페이지 내비게이션
    if last_page==0:
        last_page = source.find_all('table')[1].find('td',class_='pgRR').find('a')['href']
        last_page = last_page.split('&')[1] # &뒤의 page수 부분 추출
        last_page = last_page.split('=')[1] # =뒤의 페이지번호만 추출
        last_page = float(last_page) # 숫자형 변수로 변환
        
    if page_n < last_page:
        page_n = page_n + 1
        historical_stock_naver(stock_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

#포트폴리오 초기화
k10_historical_prices = dict()

#포트폴리오적용
for stock_cd in k10_component:
    historical_prices = dict()
    start_date = '2017-1-1' #시작날짜
    end_date = '2018-9-19' #끝날짜
    historical_prices=  historical_stock_naver(stock_cd, start_date, end_date)
    k10_historical_prices[stock_cd] = historical_prices
    
#데이터 프레임 변환하여 저장
k10_historical_price = pd.DataFrame(k10_historical_prices)

#보간 실행하여 비어있는 데이터 없애기
k10_historcial_price = k10_historical_price.fillna(method='ffill')#ffill로 구멍채우고
if k10_historical_price.isnull().values.any():
    k10_historical_price = k10_historical_price.fillna(method='bfill') #그래도 구멍있으면 bfill로 채워라
k10_historical_price.head(3) #헤드몇개만

# 삼성전자 액면분할
k10_historical_price.loc[k10_historical_price.index < dt.date(2018, 5, 4), '005930'] /= 50


## K10 지수 산출 ##
k10_historical_mc = k10_historical_price * k10_info['Outstanding']*k10_info['Floating']*0.01
k10_historical_mc.tail(3) #뒤에 3개

# 시가총액
k10 = pd.DataFrame()
k10['Market Cap'] = k10_historical_mc.sum(axis=1)
k10.head(3) #앞에 몇개만

#기준일
base_date = dt.date(2017, 1, 2) 
#인덱스계산
k10['Index']  = k10['Market Cap']/k10['Market Cap'][base_date] * 100
k10.head(5)


### K10지수와 KOSPI200 그래프그리기 ###
#한글사용세팅
plt.rcParams['font.family'] = 'Malgun Gothic'
plt.rcParams['font.size'] = 12
plt.rcParams['axes.unicode_minus'] = False
#그래프사이즈와 그리드모양
plt.rcParams['figure.figsize'] = (10,5)
plt.rcParams['grid.linestyle'] = '--'
plt.rcParams['grid.alpha'] =0.7
plt.rcParams['lines.antialiased'] = True
#K10지수그래프
plt.plot(k10['Index'], color = 'orange', label='K10')
plt.legend(loc=0)
plt.grid(True)


#### KOSPI 200 지수 크로울링 ####

def historical_index_naver(index_cd, start_date='', end_date='', page_n=1, last_page=0):
    index_cd = index_cd
    page_n = page_n

    if start_date: 
        start_date = date_format(start_date) # start_date가 있다면 date포맷으로 변환
    else:
        start_date = dt.date.today() #없으면 오늘 날짜
    if not end_date:
        end_date = dt.date.today()
    else:
        end_date= date_format(end_date)
        
    naver_index = 'https://finance.naver.com/sise/sise_index_day.nhn?code='+ index_cd + '&page=' + str(page_n)
    source = urlopen(naver_index).read() ## 지정한 페이지에서 코드읽기
    source = bs4.BeautifulSoup(source, 'lxml') ## 뷰티플수프로 태그별로 코드 분류
    
    dates=source.find_all('td',class_='date') #날짜 수집
    prices = source.find_all('td', class_='number_1') ##지수수집
    
    for n in range(len(dates)):
        #날짜처리
        if dates[n].text.split('.')[0].isdigit():
            this_date = dates[n].text
            this_date = date_format(this_date)

            if this_date <=end_date and this_date>=start_date: ## 사이 저장        
                #종가처리
                this_close = prices[n*4].text ##4배수
                this_close = this_close.replace(',','')
                this_close = float(this_close)            
                historical_prices[this_date] = this_close ## 딕셔너리 저장
            
            elif this_date < start_date:
                return historical_prices ##start_date이전이면 함수종료
         ##페이지네비게이션   
    if last_page==0:
        last_page = source.find('td',class_='pgRR').find('a')['href']
        last_page = last_page.split('&')[1] # &뒤의 page=506부분 추출
        last_page = last_page.split('=')[1] # =뒤의 페이지번호만 추출
        last_page = int(last_page) # 숫자형 변수로 변환
        
    if page_n < last_page:
        page_n = page_n + 1
        historical_index_naver(index_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

#KOSPI200 데이터프레임에 담기
index_cd = 'KPI200'
historical_prices = dict()
kospi200 = historical_index_naver(index_cd, '2017-1-1' ,'2018-9-19')
k200 = pd.DataFrame({'Index' : kospi200})
k200.head(3)

### K10지수와 KOSPI200 Fit ###
# K10 -> KOSPI200으로 환산
k10['Adj Index'] = k10['Market Cap']/k10['Market Cap'][base_date] * k200['Index'][base_date]
k10.head(3)
#
plt.plot(k10['Adj Index'], color='orange', label='K10')
plt.plot(k200['Index'], color = 'blue', label = 'KOSPI200')
plt.legend(loc=0)
plt.grid(True)

#### 펀드 설계 및 운용 ####

# 기본정보 세팅
CU = 50000 # Creation unit
base_date = dt.date(2017,1,2) #기준설정일
volume = 1000000 #최초 설정수량
interest_rate = 0.018 #이자율

## 환매 50만주 이상 유지를 위한 제약조건 ##

def creation_redemption(v):
    creation = np.random.randint(0,5) * CU # 0~5 임의 정수 * CU
    if v>500000:
        redemption = np.random.randint(0,5) * CU # 50만좌 이상인 경우에만 환매발생
    else:
        redemption = 0
    volume = v + creation - redemption # 총좌수 = 기존 좌수 + 설정 - 환매
    return(creation, redemption, volume)

# 보유 비중 산정
k10_stock_ratio = pd.DataFrame()
for s in k10_info.index:
    k10_stock_ratio[s] = k10_historical_mc[s] / k10_historical_mc.sum(axis=1)
#
Fund_NAV = pd.DataFrame() #일자별 펀드 순자산가치
Fund_Chg = pd.DataFrame() #순자산가치 변화
#
for d in k10_historical_price.index:
    stock_price = np.array(k10_historical_price.loc[d]) #일자별 주가
    stock_weight = np.array(k10_stock_ratio.loc[d]) #일자별 비중 location[date]

    # 기본주식 포트폴리오 NAV계산
    if (d<=base_date): #기준일 이전에는 최초주식 포트폴리오 보유량 0
        stock_holdings=np.array([0,0,0,0,0,0,0,0,0,0])
        NAV_cash=0 #최초현금보유량
    else: #기준일이후
        NAV_stock = sum(stock_holdings * stock_price) #주식잔고
        NAV = NAV_stock + NAV_cash #전체잔고 누적
    
    # 기준가격 산정
    if (d<=base_date):
        #최초 기준가를 기준일자의 코스피지수와 맞춤
        price = k200['Index'][base_date]*100
    else:
        price= NAV / volume
    
    #신규 펀드 설정 및 환매좌수 계산하기
    if (d==base_date):
        volume = 0 # 기존펀드좌수
        volume_chg = 1000000#첫날설정액
    else:
        vol  = creation_redemption(volume) #설정 및 환매함수 호출
        volume_chg = vol[0] + vol[1] # 변동좌수 = 설정좌수 - 환매좌수

    #총 펀드 좌수에 반영 업데이트
    volume = volume + volume_chg
    #펀드입출금액 =  변동좌수 * 펀드기준가격 
    aum_chg = price*volume_chg
    
    #신규주식 거래량계산기
    stock_trade = np.floor(price*volume_chg*stock_weight / stock_price)
    # 당일  주식 매매금액 저장
    trade_amt = sum(stock_trade*stock_price)
    # 현금 잔고 변동 업데이트
    cash_chg = aum_chg - trade_amt
    #총 주식 보유량 = 기보유량 + 신규거래량
    stock_holdings = stock_holdings + stock_trade
    #현금보유량 증가 이자율반영하기
    cash_holdings = np.floor(NAV_cash * np.exp(interest_rate/365))
    
    #NAV 업데이트
    NAV_stock = sum(stock_holdings * stock_price) # 주식 잔고
    NAV_cash = cash_holdings + cash_chg # 현금잔고
    NAV = NAV_stock + NAV_cash #전체잔고
    
    date=pd.Series(d)
    
    #Fund NAV정보를 데이터프레임에 저장하기
    NAV_tmp = {'Stock':NAV_stock, 'Cash':NAV_cash, 'Total':NAV, 'Price':price}
    tmp = pd.DataFrame(NAV_tmp, index=date)
    Fund_NAV = Fund_NAV.append(tmp)
    
    #일자별 설정 및 환매 좌수정보를 데이터 프레임에 저장
    chg_tmp = {'Amount Change':aum_chg, 'Trade Amount':trade_amt, 'Cash Change':cash_chg}
    tmp=pd.DataFrame(chg_tmp, index=date)
    Fund_Chg = Fund_Chg.append(tmp)

#### 시각화 하기 ####
    
#1. 펀드 vs 지수 수익률

Earnings = pd.DataFrame()
Earnings['K10'] = (k10['Index']-k10['Index'][base_date])/k10['Index'][base_date]*100
Earnings['KOSPI200'] = (k200['Index'] - k200['Index'][base_date])/k200['Index'][base_date]*100
Earnings['Fund'] = (Fund_NAV['Price']-Fund_NAV['Price'][base_date])/Fund_NAV['Price'][base_date]*100
ax = Earnings.plot(color = ['Orange','blue','red'])
ax.legend(loc=0)
ax.set_ylabel('(수익률, %)')
ax.grid(True)

#2. 펀드 기준가 vs 지수
Indicator = pd.DataFrame()
Indicator['K10'] = k10['Adj Index']
Indicator['KOSPI200'] = k200['Index']
Indicator['Fund'] = Fund_NAV['Price']/100
ax = Indicator.plot(color=['Orange','blue','red'])
ax.legend(loc=0)
ax.set_ylabel('(수익룰, %)')
ax.grid(True)

#3. 최종 그래프
fig = plt.figure(figsize=(10,8))
ax1 = fig.add_subplot(2,1,1)
ax2 = fig.add_subplot(2,1,2)
Earnings.plot.line(ax=ax1, color = ['orange','b','r'])
ax1.set_ylabel('(수익률, %)')
Fund_NAV['Display'] = Fund_NAV['Total'] * 0.00000001
Fund_NAV['Display'].plot.area(ax=ax2, stacked=False, sharex=True)
ax2.set_ylabel('(NAV, 억원)')
ax1.grid(True)
ax2.grid(True)

## 일간 수익률 ##

fig = plt.figure()
ax = fig.add_subplot(1,1,1)
Daily= pd.DataFrame()
Daily['K10'] = (k10['Index'] - k10['Index'].shift(1))/k10['Index'].shift(1)*100
Daily['K200'] = (k200['Index'] - k200['Index'].shift(1))/k200['Index'].shift(1)*100
Daily['Fund'] = (Fund_NAV['Price'] - Fund_NAV['Price'].shift(1))/ Fund_NAV['Price'].shift(1)*100
Daily.plot(ax=ax, color = ['orange', 'blue', 'red'], alpha = 0.7)
ax.set_ylabel('(%)')
ax.grid(True)

## 추적 오차율 ## 해석 -1~1이 좋음
#1. KOSPI200 대비 추적 오차율 일간

fig = plt.figure()
ax = fig.add_subplot(1,1,1)
Daily['TE200'] = Daily['Fund'] - Daily['K200'] # 추적오차율 = 펀드수익률 - 지수수익률
Daily['TE200'].plot(ax=ax)
ax.grid(True)

#2. K10 대비 추적 오차율 일간

fig= plt.figure()
ax= fig.add_subplot(1,1,1)
Daily['TE10'] = Daily['Fund']-Daily['K10']
Daily['TE10'].plot(ax=ax)
ax.grid(Ture)
