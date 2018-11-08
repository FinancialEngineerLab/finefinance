# -*- coding: utf-8 -*-
"""
Created on Wed Jul 18 11:22:20 2018

@author: Shinhyunjin
"""


### 현대로템 기준 기업분석 프로그램 ###

import bs4
from urllib.request import urlopen
import re

    
##
def stock_info(stock_cd):
    url_float = 'http://companyinfo.stock.naver.com/company/c1010001.aspx?cmp_cd=' + stock_cd
    
    source = urlopen(url_float).read()
    soup = bs4.BeautifulSoup(source, 'lxml')
  ##  soup.find(id='cTB11').find_all('tr')[6].td.text ## tr 7 - 1
# path 주소 //*[@id="cTB11"]/tbody/tr[7]/td
    tmp = soup.find(id='cTB11').find_all('tr')[6].td.text
    tmp = tmp.replace('\r','')
    tmp = tmp.replace('\n','')
    tmp = tmp.replace('\t','')

    tmp=re.split('/', tmp)
#슬래쉬 기준 분리 ##tmp[0]좌 tmp[1]우

##숫자화
#상장주식수
    outstanding = tmp[0].replace(',','')
    outstanding = outstanding.replace('주', '')
    outstanding = outstanding.replace(' ','')
    outstanding = int(outstanding)
#유동비율
    floating = tmp[1].replace(' ', '')
    floating = floating.replace('%', '')
    floating = float(floating)

## 유동비율 함수구하기
    name = soup.find(id='pArea').find('div').find('div').find('tr').find('td').find('span').text

    k10_outstanding[stock_cd] = outstanding
    k10_floating[stock_cd] = floating
    k10_name[stock_cd] = name
    
###
    
###한국거래소 시가총액 사위 10종목 (2018-07-18기준)
#삼성전자 005930
#SK하이닉스 000660
#셀트리온 068270
#포스코 005490
#현대차 005380
#삼성바이오로직스 207940
#네이버 035420
#LG화학 051910
#삼성물산 028260
#KB금융 105560

k10_component=['005930', '000660', '068270','005490','005380','207940','035420','051910','028260','105560']
#딕셔너리 초기화
k10_outstanding = dict()
k10_floating=dict()
k10_name=dict()
#순환작업
for stock_cd in k10_component:
    stock_info(stock_cd)
    
k10_outstanding
k10_floating


### k10 과거 주가 크로울링 ###

## 날짜 검사  :<td align="center"><span class="tah p10 gray03">2018.07.18</span></td>

import datetime as dt
import pandas as pd

def date_format(d):
    d = str(d).replace('-', '.')
    
    yyyy = int(d.split('.')[0])
    mm = int(d.split('.')[1])
    dd = int(d.split('.')[2])
    
    this_date = dt.date(yyyy,mm,dd)
    return this_date

def historical_stock_naver(stock_cd, start_date='', end_date='', page_n=1, last_page=0):
    if start_date: 
        start_date = date_format(start_date) # start_date가 있다면 date포맷으로 변환
    else:
        start_date = dt.date.today() #없으면 오늘 날짜
    if end_date:
        end_date = date_format(end_date)
    else:
        end_date=dt.date.today()
        
    naver_stock = 'https://finance.naver.com/item/sise_day.nhn?code='+ stock_cd + '&page=' + str(page_n)
    source = urlopen(naver_stock).read() ## 지정한 페이지에서 코드읽기
    source = bs4.BeautifulSoup(source, 'lxml') ## 뷰티플수프로 태그별로 코드 분류
    
    dates=source.find_all('span',class_='tah p10 gray03') #날짜 수집
    prices = source.find_all('td', class_='num') ##가격수집
    
    for n in range(len(dates)):
        if len(dates) >0 :
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)
            
            if this_date <=end_date and this_date>=start_date: ## 사이 저장                
                this_close = prices[n*6].text ## 종가처리, 6배수
                this_close = this_close.replace(',','')
                this_close = float(this_close)            
                historical_prices[this_date] = this_close ## 딕셔너리 저장
            
            elif this_date<start_date:
                return historical_prices ##start_date이전이면 함수종료
            
    if last_page==0:
        last_page = source.find_all('table')[1].find('td',class_='pgRR').find('a')['href']
        last_page = last_page.split('&')[1] # &뒤의 page수 부분 추출
        last_page = last_page.split('=')[1] # =뒤의 페이지번호만 추출
        last_page = float(last_page) # 숫자형 변수로 변환
        
    if page_n < last_page:
        page_n = page_n + 1
        historical_stock_naver(stock_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

k10_historical_prices = dict()
for stock_cd in k10_component:
    historical_prices = dict()
    start_date = '2017-1-1'
    end_date = '2017-12-31'
    historical_stock_naver(stock_cd, start_date, end_date)
    k10_historical_prices[stock_cd] = historical_prices
    
k10_historical_price = pd.DataFrame(k10_historical_prices)
k10_historcial_price = k10_historical_price.fillna(method='ffill')#ffill로 구멍채우고
if k10_historical_price.isnull().values.any():
    k10_historical_price = k10_historical_price.fillna(method='bfill') #그래도 구멍있으면 bfill로 채워라
k10_historical_price.head(3)

k10_historical_price['005930'] = k10_historical_price['005930'] / 50 # 삼성전자 액면분할 수정주가
k10_historical_price.head(3)

tmp = {'Outstanding': k10_outstanding, 'Floating':k10_floating, 'Price' : k10_historical_price.iloc[0], 'Name' : k10_name}
k10_info = pd.DataFrame(tmp)

k10_info['f Market Cap'] = k10_info['Outstanding'] * k10_info['Floating'] * k10_info['Price'] * 0.01 #유동비율 반영한시가총액
k10_info['Market Cap'] = k10_info['Outstanding'] * k10_info['Price'] * 0.01
k10_info.head(3)

k10_historical_mc=k10_historical_price * k10_info['Outstanding']*k10_info['Floating']*0.01
k10_historical_mc.head(3)

'''
<데이터프래임>.sum() 은 각 열의 합 (세로)
<데이터프레임>.sum(axis=1) 은 각 행의 합 (가로)
'''
k10_historical_mc.sum(axis=1) #일자별시가총액합

k10 = pd.DataFrame() # k10 데이터프래임초기화
k10['K10 Market Cap'] = k10_historical_mc.sum(axis=1) #일자별 시가총액 저장
k10.head(3)

k10['K10'] = k10['K10 Market Cap'] / k10['K10 Market Cap'][0] * 100
k10.head(3)

## 지수그래프그리기 작업 ##

import matplotlib.pyplot as plt
%matplotlib inline

plt.figure(figsize=(10,5))
plt.plot(k10['K10'])
plt.legend(loc=0)
plt.grid(True, color='0.7', linestyle=':', linewidth=1)


###  KOSPI200과 K10 비교하기 ###

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
        
        if dates[n].text.split('.')[0].isdigit():
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)

            if this_date <=end_date and this_date>=start_date: ## 사이 저장                
                this_close = prices[n*4].text ## 종가처리, 4배수
                this_close = this_close.replace(',','')
                this_close = float(this_close)            
                historical_prices[this_date] = this_close ## 딕셔너리 저장
            
            elif this_date < start_date:
                return historical_prices ##start_date이전이면 함수종료
         ##페이지네비게이션   
    if last_page==0:
        last_page = source.find('td',class_='pgRR').find('a')['href']
        last_page = last_page.split('&')[1] # &뒤의 page수 부분 추출
        last_page = last_page.split('=')[1] # =뒤의 페이지번호만 추출
        last_page = int(last_page) # 숫자형 변수로 변환
        
    if page_n < last_page:
        page_n = page_n + 1
        historical_index_naver(index_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

historical_prices = dict()
kospi200 = historical_index_naver('KPI200', '2017-1-1', '2017-12-31')
kospi200

k200 = pd.DataFrame({'K200' : kospi200})
k200.head(3)

plt.figure(figsize = (10,5))
plt.plot(k10['K10'] / k10['K10'][0] * 100 )
plt.plot(k200['K200'] / k200['K200'][0] * 100)
plt.legend(loc=0)
plt.grid(True, color='0.7', linestyle=':', linewidth=1)