# -*- coding: utf-8 -*-
"""
Created on Thu Sep 13 22:33:54 2018

@author: Shinhyunjin
"""

#### 투자자산 가격 결정의 원리 : 숙제1 ####

import numpy as np
import pandas as pd
import pandas_datareader.data as web
import matplotlib.pyplot as plt
%matplotlib inline

## 주식 리스트만들기 ##

pf_component = ['009540','064350','005490'] #현대중공업, 현대로템, 포스코

## 시계열 정보 수집 ##

import bs4
from urllib.request import urlopen

import datetime as dt

def date_format(d):
    d = str(d).replace('-', '.')
    
    yyyy = int(d.split('.')[0])
    mm = int(d.split('.')[1])
    dd = int(d.split('.')[2])
    
    this_date = dt.date(yyyy,mm,dd)
    return this_date
#
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
    
    dates=source.find_all('span',class_='tah p10 gray03') ##<td class="date">태그에서 날짜 수집
    prices = source.find_all('td', class_='num') ## <td class="number_1"> 태그에서 지수수집
    
    for n in range(len(dates)):
        if len(dates) >0 :
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)
            
            if this_date <=end_date and this_date>=start_date: ## 사이 저장                
                this_close = prices[n*6].text ## 종가처리, 위에서 언급한 4배수
                this_close = this_close.replace(',','') #쉼표제거
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
#초기화
pf_historical_prices = dict()
#포트폴리오적용
for stock_cd in pf_component:
    historical_prices = dict()
    start_date = '2013-9-12'
    end_date = '2018-9-12'
    historical_stock_naver(stock_cd, start_date, end_date)
    pf_historical_prices[stock_cd] = historical_prices
#데이터 프레임 변환
pf_historical_price = pd.DataFrame(pf_historical_prices)

#
pf_historcial_price = pf_historical_price.fillna(method='ffill')#ffill로 구멍채우고
if pf_historical_price.isnull().values.any():
    pf_historical_price = pf_historical_price.fillna(method='bfill') #그래도 구멍있으면 bfill로 채워라
pf_historical_price.head(3)
