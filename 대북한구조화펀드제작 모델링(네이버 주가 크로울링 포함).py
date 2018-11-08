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

## 20180919 남북경협주 ##
'''
017800 현대엘리베이터
011390 부산산업
064350 현대로템
094940 푸른기술
045390 대아이티아이

'''
k10_component = ['017800', '011390', '064350', '094940','045390']

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
## 삼성전자 액면분할
# k10_historical_price['005930'] = for[x for x in k10_historical_price['005930'] if x.index < '2018-05-14']
#k10_historical_price[k10_historical_price.loc[:, '005930'].index < dt.date(2018, 5, 16)] /= 50
#test = k10_historical_price
#k10_historical_price.loc[k10_historical_price.index < dt.date(2018, 5, 4), '005930'] /= 50


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
kospi200 = historical_index_naver(index_cd, '2017-1-1' ,'2018-9-20')
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


