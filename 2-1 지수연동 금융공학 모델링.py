# -*- coding: utf-8 -*-
"""
Created on Wed Jul  4 11:34:27 2018

@author: Shinhyunjin
"""

## 네이버 주소 기반 크로울링 준비
index_cd = 'KPI200'
page_n = 1
naver_index = 'https://finance.naver.com/sise/sise_index_day.nhn?code='+ index_cd + '&page=' + str(page_n)

from urllib.request import urlopen
source = urlopen(naver_index).read()
source

## bs4로 분류

import bs4
source = bs4.BeautifulSoup(source, 'lxml')

print(source.prettify())

##2 td개수

td = source.find_all('td')
len(td)

## /html/body/div/table[1]/tbody/tr[3]/td[1] ##tbody는 테이블내용시작 알리는 것, 실체x

source.find_all('table')[0].find_all('tr')[2].find_all('td')[0] ## Xpath는 파이썬-1

##td태그 생략
d= source.find_all('td', class_='date')[0].text
d

##날짜형식 파이썬으로 변환
import datetime as dt
yyyy = int(d.split('.')[0])
mm =  int(d.split('.')[1])
dd = int(d.split('.')[2])
this_date= dt.date(yyyy,mm,dd) 
this_date

def date_format(d):
    d = str(d).replace('-', '.') ## -에서 . 변환
    yyyy= int(d.split('.')[0])
    mm = int(d.split('.')[1])
    dd = int(d.split('.')[2])
    this_date=dt.date(yyyy,mm,dd)
    return this_date

##지수 갖고 오자
this_close = source.find_all('tr')[2].find_all('td')[1].text
this_close = this_close.replace(',', '') # 쉼표제거작업
this_close = float(this_close)
this_close
# 지수갖고오는 다른 방법
p = source.find_all('td', class_='number_1')[0].text
p

dates = source.find_all('td', class_='date')
prices = source.find_all('td', class_='number_1')
#개수
len(dates)

len(prices)

# prices <td class="number_1">가 반복됨.

for n in range(len(dates)):  #date 개수만큼 반복
    this_date = dates[n].text #n번째 date값 추출
    this_date = date_format(this_date) # 날짜형식으로 변환
    this_close = prices[n*4].text # 4배수 종가지수 추출
    this_close = this_close.replace(',','') #쉼표 제거
    this_close = float(this_close)
    this_close
    print(this_date, this_close)
    
    
## 반복 추출
paging = source.find('td', class_='pgRR').find('a')['href']
paging = paging.split('&')[1] ## &기준 좌0,우1 자르고 보여주기
paging = paging.split('=')[1] ## =기준 좌0, 우1 자르고 보여주기
paging
# 마지막 페이지 번호 뽑는 코딩
last_page = source.find('td', class_='pgRR').find('a')['href']
last_page = last_page.split('&')[1]
last_page = last_page.split('=')[1]
last_page = int(last_page)
last_page

#######완성 데이터 추출 반복기능 함수#########
def histrorical_index_naver(index_cd, page_n=1, last_page=0):
    naver_index = 'https://finance.naver.com/sise/sise_index_day.nhn?code='+ index_cd + '&page=' + str(page_n)
    source = urlopen(naver_index).read() ## 지정한 페이지에서 코드읽기
    source = bs4.BeautifulSoup(source, 'lxml') ## 뷰티플수프로 태그별로 코드 분류
    
    dates=source.find_all('td',class_='date') ##<td class="date">태그에서 날짜 수집
    prices = source.find_all('td', class_='number_1') ## <td class="number_1"> 태그에서 지수수집
    
    for n in range(len(dates)):
        if dates[n].text.split('.')[0].isdigit():
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)
            
            this_close = prices[n*4].text ## 종가처리, 위에서 언급한 4배수
            this_close = float(this_close)
            
            historical_prices[this_date] = this_close ## 딕셔너리 저장
            
    if last_page==0:
        last_page = source.find('td', class_='pgRR').find('a')['href']
        last_page = last_page.split('&')[1] # &뒤의 page수 부분 추출
        last_page = last_page.split('=')[1] # =뒤의 페이지번호만 추출
        last_page = int(last_page) # 숫자형 변수로 변환
    if page_n < last_page:
        page_n = page_n + 1
        historical_index_naver(index_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

## 특정날짜 추출
def historical_index_naver(index_cd, start_date='', end_date='', page_n=1, last_page=0):
    if start_date: 
        start_date = date_format(start_date) # start_date가 있다면 date포맷으로 변환
    else:
        start_date = dt.date.today() #없으면 오늘 날짜
    if end_date:
        end_date = date_format(end_date)
    else:
        end_date=dt.date.today()


######## 네이버 코스피 추출 반복기능 함수 ########
def historical_index_naver(index_cd, start_date='', end_date='', page_n=1, last_page=0):
    if start_date: 
        start_date = date_format(start_date) # start_date가 있다면 date포맷으로 변환
    else:
        start_date = dt.date.today() #없으면 오늘 날짜
    if end_date:
        end_date = date_format(end_date)
    else:
        end_date=dt.date.today()
        
    naver_index = 'https://finance.naver.com/sise/sise_index_day.nhn?code='+ index_cd + '&page=' + str(page_n)
    source = urlopen(naver_index).read() ## 지정한 페이지에서 코드읽기
    source = bs4.BeautifulSoup(source, 'lxml') ## 뷰티플수프로 태그별로 코드 분류
    
    dates=source.find_all('td',class_='date') ##<td class="date">태그에서 날짜 수집
    prices = source.find_all('td', class_='number_1') ## <td class="number_1"> 태그에서 지수수집
    
    for n in range(len(dates)):
        if dates[n].text.split('.')[0].isdigit():
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)
            
            if this_date <=end_date and this_date>=start_date: ## 사이 저장
                
                this_close = prices[n*4].text ## 종가처리, 위에서 언급한 4배수
                this_close = float(this_close)
            
                historical_prices[this_date] = this_close ## 딕셔너리 저장
            
            elif this_date<start_date:
                return historical_prices ##start_date이전이면 함수종료
    if last_page==0:
        last_page = source.find('td', class_='pgRR').find('a')['href']
        last_page = last_page.split('&')[1] # &뒤의 page수 부분 추출
        last_page = last_page.split('=')[1] # =뒤의 페이지번호만 추출
        last_page = int(last_page) # 숫자형 변수로 변환
        
    if page_n < last_page:
        page_n = page_n + 1
        historical_index_naver(index_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

## 테스트
index_cd = 'KPI200'
historical_prices = dict()
historical_index_naver(index_cd, '2018-7-1', '2018-7-6')
historical_prices

##### 다음으로 미국 일본 등 해외 주식정보 크로울링 하기 #####

from urllib.request import urlopen
import bs4

url = 'http://finance.daum.net/global/index_daily.daum?type=default&ric=/.GSPC&page=1'
source = urlopen(url).read()
source = bs4.BeautifulSoup(source, 'lxml')

dates = source.find_all('td', class_='datetime')
dates

prices = source.find_all('td', class_='num')
print(len(dates))
print(len(prices)) ## price나 date의 3베이다 -> 3의 배수

prices[0].text ## price 데이터확인

### 본격적인 다음 해외주식(S&P 500) 크로울링 코딩 ###

def historical_global_daum(index_cd, start_date='', end_date='', page_n=1, last_page=0):
    if start_date: 
        start_date = date_format(start_date) # start_date가 있다면 date포맷으로 변환
    else:
        start_date = dt.date.today() #없으면 오늘 날짜
    if end_date:
        end_date = date_format(end_date)
    else:
        end_date=dt.date.today()
        
    url = 'http://finance.daum.net/global/index_daily.daum?type=default&ric=/.'+ index_cd + '&page=' + str(page_n)
    source = urlopen(url).read() ## 지정한 페이지에서 코드읽기
    source = bs4.BeautifulSoup(source, 'lxml') ## 뷰티플수프로 태그별로 코드 분류
    
    dates=source.find_all('td',class_='datetime') ##<td class="datetime">태그에서 날짜 수집
    prices = source.find_all('td', class_='num') ## <td class="num"> 태그에서 지수수집
    rows_in_page = len(dates)
                       
    if len(dates)>0:
        for n in range(rows_in_page):
            this_date = dates[n].text # 날짜처리
            this_date = date_format(this_date)
            
            if this_date <=end_date and this_date>=start_date: ## 사이 저장
                
                this_close = prices[n*3].text ## 종가처리, 위에서 언급한 3배수
                this_close = this_close.replace(' ','') ##공백제거
                this_close = this_close.replace('/t','') ## 탭 제거
                this_close = this_close.replace('/n','') ## 줄바꿈 제거
                this_close = this_close.replace(',','') ## 쉼표제거
                this_close = float(this_close) #숫자형식으로 변환
                
                historical_prices[this_date] = this_close ## 딕셔너리 저장
            
            elif this_date<start_date:
                return historical_prices ##start_date이전이면 함수종료
        if rows_in_page == 10:
             page_n = int(page_n)
             page_n = page_n + 1
  
             historical_global_daum(index_cd, start_date, end_date, page_n, last_page) 
    return historical_prices

historical_prices = dict()
daum = historical_global_daum('GSPC', '2018-7-1', '2018-7-6')
daum


##### 데이터프레임으로 여러 딕셔너리를 테이블 하나로 머지 ####

index_cd = 'KPI200'
historical_prices = dict()
kospi200 = historical_index_naver(index_cd, '2008-01-01', '2018-07-16')

index_cd = 'GSPC'
historical_prices = dict()
sp500 = historical_global_daum(index_cd, '2008-01-01', '2018-07-16')

tmp = {'S&P500' : sp500, 'KOSPI200' : kospi200} ## 임시 딕셔너리로 합쳐보기 
tmp

import pandas as pd

df = pd.DataFrame(tmp) ## 판다스 데이터 엑셀화시키기
df

df = df.fillna(method='ffill')
if df.isnull().values.any():
    df = df.fillna(method='bfill')
df

df.head() ## 앞 5줄
df.tail() ## 뒤 5줄

### 맷플롯립을 이용해 그래프 그리기 ####
import matplotlib.pyplot as plt
%matplotlib inline

plt.figure(figsize=(10,5)) #크기조절
plt.plot(df['S&P500']) #데이터 선택
plt.plot(df['KOSPI200']) 
plt.legend(loc=0) # 범례위치
plt.grid(True, color='0.7', linestyle=':', linewidth=1) # 그리드 설정

df.iloc[0] # 인덱스(위치)로 지정
df.loc[dt.date(2008, 1, 2)] # 날짜인 값으로 지정

plt.figure(figsize=(10,5))
plt.plot(df['S&P500']/df['S&P500'].loc[dt.date(2008,1,2)]*100)
plt.plot(df['KOSPI200']/df['KOSPI200'].loc[dt.date(2008,1,2)]*100)
plt.legend(loc=0)
plt.grid(True, color='0.7', linestyle=':', linewidth=1)

df_ratio_2016_now = df.loc[dt.date(2016, 1, 1):] / df.loc[dt.date(2016,1,4)]* 100
df_ratio_2016_now.head(3)

## df(데이터프레임).loc[시작: 끝]으로 일부기간 조회가능

plt.figure(figsize=(10,5))
plt.plot(df_ratio_2016_now['S&P500'])
plt.plot(df_ratio_2016_now['KOSPI200'])
plt.legend(loc=0)
plt.gird(True, color='0.7', linestyle=':', linewidth=1)

##### 회귀분석 #####
##산포도그리기##

plt.figure(figsize=(5,5))
plt.scatter(df_ratio_2016_now['S&P500'], df_ratio_2016_now['KOSPI200'], marker='.')
plt.grid(True, color='0.7', linestyle=':', linewidth=1)
plt.xlabel('S&P500')
plt.ylabel('KOSPI200')

##회귀분석 코딩##

import numpy as np
from sklearn.linear_model import LinearRegression

x = df_ratio_2016_now['S&P500']
y = df_ratio_2016_now['KOSPI200']

independent_var = np.array(x).reshape(-1,1) # 1개 칼럼 np.array로 변환. 디폴트 (n열, 1행)
dependent_var = np.array(y).reshape(-1,1)

regr = LinearRegression()
regr.fit(independent_var, dependent_var) # 회귀분석 (x,y순)'

result = {'Slope':regr.coef_[0,0], 'Intercept':regr.intercept_[0], 'R^2':regr.score(independent_var, dependent_var)}
result

plt.figure(figsize=(5,5))
plt.scatter(independent_var, dependent_var, marker='.', color='skyblue')
plt.plot(independent_var, regr.predict(independent_var), color='r', linewidth=3)
plt.grid(True, color='0.7', linestyle=':', linewidth=1)
plt.xlabel('S&P500')
plt.ylabel('KOSPI200')

### K10지수 : 10대기업의 지수 ####

