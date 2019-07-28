# -*- coding: utf-8 -*-
"""
Created on Sat Jul 20 20:03:56 2019

@author: Shinhyunjin
"""

#----------------------------------------------------------------------------#
##### Real Estate Big data Analysis with Python #####
#----------------------------------------------------------------------------#

#### Education and RealEstate Market ####

#----------------------------------------------------------------------------#

import numpy as np
import pandas as pd
import xlwings as xw
import seaborn as sns
import matplotlib.pyplot as plt
from matplotlib import font_manager, rc,style
from datetime import datetime
from dateutil.relativedelta import relativedelta
%matplotlib inline

style.use('ggplot')
font_name = font_manager.FontProperties(fname = "c:/windows/fonts/malgun.ttf").get_name()
rc('font', family = font_name)
plt.rcParams['axes.unicode_minus'] = False


#

graduate_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019_edu_result.xlsx'
raw_graduate = pd.read_excel(graduate_path, sheet_name = '2019_졸업생의 진로 현황(중)')

#---------------------------------------------------------#
# data cleaning #
#---------------------------------------------------------#

select_col = raw_graduate[['지역', '학교명', '정보공시 \n 학교코드', '졸업자.2',
                           '(특수목적고)과학고 진학자.2','(특수목적고)외고ㆍ국제고 진학자.2']]
select_col.columns = ['지역', '학교명' ,'학교코드', '졸업자', '과고', '외고']
graduate_data = select_col.drop(0)
graduate_data['과고'] = pd.to_numeric(graduate_data['과고'])
graduate_data['외고'] = pd.to_numeric(graduate_data['외고'])
graduate_data['졸업자'] = pd.to_numeric(graduate_data['졸업자'])
graduate_data['총합'] = graduate_data['과고']+graduate_data['외고']

#

def get_sido(x):
    
    temp = x.split(' ')[0]
    if len(temp) != 4:
        return temp[:2]
    else:
        return temp[0] + temp[2]
    
graduate_data['시도'] = graduate_data['지역'].dropna().apply(get_sido)
graduate_data['구군'] = graduate_data['지역'].dropna().apply(lambda x: x.split(' ')[1])

graduate_data.at[588, '시도'] = '부산'
graduate_data.at[588, '구군'] = '기장군'
graduate_data.at[3011,'시도'] = '경북'
graduate_data.at[3011,'구군'] = '예천군'

#**************************#
# Data Processing Function #
#**************************#

def graduate_preprocessing(path):
    
    raw_graduate = pd.read_excel(path, sheet_name = '2019_졸업생의 진로 현황(중)')
    
    select_col = raw_graduate[['지역', '학교명', '정보공시 \n 학교코드', '졸업자.2',
                           '(특수목적고)과학고 진학자.2','(특수목적고)외고ㆍ국제고 진학자.2']]
    select_col.columns = ['지역', '학교명' ,'학교코드', '졸업자', '과고', '외고']
    graduate_data = select_col.drop(0)
    graduate_data['과고'] = pd.to_numeric(graduate_data['과고'])
    graduate_data['외고'] = pd.to_numeric(graduate_data['외고'])
    graduate_data['졸업자'] = pd.to_numeric(graduate_data['졸업자'])
    graduate_data['총합'] = graduate_data['과고']+graduate_data['외고']

    def get_sido(x):
    
        temp = x.split(' ')[0]
        if len(temp) != 4:
             return temp[:2]
        else:
             return temp[0] + temp[2]
    
    
    graduate_data['시도'] = graduate_data['지역'].dropna().apply(get_sido)
    graduate_data['구군'] = graduate_data['지역'].dropna().apply(lambda x: x.split(' ')[1])

    graduate_data.at[588, '시도'] = '부산'
    graduate_data.at[588, '구군'] = '기장군'
    graduate_data.at[3011,'시도'] = '경북'
    graduate_data.at[3011,'구군'] = '예천군'
    
    return graduate_data

#

def gamjungwon_price_preprocessing(path):
    
    row_price = pd.read_excel(path, skiprows =10)
    
    big_col = []
    
    for num, temp in enumerate(row_price['지 역']):
        if pd.isna(temp):
            big_col.append(big_col[num-1])
        else:
            big_col.append(temp)
            
    small_col = []
    
    for num in range(len(row_price)):
        
        temp_list = list(row_price[['지 역', 'Unnamed: 1', 'Unnamed: 2', 'Unnamed: 3']].iloc[num])
        
        for temp in temp_list[3::-1]:
            if not pd.isna(temp):
                
                small_col.append(temp)
                
                break
            
    row_price.index = [big_col, small_col]
    
    transposed_price = row_price.drop(['지 역', 'Unnamed: 1', 'Unnamed: 2', 'Unnamed: 3'],
                                      axis = 1).T

    time_index=  []
    
    for time in transposed_price.index:
        temp = time.split(' ')
        time_index.append(temp[0][:-1]+'.'+temp[1][:-1])
    transposed_price.index = pd.to_datetime(time_index)
    
    return transposed_price

#------------------------------------------------------------------------------------#
##### Top High School Admission Rate Analysis #####
#------------------------------------------------------------------------------------#

graduate_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019_edu_result.xlsx'
gradu_df = graduate_preprocessing(graduate_path)
gradu_df.groupby('시도').sum()

# sort : total admission

gradu_sido = gradu_df.groupby('시도').sum()
gradu_sido.sort_values(by = '총합', ascending = False)

# sort : admission rate
gradu_sido['진학률'] = gradu_sido['총합'] / gradu_sido['졸업자'] * 100
gradu_sido.sort_values(by = '진학률', ascending = False)

# Merge : Adimission rate + real estate price #


price_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019_edu_price.xlsx'
price_data = gamjungwon_price_preprocessing(price_path)

sido_list = []

for i in gradu_sido.index:
    sido_list.append(price_data.loc['2019-1-1'][i][i])
gradu_sido['평균매매가격'] = sido_list

#-------------------------------------------------------------------------------------#
#### Visulization ####
#-------------------------------------------------------------------------------------#

plt.figure(figsize = (10,7))
plt.scatter(gradu_sido['진학률'], gradu_sido['평균매매가격'], color='darkcyan', s= 50)
plt.xlabel('졸업생 대비 특목고 진학생 비율(%)')
plt.ylabel('평균아파트매매가격')

for name in gradu_sido.index:
    
    plt.text(gradu_sido['진학률'][name] * 1.02, gradu_sido['평균매매가격'][name], name, fontsize =13)
    
plt.show()


# + Regression Straight Line Plot #

plt.figure(figsize = (10,7))
plt.scatter(gradu_sido['진학률'], gradu_sido['평균매매가격'], color = 'darkcyan', s = 50)
sns.regplot(gradu_sido['진학률'], gradu_sido['평균매매가격'], scatter = False,
            color = 'darkcyan')
plt.xlabel('졸업생 대비 특목고 진학생 비율(%)')
plt.ylabel('평균아파트매매가격')
for name in gradu_sido.index:
    
    plt.text(gradu_sido['진학률'][name] * 1.02, gradu_sido['평균매매가격'][name], name, fontsize = 13)
    
plt.show()

## Local Analysis : Seoul and Busan ##

# Seoul #
local = '서울'
graduate_data[graduate_data['시도'] == local]
gradu_gu = graduate_data[graduate_data['시도'] == local].groupby('구군').sum()
gradu_gu['진학률'] = gradu_gu['총합'] / gradu_gu['졸업자'] * 100
gradu_gu['평균매매가격'] = price_data.loc['2019-1-1'][local][gradu_gu.index]

plt.figure(figsize = (12,7))
plt.scatter(gradu_gu['진학률'], gradu_gu['평균매매가격'], color = 'steelblue', s = 50)
sns.regplot(gradu_gu['진학률'], gradu_gu['평균매매가격'], scatter = False, color = 'steelblue')
plt.xlabel('졸업생 대비 특목고 진학생 비율(%)')
plt.ylabel('평균아파트매매가격')
for name in gradu_gu.index:
    plt.text(gradu_gu['진학률'][name] * 1.02, gradu_gu['평균매매가격'][name], name, fontsize = 13)
    
plt.show()

#Busan #
local2 = '부산'
gradu_gu = graduate_data[graduate_data['시도'] == local2].groupby('구군').sum()
gradu_gu['진학률'] = gradu_gu['총합'] / gradu_gu['졸업자'] * 100
gradu_gu['평균매매가격'] = price_data.loc['2019-1-1'][local2][gradu_gu.index]
gradu_gu = gradu_gu.dropna()

plt.figure(figsize = (12,7))
plt.scatter(gradu_gu['진학률'], gradu_gu['평균매매가격'], color = 'steelblue', s = 50)
sns.regplot(gradu_gu['진학률'], gradu_gu['평균매매가격'], scatter = False, color = 'steelblue')
plt.xlabel('졸업생 대비 특목고 진학생 비율(%)')
plt.ylabel('평균아파트매매가격')
for name in gradu_gu.index:
    plt.text(gradu_gu['진학률'][name] * 1.02, gradu_gu['평균매매가격'][name], name, fontsize = 13)
    
plt.show()

#------------------------------------------------------------------------------------#
#### Private Academy Analysis ####
#------------------------------------------------------------------------------------#

aca_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019_edu_private.xlsx'
aca_raw = pd.read_excel(aca_path, skiprows =3 )
aca_data = aca_raw[aca_raw['분야'] == '입시검정및보습']
aca_data = aca_data[['시도', '행정구역', '학원수', '정원\n(수강자수)', '강사수',
                     '강의실수','월평균교습시간', '월평균교습비(원)']]
aca_data.columns = ['시도', '구군', '학원수', '수강자수', '강사수','강의실수',
                    '월평균교습시간', '월평균교습비']

aca_sido = aca_data.groupby('시도').sum()

sido_list = []
for i in aca_sido.index:
    sido_list.append(price_data.loc['2019-1-1'][i][i])
    
aca_sido['평균매매가격'] = sido_list

plt.figure(figsize = (10,7))
plt.scatter(aca_sido['학원수'], aca_sido['평균매매가격'], color = 'orange', s = 50)
sns.regplot(aca_sido['학원수'], aca_sido['평균매매가격'], scatter = False, color = 'orange')
plt.xlabel('학원수')
plt.ylabel('평균아파트매매가격')
for name in aca_sido.index:
    plt.text(aca_sido['학원수'][name]*1.02, aca_sido['평균매매가격'][name], name, fontsize =13)
plt.show()

# Local Data Analysis : Seoul #

local = '서울'

aca_gu = aca_data[aca_data['시도'] == local].groupby('구군').sum()
aca_gu['평균매매가격'] = price_data.loc['2019-1-1'][local][aca_gu.index]
aca_gu = aca_gu.dropna()

plt.figure(figsize = (12,7))
plt.scatter(aca_gu['학원수'], aca_gu['평균매매가격'], color = 'orange', s = 50)
sns.regplot(aca_gu['학원수'], aca_gu['평균매매가격'], scatter = False, color = 'orange')
plt.xlabel('학원수')
plt.ylabel('평균아파트매매가격')
for name in aca_gu.index:
    plt.text(aca_gu['학원수'][name]*1.02, aca_gu['평균매매가격'][name], name, fontsize = 13)
plt.show()

#-------------------------------------------------------------------------------------------#
##### Education + Private Academy + Market Data #####
#-------------------------------------------------------------------------------------------#

local = '서울'

gradu_gu = graduate_data[graduate_data['시도'] == local].groupby('구군').sum()
gradu_gu['진학률'] = gradu_gu['총합'] / gradu_gu['졸업자'] * 100
aca_gu = aca_data[aca_data['시도'] == local].groupby('구군').sum()
study_df = pd.merge(gradu_gu, aca_gu, how = 'outer', right_index = True, left_index= True)
study_df['평균매매가격'] = pd.to_numeric(price_data.loc['2019-1-1'][local][study_df.index])

plt.figure(figsize = (12,6))
plt.scatter(study_df['진학률'], study_df['학원수'], c= study_df['평균매매가격'],
            s = study_df['평균매매가격'] * 0.001, alpha = 0.5)
sns.regplot(study_df['진학률'], study_df['학원수'], scatter=False, color = 'silver')
plt.xlabel('특목고 진학률(%)')
plt.ylabel('학원수')

for name in study_df.index:
    plt.text(study_df['진학률'][name]*1.01, study_df['학원수'][name], name)
plt.colorbar()
plt.show()
