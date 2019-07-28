# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 20:07:23 2019

@author: Shinhyunjin
"""


#----------------------------------------------------------------------------#
##### Real Estate Big data Analysis with Python #####
#----------------------------------------------------------------------------#

#### Labor Market and RealEstate Market ####

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



# Load Data #

job_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/18worker.xlsx'
job_raw = pd.read_excel(job_path, skiprows = 1)

#

job_data = job_raw[job_raw['산업별'] == '전산업']
job_data = job_data[['지역별', '전체종사자']]
job_data.columns = ['지역명', '고용자수']

#
def get_sido(x):
    
    temp = x.split(' ')[0]
    if len(temp) != 4:
        return temp[:2]
    else:
        return temp[0] + temp[2]
    
job_data['시도'] = job_data['지역명'].apply(get_sido)
job_data['구군'] = job_data['지역명'].apply(lambda x: x.split(' ')[1])

#

def job_preprocessing(path):
    
    job_raw = pd.read_excel(path, skiprows = 1)
    
    job_data = job_raw[job_raw['산업별'] == '전산업']
    job_data = job_data[['지역별', '전체종사자']]
    job_data.columns = ['지역명', '고용자수']
    
    def get_sido(x):
        
        temp = x.split(' ')[0]
        
        if len(temp) != 4:
            return temp[:2]
        
        else:
            return temp[0] + temp[2]
        
    
    job_data['시도'] = job_data['지역명'].apply(get_sido)
    job_data['구군'] = job_data['지역명'].apply(lambda x: x.split(' ')[1])
    
    return job_data

# load people data
    
house_n_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/19people.xlsx'
house_n_raw = pd.read_excel(house_n_path)

house_n_raw.columns = ['시도', '구군', '세대수']

big_col = []

for num, temp in enumerate(house_n_raw['시도']):
    
    if pd.isna(temp):
        big_col.append(big_col[num-1])
        
    else:
        big_col.append(temp)
        
house_n_raw['시도'] = big_col

#

def get_sido(x):
    
    if len(x) != 4:
        return x[:2]
    else:
        return x[0] + x[2]
    
house_n_raw['시도'] = house_n_raw['시도'].apply(get_sido)
house_n_data = house_n_raw[house_n_raw['구군'] != '소계']

#

def house_number_preprocessing(path):
    
    house_n_raw=  pd.read_excel(path)
    house_n_raw.columns = ['시도', '구군', '세대수']
    
    big_col = []
    
    for num, temp in enumerate(house_n_raw['시도']):
        
        if pd.isna(temp):
            big_col.append(big_col[num-1])
            
        else:
            big_col.append(temp)
    house_n_raw['시도'] = big_col
    
    def get_sido(x):
        
        if len(x) != 4:
            return x[:2]
        else:
            return x[0] + x[2]
        
    house_n_raw['시도'] = house_n_raw['시도'].apply(get_sido)
    house_n_data = house_n_raw[house_n_raw['구군'] != '소계']
    
    return house_n_data


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


# 
    
job_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/18worker.xlsx'
job_df = job_preprocessing(job_path)
job_sido = job_df.groupby('시도').sum()
job_sido = job_sido.sort_values(by = '고용자수', ascending = False)

#-----------------------------------------------------------------------#
#### Visulization ####
#-----------------------------------------------------------------------#

# Workers by Provinces
plt.figure(figsize = (12,4))
job_sido['고용자수'].plot(kind = 'bar', color = 'darkcyan')
plt.axhline(y = job_sido['고용자수'].mean(), color = 'orange', linewidth = 2, ls = '--')
plt.xticks(rotation = 0)
plt.xlabel('')
plt.ylabel('고용자수')
plt.show()

# Workes / People by Provinces

house_n_path = r'C:/users/shinhyunjin/dropbox/data/KB Real Estate Data/19people.xlsx'
house_n_df = house_number_preprocessing(house_n_path)

job_sido['세대수'] = house_n_df.groupby('시도').sum().loc[job_sido.index]
job_sido['세대수대비고용'] = job_sido['고용자수']/job_sido['세대수'] * 100


#  

plt.figure(figsize=(12,4))
job_sido.sort_values(by = '세대수대비고용', ascending = False)['세대수대비고용'].plot(kind = 'bar', color ='darkcyan')
plt.axhline(y = job_sido['세대수대비고용'].mean(), color = 'orange', linewidth =2 , ls = '--')
plt.xticks(rotation = 45)
plt.xlabel('')
plt.ylabel('세대수 대비 고용자수 (%)')
plt.show()


###

price_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019_edu_price.xlsx'
price_df = gamjungwon_price_preprocessing(price_path)

sido_list = []
for i in job_sido.index:
    
    sido_list.append(price_df.loc['2019-6-1'][i][i])
    
job_sido['평균매매가격'] = sido_list

plt.figure(figsize = (10,6))
plt.scatter(job_sido['고용자수'] , job_sido['세대수'], c = job_sido['평균매매가격'],
            s = job_sido['평균매매가격'] * 0.001, cmap = "YlOrRd", alpha = 0.5)
plt.xlabel('고용자수')
plt.ylabel('세대수')
for name in job_sido.index:
    plt.text(job_sido['고용자수'][name]*1.01, job_sido['세대수'][name]*1.05, name, fontsize =13)
    
plt.colorbar()
plt.show()



# 
plt.figure(figsize = (10,6))
plt.scatter(job_sido['고용자수'], job_sido['세대수'], c = job_sido['평균매매가격'],
            s = job_sido['평균매매가격'] * 0.001, cmap = "YlOrRd", alpha = 0.5)
plt.xlabel('고용자수')
plt.ylabel('세대수')
for name in job_sido.index:
    plt.text(job_sido['고용자수'][name] * 1.01, job_sido['세대수'][name] * 1.05, name,
             fontsize = 13)
plt.colorbar()
plt.show()

#

local = '서울'

job_gugun=- job_df[job_df['시도']==local].groupby('구군').sum()
job_gugun['세대수']= house_n_df[house_n_df['시도'] == local].groupby('구군').sum().loc[job_gugun.index]
job_gugun['세대수대비고용'] = job_gugun['고용자수'] / job_gugun['세대수'] * 100
job_gugun['평균매매가격'] = price_df.loc['2019-6-1'][local][job_gugun.index]
job_gugun = job_gugun.dropna()


###

plt.figure(figsize = (12,4))
job_gugun.sort_values(by = '고용자수', ascending = False)['고용자수'].plot(kind = 'bar', color = 'darkcyan')
plt.axhline(y = job_gugun['고용자수'].mean(), color = 'orange', linewidth = 2, ls = '--')
plt.xticks(rotation = 45)
plt.xlabel('')
plt.ylabel('고용자수')
plt.show()

plt.figure(figsize = (12,4))
job_gugun.sort_values(by = '세대수대비고용', ascending = False)['세대수대비고용'].plot(kind = 'bar', color = 'darkcyan')
plt.axhline(y = job_sido['세대수대비고용'].mean(), color = 'orange', linewidth = 2, ls ='--')
plt.xticks(rotation = 45)
plt.xlabel('')
plt.ylabel('세대수 대비 고용자수 (%)')
plt.show()

#------------------------------------------------------------------------------#
# Scatter plotting #
#------------------------------------------------------------------------------#

## Seoul Workers, People, Real Estate Price ##

plt.figure(figsize= (10,6))
plt.scatter(job_gugun['고용자수'], job_gugun['세대수'], c = job_gugun['평균매매가격'],
            s =pd.to_numeric(job_gugun['평균매매가격'])*0.001, cmap = "YlOrRd", alpha = 0.5)
plt.xlabel('고용자수')
plt.ylabel('세대수')

for name in job_gugun.index:
    plt.text(job_gugun['고용자수'][name]*1.01, job_gugun['세대수'][name], name)
    
plt.colorbar()
plt.show()

## working rate, ㅋeople, Real Estate Price ##

plt.figure(figsize = (10,6))
plt.scatter(job_gugun['세대수대비고용'], job_gugun['세대수'], c = job_gugun['평균매매가격'],
            s = pd.to_numeric(job_gugun['평균매매가격'])*0.001, cmap="YlOrRd", alpha = 0.5)
plt.xlabel('세대수 대비 고용자수 (%)')
plt.ylabel('세대수')

for name in job_gugun.index:
    
    plt.text(job_gugun['세대수대비고용'][name] * 1.01, job_gugun['세대수'][name], name)

plt.colorbar()
plt.show()


ㅋ

