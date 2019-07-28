# -*- coding: utf-8 -*-
"""
Created on Tue Jul 23 21:35:34 2019

@author: Shinhyunjin
"""

import folium
import pandas as pd

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

seoul_map = folium.Map(location = [37.561437, 126.9751701])
seoul_map = folium.Map(location = [37.561437, 126.9751701], zoom_start = 14)
seoul_map = folium.Map(location = [37.561437, 126.9751701], popup = '서울시청').add_to(seoul_map)

#

school_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019school.xlsx'
school_raw = pd.read_excel(school_path)
school_data = school_raw[['정보공시 \n 학교코드', '학교명', '위도', '경도' ]]
school_data.columns = ['학교코드', '학교명', '위도', '경도']
school_df = school_data.drop_duplicates()


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


graduate_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/2019_edu_result.xlsx'
gradu_df = graduate_preprocessing(graduate_path)

#

total_school_df = pd.merge(gradu_df, school_df, how = 'inner', right_on = '학교코드',
                           left_on = '학교코드')

#

seoul_school = total_school_df[total_school_df['시도'] == '서울']
good_school = seoul_school[seoul_school['총합'] > 0]
seoul_map = folium.Map(location = [37.5614378, 126.9751701], zoom_start =11)

for n in good_school.index:
    folium.Marker([good_school['위도'][n], good_school['경도'][n]],
                  popup = good_school['학교명_x'][n]).add_to(seoul_map)
    
seoul_school['비율'] = seoul_school['총합'] / seoul_school['졸업자']*100
good_school = seoul_school[seoul_school['비율'] >=3]
bad_school = seoul_school[seoul_school['비율'] < 3]
seoul_map = folium.Map(location = [37.5614378, 126.9751701], zoom_start = 11)

#

for n in good_school.index:
    folium.CircleMarker([good_school['위도'][n], good_school['경도'][n]],
                          color = 'crimson', fill_color = 'crimson', radius = 7).add_to(seoul_map)

for n in bad_school.index:
    folium.CircleMarker([bad_school['위도'][n], bad_school['경도'][n]],
                        color = '#3186cc', fill_color = '#3186cc', radius = 7).add_to(seoul_map)
                        
    