# -*- coding: utf-8 -*-
"""
Created on Tue Jul 16 17:36:05 2019

@author: Shinhyunjin
"""
#----------------------------------------------------------------------------#
##### Real Estate Big data Analysis with Python #####
#----------------------------------------------------------------------------#

##### Demand and Supply Analysis #####

#----------------------------------------------------------------------------#


import numpy as np
import pandas as pd
import xlwings as xw
import matplotlib.pyplot as plt
from matplotlib import font_manager, rc,style
from datetime import datetime
from dateutil.relativedelta import relativedelta
%matplotlib inline


def KBpriceindex_preprocessing(path, data_type):
    wb = xw.Book(path)
    sheet = wb.sheets[data_type]
    row_num = sheet.range(1,1).end('down').end('down').end('down').row
    data_range = 'A2:GE' + str(row_num)
    raw_data = sheet[data_range].options(pd.DataFrame, index = False, header =True).value

    bignames = '서울 대구 부산 대전 광주 인천 울산 세종 경기 강원 충북 충남 전북 전남 경북 경남 제주도 6개광역시 5개광역시 수도권 기타지방 구분 전국'
    bigname_list = bignames.split(' ')
  
    big_col = list(raw_data.columns)
    small_col = list(raw_data.iloc[0])

#column cleaning
    for num, gu_data in enumerate(small_col): # order saved
        if gu_data == None:
            small_col[num] = big_col[num]
        
        check = num
    
        while True:
            if big_col[check] in bigname_list:
                big_col[num] = big_col[check]
                break
            else:
                check = check-1
            
    big_col[129] = '경기'
    big_col[130] = '경기'
    small_col[185] = '서귀포'

    raw_data.columns = [big_col, small_col]
    new_col_data = raw_data.drop([0,1])

# date list #

    index_list = list(new_col_data['구분']['구분'])
    new_index =[]

    for num, raw_index in enumerate(index_list):
        
        temp = str(raw_index).split('.')
        if int(temp[0]) > 12:
            if len(temp[0]) ==2:
                new_index.append('19'+temp[0] + '.' + temp[1])
            else:
                new_index.append(temp[0] + '.' + temp[1])
        else:
             
             new_index.append(new_index[num-1].split('.')[0]+'.'+temp[0])

    new_col_data.set_index(pd.to_datetime(new_index), inplace = True)
    cleaned_data = new_col_data.drop(('구분', '구분'), axis = 1)
    return cleaned_data


path = r'C:/Users/Shinhyunjin/Dropbox/data/KB Real Estate Data/1906data.xlsx'
price_index = KBpriceindex_preprocessing(path, '매매apt')
jeonse_index = KBpriceindex_preprocessing(path, '전세apt')


# specific periods #

index_date = datetime(2010,1,1)
time_range = 12
prev_date = index_date - relativedelta(months = time_range)

print(index_date)
print(prev_date)

price_index.loc[index_date]

##

(price_index.loc[index_date]-price_index.loc[prev_date])/price_index.loc[prev_date]

demand_df = pd.DataFrame()
demand_df['매매증감률'] = (price_index.loc[index_date] - price_index.loc[prev_date])/price_index.loc[prev_date]
demand_df['전세증감률'] = (jeonse_index.loc[index_date] - jeonse_index.loc[prev_date])/jeonse_index.loc[prev_date]

#

prev_date2 = index_date - relativedelta(months = time_range*3)
price_index[prev_date2:index_date][:-1]

#

demand_df['이전최대값'] = price_index[prev_date2:index_date][:-1].max()
demand_df['최대값대비증감률'] = (price_index.loc[index_date] - demand_df['이전최대값']) / demand_df['이전최대값']
#

# strategy realization #
demand_df['매매증감률'] > 0.01

demand_df['매매가상승'] = demand_df['매매증감률'] > 0.01
demand_df['전세가상승'] = demand_df['전세증감률'] > 0.01

demand_df['더빠른전세상승'] = demand_df['전세증감률'] > demand_df['매매증감률']
demand_df['최대값대비상승'] = demand_df['최대값대비증감률'] > 0

demand_df['수요총합'] =demand_df[['매매가상승', '전세가상승', '더빠른전세상승', '최대값대비상승']].sum(axis=1)

#

demand_df = demand_df[demand_df['수요총합']==4]
demand_df
demand_df.loc[[('서울', '동대문구'), ('서울', '강북구')]]

#

selected_index = []

for name in demand_df.index:
    if name[0] is not name[1]:
        selected_index.append((name[0], name[1]))
        
demand_df = demand_df.loc[selected_index]


# Function 화 #


def demand(price_index, jeonse_index, index_date, time_range):
    
    prev_date = index_date - relativedelta(months = time_range)
    prev_date2 = index_date - relativedelta(months = time_range * 3)
    
    demand_df = pd.DataFrame()
    
    demand_df['매매증감률'] = (price_index.loc[index_date] - price_index.loc[prev_date])/price_index.loc[prev_date].replace(0, None)
    
    demand_df['전세증감률'] = (jeonse_index.loc[index_date] - jeonse_index.loc[prev_date])/jeonse_index.loc[prev_date].replace(0, None)
    
    demand_df['이전최대값'] = price_index[prev_date2:index_date][:-1].max()
    
    demand_df['최대값대비증감률'] = (price_index.loc[index_date] - demand_df['이전최대값'])/demand_df['이전최대값'].replace(0, None)
    
    demand_df['매매가상승'] = demand_df['매매증감률'] > 0.01
    demand_df['전세가상승'] = demand_df['전세증감률'] > 0.01
    demand_df['더빠른전세상승']= demand_df['전세증감률'] > demand_df['매매증감률']
    demand_df['최대값대비상승'] = demand_df['최대값대비증감률'] > 0
    demand_df['수요총합'] = demand_df[['매매가상승', '전세가상승', '더빠른전세상승', '최대값대비상승']].sum(axis = 1)
    
    demand_df = demand_df[demand_df['수요총합'] ==4]
    
    selected_index = []
    
    for name in demand_df.index:
        if name[0] is not name[1]:
            selected_index.append((name[0], name[1]))
    demand_df = demand_df.loc[selected_index]
    
    return demand_df


price_index = KBpriceindex_preprocessing(path, '매매apt')
jeonse_index = KBpriceindex_preprocessing(path, '전세apt')

index_date = datetime(2010,1,1)
time_range = 12

demand_ex = demand(price_index, jeonse_index, index_date, time_range)


# Visualization #
style.use('ggplot')
%matplotlib inline

font_name = font_manager.FontProperties(fname = "c:/windows/fonts/malgun.ttf").get_name()
rc('font', family = font_name)
plt.rcParams['axes.unicode_minus'] = False


#

si = '서울'
gu = '동대문구'
index_date = datetime(2010,1,1)

prev_date = index_date - relativedelta(months = 12)
prev_date2 = index_date - relativedelta(months = 36)
graph_start = index_date - relativedelta(years = 3)
graph_end = index_date + relativedelta(years = 3)

plt.figure(figsize  = (10,5))
plt.title(si + ' - ' + gu)
plt.plot(price_index[si][gu][graph_start:graph_end], label = '매매가')
plt.plot(jeonse_index[si][gu][graph_start:graph_end], label = '전세가')
plt.axvline(x = index_date, color = 'lightcoral', linestyle = '--')
plt.axvline(x = prev_date, color = 'darkseagreen', linestyle = '--')
plt.axvline(x = prev_date2, color = 'darkseagreen', linestyle = '--')
plt.legend()
plt.show()

# multiple visulaization

for name in demand_ex.index:
    print(name)
    
index_date = datetime(2010,1,1)
time_range = 12
prev_date = index_date - relativedelta(months = time_range)
prev_date2 = index_date - relativedelta(months = time_range * 3)
graph_start = index_date - relativedelta(months = time_range*3)
graph_end = index_date + relativedelta(months = time_range*3)
    
num_row = int((len(demand_ex.index)-1)/2)+1
    

plt.figure(figsize = (15, num_row * 5))
for i, spot in enumerate(demand_ex.index):
    plt.subplot(num_row, 2, i+1)
    plt.title(spot)
    
    si = spot[0]
    gu = spot[1]
    
    plt.plot(price_index[si][gu][graph_start:graph_end], label = '매매가')
    plt.plot(jeonse_index[si][gu][graph_start:graph_end], label = '전세가')
    
    plt.axvline(x = index_date, color = 'lightcoral', linestyle = '--')
    plt.axvline(x = prev_date, color = 'darkseagreen', linestyle = '--')
    plt.axvline(x = prev_date2, color = 'darkseagreen', linestyle = '--')
    plt.legend(loc = 'lower right')
    
plt.show()
    
#_---------------------------------------------------------------------------#
### Supply Analysis ###
#_---------------------------------------------------------------------------#

##

# load 인허가 data
permission_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/supply_1.xlsx'
pd.read_excel(permission_path)

permission_raw  = pd.read_excel(permission_path, skiprows = 10, index_col = 0)
transposed_permission = permission_raw.T

ex ='2007년 01월'
temp_list = ex.split(' ')
temp_list[0][:4] + '.' + temp_list[1][:2]

new_index = []

for old_date in transposed_permission.index:
    temp_list = old_date.split(' ')
    new_index.append(temp_list[0][:4] + '.' + temp_list[1][:2])
    

transposed_permission.index = pd.to_datetime(new_index)
transposed_permission.columns.name = None

#***************#
## function 화 ##
#***************#

def permission_preprocessing(path):
    permission_raw = pd.read_excel(path, skiprows = 10, index_col = 0)
    transposed_permission = permission_raw.T
    new_index = []
    
    for old_date in transposed_permission.index:
        temp_list = old_date.split(' ')
        new_index.append(temp_list[0][:4] + '.' + temp_list[1][:2])
        
    transposed_permission.index = pd.to_datetime(new_index)
    transposed_permission.columns.name = None
    
    return transposed_permission

# load 미분양데이터 #

unsold_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/supply_2.xlsx'
unsold_raw = pd.read_excel(unsold_path, skiprows= 1, index_col = 0)
#

del unsold_raw['시군구']
transposed_unsold = unsold_raw.T
transposed_unsold.index = pd.to_datetime(transposed_unsold.index)
transposed_unsold.columns.name = None


#***************#
## function 화 ##
#***************#

def unsold_preprocessing(path):
    
    unsold_raw = pd.read_excel(path, skiprows = 1, index_col = 0)
    
    del unsold_raw['시군구']
    
    transposed_unsold = unsold_raw.T
    transposed_unsold.index = pd.to_datetime(transposed_unsold.index)
    transposed_unsold.columns.name = None
    
    return transposed_unsold

#-----------------------------------------------------------------------------#
# Supply Visulization #
#_----------------------------------------------------------------------------#
    
permission = permission_preprocessing(permission_path)
unsold = unsold_preprocessing(unsold_path)
kb_path = r'C:/Users/Shinhyunjin/Dropbox/data/KB Real Estate Data/1906data.xlsx'
price_index = KBpriceindex_preprocessing(kb_path, '매매종합')
jun_index = KBpriceindex_preprocessing(kb_path, '전세종합')

plt.figure(figsize = (10,6))
ax = plt.subplot()
ax2 = ax.twinx()

si = '서울'
gu = '서울'

plt.title(si + '-' + gu)
ln1 = ax.plot(price_index[si][gu]['2009-1':], label = '매매가')
ln2 = ax2.plot(permission[si]['2009-1':], label = '인허가', color = 'green', marker = "o")
lns = ln1+ ln2
labs = [l.get_label() for l in lns]
ax.legend(lns, labs, loc='upper left')
plt.show()

#-----------------------------------------------------------------------------#
# 인허가 (T) -> T+2 ~ +3 반영 #
# Graph #
#_----------------------------------------------------------------------------#

year_permission = permission.groupby(permission.index.year).sum()

modified_permission = year_permission.shift(2)

temp = []

for year in modified_permission.index:
    temp.append(str(year) + '-6-1')
modified_permission.index = pd.to_datetime(temp)

#

plt.figure(figsize = (10,6))
ax = plt.subplot()
ax2 = ax.twinx()

si = '서울'
gu = '서울'

plt.title(si + '-' + gu)
ln1 = ax.plot(price_index[si][gu]['2009-1':], label = '매매가')
ln2 = ax2.plot(modified_permission[si]['2009':], label = '인허가', color = 'green', marker = "o")
lns = ln1 + ln2
labs = [l.get_label() for l in lns]
ax.legend(lns, labs, loc = 'upper left')
plt.show()


#-----------------------------------------------------------------------------#
# 미분양 + 전세가 #
# Graph #
#_----------------------------------------------------------------------------#

plt.figure(figsize = (10,6))
ax = plt.subplot()
ax2 = ax.twinx()

si = '서울'
gu = '서울'

plt.title(si + '-' + gu)
ln1 =ax.plot(price_index[si][gu]['2009-1':], label = '매매가')
ln2 = ax.plot(jun_index[si][gu]['2009-1':], label = '전세가')
ln3 = ax2.plot(modified_permission[si]['2009':], label = '인허가', color = 'lightslategray', ls = '--')
ln4 = ax2.plot(unsold[si]['2009':], label = '미분양', color = 'y', ls= '--')
lns = ln1 + ln2 + ln3 + ln4
labs = [l.get_label() for l in lns]
ax.legend(lns, labs, loc = 'upper left')
plt.show()


#-----------------------------------------------------------------------------#
# 미분양 + 전세가 (%) #
# Graph #
#_----------------------------------------------------------------------------#

plt.figure(figsize = (10,6))
ax = plt.subplot()
ax2 = ax.twinx()

si = '서울'
gu = '서울'

plt.title(si + '-' + gu)
ln1 =ax.plot(price_index[si][gu]['2009-1':].pct_change(12), label = '매매가')
ln2 = ax.plot(jun_index[si][gu]['2009-1':].pct_change(12), label = '전세가')
ln3 = ax2.plot(modified_permission[si]['2009':].pct_change(12), label = '인허가', color = 'lightslategray', ls = '--')
ln4 = ax2.plot(unsold[si]['2009':].pct_change(12), label = '미분양', color = 'y', ls= '--')
lns = ln1 + ln2 + ln3 + ln4
labs = [l.get_label() for l in lns]
ax.legend(lns, labs, loc = 'upper left')
plt.show()



#-----------------------------------------------------------------------------#
# 수요전략 + 공급전략 #
# Graph #
#_----------------------------------------------------------------------------#

index_date = datetime(2013,1,1)
time_range =12
demand_1 = demand(price_index, jun_index, index_date, time_range)

prev_date = index_date - relativedelta(months = time_range)
prev_date2 = index_date - relativedelta(months = time_range *3 )
graph_start = index_date - relativedelta(months = time_range*3)
num_row = int((len(demand_1.index)-1)/2)+1

#
plt.figure(figsize = (15, num_row * 5))

for i, spot in enumerate(demand_1.index):
    ax = plt.subplot(num_row, 2, i+1)
    si = spot[0]
    gu = spot[1]
    plt.title(spot)
    ax2 = ax.twinx()
    ln1 = ax.plot(price_index[si][gu][graph_start:], label ='매매가')
    ln2 = ax.plot(jun_index[si][gu][graph_start:], label = '전세가')
    ln3 = ax2.plot(modified_permission[si][graph_start:], label = '인허가', color = 'lightslategray', ls = '--')
    ln4 = ax2.plot(unsold[si][graph_start:], label = '미분양', color = 'y', ls= '--')
    
    ax.axvline(x = index_date, color = 'lightcoral', linestyle = '--')
    ax.axvline(x = prev_date, color = 'darkseagreen', linestyle = '--')
    ax.axvline(x = prev_date2, color = 'darkseagreen', linestyle = '--')

    lns = ln1 + ln2 + ln3 + ln4
    labs = [l.get_label() for l in lns]
    ax.legend(lns, labs, loc = 'upper left')
    
plt.show()