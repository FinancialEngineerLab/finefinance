# -*- coding: utf-8 -*-
"""
Created on Sun Jul 14 17:40:43 2019

@author: Shin hyunjin

@KAIST Business School
@SK Securities

"""
#----------------------------------------------------------------------------#
##### Real Estate Big data Analysis with Python #####
#----------------------------------------------------------------------------#

import numpy as np
import pandas as pd
import xlwings as xw
import matplotlib.pyplot as plt
from matplotlib import font_manager, rc,style
%matplotlib inline

## Pandas Practice ##

series_ex = pd.Series([199, 89, 129, 110,105], index = ['월', '화', '수','목','금'])
series_ex

series_ex['월']
series_ex[0]
series_ex[1]
series_ex.sort_values(ascending=True)
series_ex.sort_values(ascending=False)


#dataframe pandas
dataframe_ex = pd.DataFrame({'서울GDP':[100,110,120,130,135], '부산GDP':[70,90,80,90,100],
                             '광주GDP':[50,60,65,69,80]}, index = [2012,2013,2014,2015,2016])
dataframe_ex
dataframe_ex['서울GDP']
dataframe_ex.loc[2015] # index : loc
dataframe_ex[dataframe_ex['부산GDP'] > 80]

#-----------------------------------------------------------------------------#
### 1. Price Data Analysis ###

### Data Cleaning ###
#data load
path = r'C:/Users/Shinhyunjin/Dropbox/data/KB Real Estate Data/1906data.xlsx'
data_type = '매매종합'
#path : 엑셀 파일 디렉토리
#매매종합 매매APT 매매연립 매매단독 전세종합 전세APT 전세연립 전세단독

#------------------------------------------------------------------------------#
##### Data Cleaning Function #####
#------------------------------------------------------------------------------#

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

#-----------------------------------------------------------------------------#
#### Data Visulization ####
#-----------------------------------------------------------------------------#

#preparation
font_name = font_manager.FontProperties(fname="c:/windows/fonts/malgun.ttf").get_name()
rc('font', family = font_name)
plt.rcParams['axes.unicode_minus']= False

# Whole Regions #
new_data = KBpriceindex_preprocessing(path, data_type)
new_data['전국']['전국'].plot(legend = '전국')
plt.show()

# Specific periods #
new_data['전국']['전국']['2016-01':].plot(legend='전국')
plt.show()

# Multiple Regions #

plt.figure(figsize = (10,5))

plt.subplot(1,2,1)
plt.title('서울')
plt.plot(new_data['서울']['서울']['2016-01':])

plt.subplot(1,2,2)
plt.title('경기')
plt.plot(new_data['경기']['경기']['2016-01':])

# for 문으로 표현 #

spots = '전국 서울 경기 부산'
start_date = '2016-1'
spot_list= spots.split(' ')
num_row = int((len(spot_list)-1)/2)+1

plt.figure(figsize = (10, num_row*5))
for i, spot in enumerate(spot_list):
    plt.subplot(num_row, 2, i+1)
    plt.title(spot)
    plt.plot(new_data [spot][spot][start_date:])
    plt.ylim(90,101)
plt.show()


spots = '전국 서울 대구 부산'
start_date = '2017-1'
spot_list= spots.split(' ')
num_row = int((len(spot_list)-1)/2)+1

plt.figure(figsize = (10, num_row*5))
for i, spot in enumerate(spot_list):
    plt.subplot(num_row, 2, i+1)
    plt.title(spot)
    plt.plot(new_data [spot][spot][start_date:])
    plt.ylim(90,101)
plt.show()


spots = '전국 서울 대구 울산'
start_date = '2018-1'
spot_list= spots.split(' ')
num_row = int((len(spot_list)-1)/2)+1

plt.figure(figsize = (10, num_row*5))
for i, spot in enumerate(spot_list):
    plt.subplot(num_row, 2, i+1)
    plt.title(spot)
    plt.plot(new_data [spot][spot][start_date:])
    plt.ylim(80,101)
plt.show()


# 서울시내 구별 #
#new_data['서울']['강북구']
spots = '서울 서울,강북 서울,강북구 서울,도봉구 서울,성북구 서울,동작구 서울,강남구 서울,송파구'
spot_list  = spots.split(' ')
num_row = int((len(spot_list)-1)/2)+1
start_date = '2017-1'
plt.figure(figsize = (10, num_row*5))
for i, spot in enumerate(spot_list):
    plt.subplot(num_row, 2, i+1)
    plt.title(spot)
    if ',' in spot:
        si, gu = spot.split(',')
    else:
        si = gu = spot
    plt.plot(new_data[si][gu][start_date:])
    plt.ylim(80,101)
plt.show()

#--------------------------------------------------------------------------------------------#
##### Price Returns #####
#--------------------------------------------------------------------------------------------#
new_data = KBpriceindex_preprocessing(path, data_type)

new_data = new_data['서울'][:] #서울 topdown
diff = ((new_data.loc['2019-06-01'] - new_data.loc['2016-01-01']) / new_data.loc['2016-1-1']*100).dropna()
print("하위 5개")
print(diff.sort_values()[:10])
print(' ')
print("상위 5개")
print(diff.sort_values(ascending=False)[:10])

style.use('ggplot')

fig = plt.figure(figsize = (13,7))
ind = np.arange(5)

ax2= fig.add_subplot(1,3,3)
plt.title(' 2016.1 - 2019.6 가격 변화율 최상위 5')
rects2 = plt.barh(ind, diff.sort_values()[-5:].values, align = 'center', height = 0.5)
plt.yticks(ind, diff.sort_values()[-5:].index)
for i, rect in enumerate(rects2):
    ax2.text(0.95*rect.get_width(), rect.get_y() + rect.get_height()/ 2.0,
            str(round(diff.sort_values()[-5:].values[i],2)) + '%',
            ha = 'right', va = 'center', bbox = dict(boxstyle = "round", fc=(0.5, 0.9, 0.7),
                                                    ec ="0.1"))
ax = fig.add_subplot(1,3,1)
plt.title(' 2016.1 - 2019.6 가격 변화율 최하위 5')
rects = plt.barh(ind, diff.sort_values(ascending = True)[:5].values, align = 'center', height = 0.5)
plt.yticks(ind, diff.sort_values(ascending = True)[:5].index)
for i, rect in enumerate(rects):
    ax.text(0.95*rect.get_width(), rect.get_y() + rect.get_height()/ 2.0,
            str(round(diff.sort_values(ascending = True)[:5].values[i],2)) + '%',
            ha = 'left', va = 'center', bbox = dict(boxstyle = "round", fc=(0.5, 0.9, 0.7),
                                                    ec ="0.1"))

plt.show()


## 2017
new_data = KBpriceindex_preprocessing(path, data_type)

new_data = new_data['서울'][:] #서울 topdown
diff = ((new_data.loc['2019-06-01'] - new_data.loc['2017-01-01']) / new_data.loc['2017-1-1']*100).dropna()
print("하위 5개")
print(diff.sort_values()[:10])
print(' ')
print("상위 5개")
print(diff.sort_values(ascending=False)[:10])

style.use('ggplot')

fig = plt.figure(figsize = (13,7))
ind = np.arange(5)

ax2= fig.add_subplot(1,3,3)
plt.title(' 2017.1 - 2019.6 가격 변화율 최상위 5')
rects2 = plt.barh(ind, diff.sort_values()[-5:].values, align = 'center', height = 0.5)
plt.yticks(ind, diff.sort_values()[-5:].index)
for i, rect in enumerate(rects2):
    ax2.text(0.95*rect.get_width(), rect.get_y() + rect.get_height()/ 2.0,
            str(round(diff.sort_values()[-5:].values[i],2)) + '%',
            ha = 'right', va = 'center', bbox = dict(boxstyle = "round", fc=(0.5, 0.9, 0.7),
                                                    ec ="0.1"))
ax = fig.add_subplot(1,3,1)
plt.title(' 2017.1 - 2019.6 가격 변화율 최하위 5')
rects = plt.barh(ind, diff.sort_values(ascending = True)[:5].values, align = 'center', height = 0.5)
plt.yticks(ind, diff.sort_values(ascending = True)[:5].index)
for i, rect in enumerate(rects):
    ax.text(0.95*rect.get_width(), rect.get_y() + rect.get_height()/ 2.0,
            str(round(diff.sort_values(ascending = True)[:5].values[i],2)) + '%',
            ha = 'left', va = 'center', bbox = dict(boxstyle = "round", fc=(0.5, 0.9, 0.7),
                                                    ec ="0.1"))

plt.show()

## by location ##
new_data = KBpriceindex_preprocessing(path, data_type)

loca = '전국 서울 부산 경기 대구 광주 울산 대전'

temp_list = loca.split(" ")
loca_list = []
for temp in temp_list:
    if ',' in temp:
        temp_split = temp.split(",")
        loca_list.append((temp_split[0], temp_split[1]))
    else:
        loca_list.append((temp,temp))
        
diff = ((new_data.loc['2019-6-1', loca_list] - new_data.loc['2016-1-1', loca_list]) / new_data.loc['2016-1-1', loca_list]*100).sort_values()
num = len(loca_list)
fig = plt.figure(figsize = (13,7))
ind = np.arange(num)

ax = fig.add_subplot(1, 3, 1)
plt.title('2016.1 ~ 2019.6 가격지수 변화율 ')
rects = plt.barh(ind, diff.head(num).values, align = 'center', height = 0.5)
plt.yticks(ind, diff.head(num).index)
for i, rect in enumerate(rects):
    ax.text(0.95*rect.get_width(), rect.get_y() + rect.get_height()/ 2.0,
            str(round(diff.head(20).values[i],2)) + '%',
            ha = 'left', va = 'center', bbox = dict(boxstyle = "round", fc=(0.5, 0.9, 0.7),
                                                    ec ="0.1"))

plt.show()




## by location ##
new_data = KBpriceindex_preprocessing(path, data_type)

loca = '전국 서울 부산 경기 대구 광주 울산 대전'

temp_list = loca.split(" ")
loca_list = []
for temp in temp_list:
    if ',' in temp:
        temp_split = temp.split(",")
        loca_list.append((temp_split[0], temp_split[1]))
    else:
        loca_list.append((temp,temp))
        
diff = ((new_data.loc['2019-6-1', loca_list] - new_data.loc['2016-1-1', loca_list]) / new_data.loc['2016-1-1', loca_list]*100).sort_values()
num = len(loca_list)
fig = plt.figure(figsize = (13,7))
ind = np.arange(num)

ax = fig.add_subplot(1, 3, 1)
plt.title('2016.1 ~ 2019.6 가격지수 변화율 ')
rects = plt.barh(ind, diff.head(num).values, align = 'center', height = 0.5)
plt.yticks(ind, diff.head(num).index)
for i, rect in enumerate(rects):
    ax.text(0.95*rect.get_width(), rect.get_y() + rect.get_height()/ 2.0,
            str(round(diff.head(20).values[i],2)) + '%',
            ha = 'left', va = 'center', bbox = dict(boxstyle = "round", fc=(0.5, 0.9, 0.7),
                                                    ec ="0.1"))

plt.show()


#----------------------------------------------------------------------------------------#
##### personal income #####
#----------------------------------------------------------------------------------------#

import pandas as pd
import xlwings as xw
import matplotlib.pyplot as plt
from matplotlib import font_manager, rc
%matplotlib inline

### Data Cleaning ###

font_name = font_manager.FontProperties(fname = "c:/windows/fonts/malgun.ttf").get_name()
rc('font', family = font_name)
plt.rcParams['axes.unicode_minus'] = False

path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/1906income.xlsx'
raw = pd.read_excel(path, sheet_name = '데이터', index_col =0)
raw.drop(['행정구역(시도)별'], inplace = True)

index_list = raw.index
new_index = []

for temp in index_list:
    if temp[-1] == '시':
        new_index.append(temp[:2])
        
    elif temp[-1] == '도':
        
        if len(temp) == 3:
            new_index.append(temp[:2])
        elif len(temp) == 4:
            new_index.append(temp[0] + temp[2])
        else:
            new_index.append('제주')

    else:
        new_index.append(temp)
raw.index = new_index

income_data = raw.T

def income_preprocessing(path):
    
    raw = pd.read_excel(path)
    raw.drop([0], inplace =True)
    raw.set_index('행정구역(시도)별', inplace = True)
    index_list = raw.index
    
    new_index = []
    
    for temp in index_list:
        if temp[-1] == '시':
            new_index.append(temp[:2])
        elif temp[-1] == '도':
            if len(temp) == 3:
                new_index.append(temp[:2])
            elif len(temp) == 4:
                new_index.append(temp[0] + temp[2])
            else:
                new_index.append('제주')
        else:
            new_index.append(temp)
            
    raw.index = new_index
    income_data = raw.T
    return income_data

income_data_path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/1906income.xlsx'
income_data = income_preprocessing(income_data_path)


path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/1906data.xlsx'
data_type = '매매종합'
price_data = KBpriceindex_preprocessing(path, data_type)

#

location_list = ['전국', '서울', '부산', '대구', '대전', '광주', '경기']
start_year = '2010'
end_year = '2017'

num_row = int((len(location_list)-1)/2)+1
plt.figure(figsize = (12, num_row*5))

for j, location in enumerate(location_list):
    year_data = []
    
    for i in range(int(start_year), int(end_year)+1):
        if location == '제주':
            year_data.append(price_data[location]['서귀포'][str(i)+' .12.1'])
        else:
            year_data.append(price_data[location][location][str(i) +' .12.1'])
            
    temp_df = pd.DataFrame(income_data[location][start_year:])
    temp_df.columns= [location + '소득']
    temp_df[location + '부동산 가격지수'] = year_data
    temp_df['소득 변화율'] = (temp_df[location + '소득'] / temp_df[location + '소득'][0] -1)*100
    temp_df['부동산 가격 지수 변화율'] = (temp_df[location + '부동산 가격지수']/temp_df[location + '부동산 가격지수'][0] -1)*100
    
    plt.subplot(num_row, 2, j+1)
    plt.title(location + ', ' + start_year + ' ~ ' + end_year + '까지')
    plt.plot(temp_df['부동산 가격 지수 변화율'], label = location + '부동산 가격 지수 변화율')
    plt.plot(temp_df['소득 변화율'], label = location + ' 소득 변화율')
    plt.legend()


#----------------------------------------------------------------------------------------------#
###### PIR Analysis ######
#----------------------------------------------------------------------------------------------#
    
path = r'c:/users/shinhyunjin/dropbox/data/KB Real Estate Data/1906data.xlsx'
wb = xw.Book(path)
sheet = wb.sheets['PIR(월별)']
row_num = sheet.range('J2').end('down').row
data_range = 'B2:N' + str(row_num)
pir_rawdata = sheet[data_range].options(pd.DataFrame, index = False, header =True).value

#

big_col = list(pir_rawdata.columns)
big_col[0] = 'index1'
big_col[1] = 'index2'
big_col[2] = 'index3'

for num, col in enumerate(big_col):
    if col == None:
        big_col[num] = big_col[num -1]
    else:
        pass

small_col = list(pir_rawdata.loc[1])
small_col[0] = 'index1'
small_col[1] = 'index2'
small_col[2] = 'index3'

pir_rawdata.columns = [big_col, small_col]
pir_rawdata.drop([0,1], inplace = True)

#

big_index = list(pir_rawdata['index1']['index1'])

for num, index in enumerate(big_index):
    if index is not None:
        if type(index) == str:
            big_index[num] = '20' + index.split(".")[0][1:] + '.' + index.split(".")[1][:2]
        else:
            big_index[num] = big_index[num -1].split(".")[0] + "." + str(int(index))
    else:
        big_index[num] = big_index[num-1]
        

#
        
small_index = list(pir_rawdata['index3']['index3'])
pir_rawdata.index = [pd.to_datetime(big_index), small_index]

del pir_rawdata['index1']
del pir_rawdata['index2']
del pir_rawdata['index3']

pir_rawdata.index.names = ['날짜', '평균주택가격']

gagus = ['1분위', '2분위', '3분위', '4분위', '5분위']
location = '서울 Seoul'
num_row= int((len(gagus)-1)/2)+1

plt.figure(figsize = (10, num_row*5))
for i, gagu in enumerate(gagus):
    plt.subplot(num_row, 2, i+1)
    plt.title(gagu + " 가구의 중간값(3분위) 주택가격 PIR")
    plt.plot(pir_rawdata.xs('3분위', level = '평균주택가격')[location][gagu])
    
plt.show()

####

gagus = ['1분위', '2분위', '3분위', '4분위', '5분위']
location = '서울 Seoul'
house_price_level = '3분위'
num_row = int((len(gagus)-1)/2)+1

plt.figure(figsize = (10, num_row*5))
for i, gagu in enumerate(gagus):
    plt.subplot(num_row, 2, i+1)
    plt.title(gagu + " 가구의 중간값(" + house_price_level + ") 주택가격 PIR")
    plt.plot(pir_rawdata.xs(house_price_level, level = '평균주택가격')[location][gagu])
    indx = pir_rawdata.xs(house_price_level, level = '평균주택가격')[location][gagu].index
    
    long_mean = pir_rawdata.xs(house_price_level, level = '평균주택가격')[location][gagu].mean()
    
    plt.plot(indx, [long_mean for a in range(len(pir_rawdata.xs(house_price_level, level = '평균주택가격')[location][gagu]))])

plt.show()



#------------------------------------------------------------------------------#

