# -*- coding: utf-8 -*-
"""
Created on Thu Nov  8 09:33:45 2018

@author: Shinhyunjin
"""

#### 파이썬을 활용한 금융시계열분석 ####

## 1. ARIMA ##

import pandas as pd
import matplotlib.pyplot as plt
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.tsa.arima_model import ARIMA

series = pd.read_csv('C:/Users/Shinhyunjin/Dropbox/market-price.csv', header = 0, index_col = 0, squeeze = True)
series.plot()


plot_acf(series)
plot_pacf(series)
plt.show

#1차차분
diff_1 = series.diff(periods = 1).iloc[1:]
diff_1.plot()
plot_acf(diff_1)
plot_pacf(diff_1)
plt.show()

# Modeling

model = ARIMA(series, order=(0,1,1))  #ARIMA(0,1,1)
model_fit = model.fit(trend='c', full_output=True, disp=1)
print(model_fit.summary())

# Forecasting

model_fit.plot_predict()

#예측값(stderr, upper bound, lower bound)
fore = model_fit.forecast(steps=1)
print(fore)

