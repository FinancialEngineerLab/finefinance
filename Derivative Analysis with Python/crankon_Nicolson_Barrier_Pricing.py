# -*- coding: utf-8 -*-
"""
Created on Sun Apr 28 15:10:11 2019

@author: Shinhyunjin
"""

#### Fnbbbbbbbbbbbinancial Engineering Project ####
### Prof. Martin ###
### Author : Shin Hyunjin and Ko KyeongHyeong (UIUC Financial Engineering)
### Crank-Nicolson Methods for MLS Pricing ###

#### AutoCallabe Contingent Interest Notes ####

#importing packages

import datetime
from datetime import date
import pandas as pd
import numpy as np

#setting variables

r = 0.0249574
sigma = 0.246
d = 0.01844
initial_val = 195.09
trigger_b = 0.78*initial_val
SU = 2.5 * initial_val
SL = 0
issue_date = date(2019, 3, 26)
maturity_date = date(2020, 6, 25)
delta_day = maturity_date - issue_date
imax = delta_day.days
delta_t = 1 / imax
print(imax)
jmax = 250
delta_S = (SU - SL) / jmax
date_list = [issue_date + datetime.timedelta(days=x) for x in range(0,imax+1)]
review_date_index = [date_list.index(datetime.date(2019, 6, 21)), date_list.index(datetime.date(2019, 9, 23)), date_list.index(datetime.date(2019, 12, 23)), date_list.index(datetime.date(2020, 3, 23)), date_list.index(datetime.date(2020, 6, 22))]
payment_date_index = [date_list.index(datetime.date(2019, 6, 26)), date_list.index(datetime.date(2019, 9, 26)), date_list.index(datetime.date(2019, 12, 27)), date_list.index(datetime.date(2020, 3, 26)), date_list.index(datetime.date(2020, 6, 25))]

#setting grids

S_grid = pd.DataFrame(np.zeros(shape=(jmax+1,imax+1)))

for i in range(0,imax+1):
    for j in range(0,jmax+1):
        S_grid[i][j] = SL + j*delta_S

V_grid_triggered = pd.DataFrame(np.zeros(shape=(jmax+1,imax+1)))
V_grid_total = pd.DataFrame(np.zeros(shape=(jmax+1,imax+1)))

#deriving trigger boundary

for i in range(0,imax+1):
    if i <= payment_date_index[1]:
        V_grid_triggered[i][jmax] = 1000 * np.exp(-r*(payment_date_index[1]-i)/imax)
    elif i <=  payment_date_index[2]:
        V_grid_triggered[i][jmax] = 1000 * np.exp(-r*(payment_date_index[2]-i)/imax)
    elif i <=  payment_date_index[3]:
        V_grid_triggered[i][jmax] = 1000 * np.exp(-r*(payment_date_index[3]-i)/imax)
    else:
        V_grid_triggered[i][jmax] = 1000 * np.exp(-r*(payment_date_index[4]-i)/imax)

for i in range(0,jmax+1):
    if S_grid[imax][i] >= initial_val:
        V_grid_triggered[imax][i] = 1000
    elif S_grid[imax][i] >= trigger_b:
        V_grid_triggered[imax][i] = 1000 * S_grid[imax][i]/initial_val
    else:
        V_grid_triggered[imax][i] = 1000 * S_grid[imax][i]/initial_val

a_standard = np.zeros(251)
for i in range(1, 251):
    a_standard[i] = 1 / 4 * ((sigma ** 2) * (i ** 2) - (r - d) * i)
a_standard[250] = 0

b_standard = np.zeros(251)
for i in range(0, 251):
    b_standard[i] = -(sigma ** 2) * (i ** 2) / 2 - (r - d) / 2 - 1 / delta_t
b_standard[0] = 1
b_standard[250] = 1

c_standard = np.zeros(251)
for i in range(0, 250):
    c_standard[i] = 1 / 4 * ((sigma ** 2) * (i ** 2) + (r - d) * i)
c_standard[0] = 0

for i in range(imax-1,-1,-1):
    if i in review_date_index[1:4]:

        a = np.copy(a_standard)
        a[100] = 0
        b = np.copy(b_standard)
        b[100] = 1
        c = np.copy(c_standard)

        alpha = np.zeros(101)
        alpha[0] = b[0]
        for j in range(1, 101):
            alpha[j] = b[j] - (a[j] * c[j - 1]) / alpha[j - 1]

        d = np.zeros(101)
        d[0] = V_grid_triggered[i][0]
        for j in range(1, 100):
            d[j] = - a[j] * V_grid_triggered[i + 1][j - 1] - (-(sigma ** 2) * (j ** 2) / 2 - r / 2 + 1 / delta_t) * \
                   V_grid_triggered[i + 1][j] - c[j] * V_grid_triggered[i + 1][j + 1]
        d[100] = V_grid_triggered[i][250]

        S = np.zeros(101)
        S[0] = d[0]
        for j in range(1, 101):
            S[j] = d[j] - a[j] / alpha[j - 1] * S[j - 1]

        for j in range(101,251):
            V_grid_triggered[i][j] = 1000 * np.exp(-r*(payment_date_index[review_date_index.index(i)]-i)/imax)
        V_grid_triggered[i][100] = S[100] / alpha[100]
        for j in range(99, -1, -1):
            V_grid_triggered[i][j] = 1 / alpha[j] * (S[j] - c[j] * V_grid_triggered[i][j + 1])

    else:

        a = np.copy(a_standard)
        b = np.copy(b_standard)
        c = np.copy(c_standard)

        alpha = np.zeros(251)
        alpha[0] = b[0]
        for j in range(1, 251):
            alpha[j] = b[j] - (a[j] * c[j - 1]) / alpha[j - 1]

        d = np.zeros(251)
        d[0] = V_grid_triggered[i][0]
        for j in range(1, 250):
            d[j] = - a[j] * V_grid_triggered[i + 1][j - 1] - (-(sigma ** 2) * (j ** 2) / 2 - r / 2 + 1 / delta_t) * \
                   V_grid_triggered[i + 1][j] - c[j] * V_grid_triggered[i + 1][j + 1]
        d[250] = V_grid_triggered[i][250]

        S = np.zeros(251)
        S[0] = d[0]
        for j in range(1, 251):
            S[j] = d[j] - a[j] / alpha[j - 1] * S[j - 1]

        V_grid_triggered[i][250] = S[250] / alpha[250]
        for j in range(249, -1, -1):
            V_grid_triggered[i][j] = 1 / alpha[j] * (S[j] - c[j] * V_grid_triggered[i][j + 1])

#Redo LU Decomposition with j = 78 as lower boundary including interests

V_grid_total.iloc[78] = V_grid_triggered.iloc[78]

for i in range(0,imax+1):
    if i <= payment_date_index[1]:
        V_grid_total[i][jmax] = (1000 + 20.375) * np.exp(-r*(payment_date_index[1]-i)/imax)
    elif i <=  payment_date_index[2]:
        V_grid_total[i][jmax] = (1000 + 20.375) * np.exp(-r*(payment_date_index[2]-i)/imax)
    elif i <=  payment_date_index[3]:
        V_grid_total[i][jmax] = (1000 + 20.375) * np.exp(-r*(payment_date_index[3]-i)/imax)
    else:
        V_grid_total[i][jmax] = 1000 * np.exp(-r*(payment_date_index[4]-i)/imax)

for i in range(0,jmax):
    if S_grid[imax][i] >= initial_val:
        V_grid_total[imax][i] = 1000
    elif S_grid[imax][i] >= trigger_b:
        V_grid_total[imax][i] = 1000
    else:
        V_grid_total[imax][i] = 1000 * S_grid[imax][i] / initial_val

for i in range(imax-1,-1,-1):
    if i in review_date_index[1:4]:

        a = np.copy(a_standard)
        a[100] = 0
        b = np.copy(b_standard)
        b[100] = 1
        c = np.copy(c_standard)

        alpha = np.zeros(101)
        alpha[0] = b[0]
        for j in range(1, 101):
            alpha[j] = b[j] - (a[j] * c[j - 1]) / alpha[j - 1]

        d = np.zeros(101)
        d[0] = V_grid_triggered[i][0]
        for j in range(1, 100):
            d[j] = - a[j] * V_grid_triggered[i + 1][j - 1] - (-(sigma ** 2) * (j ** 2) / 2 - r / 2 + 1 / delta_t) * \
                   V_grid_triggered[i + 1][j] - c[j] * V_grid_triggered[i + 1][j + 1]
        d[100] = V_grid_triggered[i][250]

        S = np.zeros(101)
        S[0] = d[0]
        for j in range(1, 101):
            S[j] = d[j] - a[j] / alpha[j - 1] * S[j - 1]

        for j in range(101,251):
            V_grid_triggered[i][j] = 1000 * np.exp(-r*(payment_date_index[review_date_index.index(i)]-i)/imax)
        V_grid_triggered[i][100] = S[100] / alpha[100]
        for j in range(99, -1, -1):
            V_grid_triggered[i][j] = 1 / alpha[j] * (S[j] - c[j] * V_grid_triggered[i][j + 1])
        for j in range(78,251):
            V_grid_triggered[i][j] += 20.375 * np.exp(-r*(payment_date_index[review_date_index.index(i)]-i)/imax)
    elif i == review_date_index[0] or i == review_date_index[4]:

        a = np.copy(a_standard)
        b = np.copy(b_standard)
        c = np.copy(c_standard)

        alpha = np.zeros(251)
        alpha[0] = b[0]
        for j in range(1, 251):
            alpha[j] = b[j] - (a[j] * c[j - 1]) / alpha[j - 1]

        d = np.zeros(251)
        d[0] = V_grid_triggered[i][0]
        for j in range(1, 250):
            d[j] = - a[j] * V_grid_triggered[i + 1][j - 1] - (-(sigma ** 2) * (j ** 2) / 2 - r / 2 + 1 / delta_t) * \
                   V_grid_triggered[i + 1][j] - c[j] * V_grid_triggered[i + 1][j + 1]
        d[250] = V_grid_triggered[i][250]

        S = np.zeros(251)
        S[0] = d[0]
        for j in range(1, 251):
            S[j] = d[j] - a[j] / alpha[j - 1] * S[j - 1]

        V_grid_triggered[i][250] = S[250] / alpha[250]
        for j in range(249, -1, -1):
            V_grid_triggered[i][j] = 1 / alpha[j] * (S[j] - c[j] * V_grid_triggered[i][j + 1])
        for j in range(78,251):
            V_grid_triggered[i][j] += 20.375 * np.exp(-r*(payment_date_index[review_date_index.index(i)]-i)/imax)

    else:
        a = np.copy(a_standard)
        b = np.copy(b_standard)
        c = np.copy(c_standard)

        alpha = np.zeros(251)
        alpha[0] = b[0]
        for j in range(1, 251):
            alpha[j] = b[j] - (a[j] * c[j - 1]) / alpha[j - 1]

        d = np.zeros(251)
        d[0] = V_grid_triggered[i][0]
        for j in range(1, 250):
            d[j] = - a[j] * V_grid_triggered[i + 1][j - 1] - (-(sigma ** 2) * (j ** 2) / 2 - r / 2 + 1 / delta_t) * \
                   V_grid_triggered[i + 1][j] - c[j] * V_grid_triggered[i + 1][j + 1]
        d[250] = V_grid_triggered[i][250]

        S = np.zeros(251)
        S[0] = d[0]
        for j in range(1, 251):
            S[j] = d[j] - a[j] / alpha[j - 1] * S[j - 1]

        V_grid_triggered[i][250] = S[250] / alpha[250]
        for j in range(249, -1, -1):
            V_grid_triggered[i][j] = 1 / alpha[j] * (S[j] - c[j] * V_grid_triggered[i][j + 1])

print(V_grid_triggered[0][100]*np.exp(-r*5/imax))

