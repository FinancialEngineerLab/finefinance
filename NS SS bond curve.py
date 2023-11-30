# -*- coding: utf-8 -*-
"""
Created on Mon Nov 12 10:12:45 2018

@author: Shinhyunjin
"""

"""
Topic: Nelson-Siegel Curve Smoothing Technique
Sources: Generating a Yield Curve with Nelson-Seigel Method in Excel -> https://www.youtube.com/watch?v=GsZRJmDcDbY
         Data fitting using fmin -> http://glowingpython.blogspot.it/2011/05/curve-fitting-using-fmin.html
         
Comment: Refer to the above video for an explanation of the method.  Also see https://github.com/MaximumBeings/public/blob/master/NSSandNS.txt
         for an MS Excel version of the NSS and NS method.  Though the code works very
         well, we noticed that the Excel version fitted our original data better because the solver in Excel is pretty powerful.  Nonetheless,
         this is a very good implementation of the NSS method.  We try to fit the curve by minimizing the sum of squared errors using the
         NS formula.  It is an iterative method that tries to guess the values for the NS parameters so you need to try different initial
         values.  Feed the program with a set of initial values and observe the curve plot and change the parameters to get better fits. A good
         starting set of initial values is array([0.01,0.01,0.01,1.00]) but can be changed to get a better fit.
"""



import pylab
from numpy import *
from scipy.optimize import fmin
import pandas as pd
import os
import numpy as np
import matplotlib.pyplot as plt


### Nelson - Siegel ###

pre1 = os.path.dirname(os.path.realpath('C:/Users/Shinhyunjin/Dropbox/hw1data.xlsx'))
fname1 = 'hw1data.xlsx'
path1 = os.path.join(pre1, fname1)
df1 = pd.read_excel(path1)
data = pd.DataFrame(df1)


#print ""
# parametric function, x is the independent variable
# and c are the parameters.
# it's a polynomial of degree 2
fp = lambda c, x: c[0]+(c[1]+c[2])*(c[3]/x)*(1-exp(-x/c[3])-c[2]*exp(-x/c[3]))

# error function to minimize
e = lambda p, x, y: ((fp(p,x)-y)**2).sum()


# generating data with noise

y = np.array(data['YTM']/100)  #The periods for which data are available, skip the periods with empty rate
x = np.array(data['TAU'])  #Available rates only

# fitting the data with fmin
p0 = array([0.01,0.01,0.01,1.00])  # initial parameter value
p = fmin(e, p0, args=(x,y))
c = p
j=[]

for h in x:
    j.append(c[0]+(c[1]+c[2])*(c[3]/h)*(1-exp(-h/c[3])-c[2]*exp(-h/c[3])))


#print ""
print ('Initial Parameters: ', p0)
print ('Estimated Parameters: ', p)



#To display interpolated data.
h = x
rateTable = pd.DataFrame.from_items([('Period', h),('NS', j)])
print(rateTable.to_string())

#xx = 
pylab.plot(x,y, 'b')
pylab.plot(x, fp(p,x), 'ro')
pylab.title('Nelson-Siegel')
pylab.show()



#### Svensson ####

# parametric function, x is the independent variable
# and c are the parameters.
# it's a polynomial of degree 2
fp2 = lambda c2, x: (c2[0])+ (c2[1]*((1- exp(-x/c2[4]))/(x/c2[4])))+ (c2[2]*((((1-exp(-x/c2[4]))/(x/c2[4])))- (exp(-x/c2[4]))))+ (c2[3]*((((1-exp(-x/c2[5]))/(x/c2[5])))- (exp(-x/c2[5]))))
real_p = array([0.02,0.01,0.01,0.01,1.00,1.00])

# error function to minimize
e2 = lambda p2, x, y: ((fp2(p2,x)-y)**2).sum()

# generating data with noise

# fitting the data with fmin
p02 = array([0.01,0.01,0.01,0.01,0.01,1.00,1.00])  # initial parameter value
p2 = fmin(e2, p02, args=(x,y))
c2 = p2
j2=[]
for h2 in x:
    j2.append((c2[0])+ (c2[1]*((1- exp(-h2/c2[4]))/(h2/c2[4])))+ (c2[2]*((((1-exp(-h2/c2[4]))/(h2/c2[4])))- (exp(-h2/c2[4]))))+ (c2[3]*((((1-exp(-h2/c2[5]))/(h2/c2[5])))- (exp(-h2/c2[5])))))

print ('Initial Parameters: ', p02)

print ('Estimated Parameters: ', p2)


#To display interpolated data.

h2 = x
rateTable = pd.DataFrame.from_items([('Period', h2),('NSS', j2)])
print(rateTable.to_string())


#xx = array([1,2,5,10,25,30])
plt.title('Result')
plt.grid(True)
plt.plot(x, y, 'g')
plt.plot(x, fp2(p2,x), '--', label = 'SS')
plt.plot(x, fp2(p2,x), 'bo')
plt.plot(x, fp(p,x), 'ro')
plt.plot(x, fp(p,x), ':', label = 'NS')
plt.legend(loc=0)
plt.xlabel('Mat')
plt.ylabel('YTM')

plt.show()

