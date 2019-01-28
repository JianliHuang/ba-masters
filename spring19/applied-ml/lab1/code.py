import pandas as pd
import numpy as np

df1 = pd.read_csv('adult.csv')
df2 = pd.read_csv('auto.csv')

df1 = df1.replace(' ?', np.nan)
df2 = df2.replace('?', np.nan)

df1.isna().any()
df2.isna().any()

df1_us = df1[df1['native-country']==' United-States']
df1_us_agg = df1_us.group_by('Salary')

df2 = df2.dropna(how='any',axis=0)
x = df2.groupby('cylinders')
