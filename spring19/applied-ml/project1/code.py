import pandas as pd
import numpy as np
#raw_data = pd.read_csv('audit_risk.csv')
d1 = pd.read_csv('audit_risk.csv')
d2 = pd.read_csv('trial.csv')

#merging datasets
print(d1.shape)
print(d2.shape)
print(list(d1),'\n')
print(list(d2))

print('Sector_score: ',sum(d1['Sector_score']==d2['Sector_score']))
print('LOCATION_ID: ',sum(d1['LOCATION_ID']==d2['LOCATION_ID']))
print('PARA_A: ',sum(d1['PARA_A']==d2['PARA_A']))
print('Score_A: ',sum(d1['Score_A']==d2['SCORE_A']))
print('PARA_B: ',sum(d1['PARA_B']==d2['PARA_B']))
print('Score_B: ',sum(d1['Score_B']==d2['SCORE_B']))
print('TOTAL: ',sum(d1['TOTAL']==d2['TOTAL']))
print('numbers: ',sum(d1['numbers']==d2['numbers']))
print('Money_Value: ',sum(d1['Money_Value']==d2['Money_Value']))
print('History: ',sum(d1['History']==d2['History']))
print('Score: ',sum(d1['Score']==d2['Score']))
print('Risk: ',sum(d1['Risk']==d2['Risk']))

d2.drop(['Sector_score','LOCATION_ID','PARA_A','PARA_B','TOTAL','numbers','History','Score'],axis=1)




raw_data = pd.concat([d1,d2],axis=1)

## Data Preview
raw_data.head()
raw_data.shape
raw_data.describe()
raw_data.isnull().values.any()
a = np.array(raw_data.isnull().sum())
np.where(a > 0)
raw_data[raw_data.columns[12]].describe(include='all')
raw_data[raw_data.columns[12]].isnull().sum()
raw_data.shape
raw_data = raw_data.dropna()
raw_data.shape
raw_data[pd.to_numeric(raw_data['LOCATION_ID'], errors='coerce').isnull()]
raw_data = raw_data[pd.to_numeric(raw_data['LOCATION_ID'], errors='coerce').notnull()]
list(raw_data)

## Creating test and control groups
x = raw_data.drop(['Audit_Risk','Risk'],axis=1)
y_reg = raw_data['Audit_Risk']
y_class = raw_data['Risk']

from sklearn.model_selection import train_test_split

# default is 75% / 25% train-test split
x_train_reg, x_test_reg, y_train_reg, y_test_reg = train_test_split(x, y_reg, random_state=0)
x_train_class, x_test_class, y_train_class, y_test_class = train_test_split(x, y_class, random_state=0)

from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()

x_train_reg = scaler.fit_transform(x_train_reg)
x_test_reg = scaler.transform(x_test_reg)

x_train_class = scaler.fit_transform(x_train_class)
x_test_class = scaler.transform(x_test_class)

#Regression Analysis

