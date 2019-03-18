import pandas as pd
import numpy as np
raw_data = pd.read_csv('audit_risk.csv')

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

