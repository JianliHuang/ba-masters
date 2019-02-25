import numpy as np
import pandas as pd
from sklearn.neighbors import KNeighborsClassifier
from sklearn.linear_model import LinearRegression, Ridge, Lasso
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt

#importing and imputing
df = pd.read_csv('~/documents/ba-masters/spring19/applied-ml/lab3/auto.csv')
df = df.replace('?', np.nan)
df.isna().any()
df.horsepower = pd.to_numeric(df.horsepower)
df['horsepower'] = df['horsepower'].fillna(df.groupby('cylinders')['horsepower'].transform('median'))

#scaling and splitting
X = df
X.drop('car name',axis=1,inplace=True)
y = X.mpg
X.drop('mpg',axis=1,inplace=True)
#X.drop('model year',axis=1,inplace=True)
#need to remove the test size as it was not asked to be set in the instructions for the lab
X_train_org, X_test_org, y_train, y_test = train_test_split(X, y, random_state = 0)#, test_size = 0.2)
scaler = StandardScaler()
X_train = scaler.fit_transform(X_train_org)
X_test = scaler.transform(X_test_org)


#LINEAR REGRESSION
lreg = LinearRegression()
lreg.fit(X_train, y_train)
print(lreg.score(X_train, y_train))
print(lreg.score(X_test, y_test))
lreg.intercept_


#RIDGE
x_range = [0.01, 0.1, 1, 10]
train_score_list = []
test_score_list = []

for alpha in x_range: 
    ridge = Ridge(alpha)
    ridge.fit(X_train,y_train)
    train_score_list.append(ridge.score(X_train,y_train))
    test_score_list.append(ridge.score(X_test, y_test))

plt.plot(x_range, train_score_list, c = 'g', label = 'Train Score')
plt.plot(x_range, test_score_list, c = 'b', label = 'Test Score')
plt.xscale('log')
plt.legend(loc = 3)
plt.xlabel(r'$\alpha$')

#LASSO
x_range = [0.01, 0.1, 1, 10]
train_score_list = []
test_score_list = []

for alpha in x_range:
    lasso = Lasso(alpha)
    lasso.fit(X_train,y_train)
    train_score_list.append(lasso.score(X_train,y_train))
    test_score_list.append(lasso.score(X_test, y_test))


#KNEIGHBORS

train_score_array = []
test_score_array = []

for k in range(1,20):
    knn = KNeighborsClassifier(k)
    knn.fit(X_train, y_train)
    train_score_array.append(knn.score(X_train, y_train))
    test_score_array.append(knn.score(X_test, y_test))