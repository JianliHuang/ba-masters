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

from sklearn.neighbors import KNeighborsClassifier
from sklearn.model_selection import GridSearchCV

knn = KNeighborsClassifier()
param_grid = {'n_neighbors':[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15]}
grid_search = GridSearchCV(knn, param_grid, scoring='recall_weighted', return_train_score=True)
grid_search.fit(x_train_class, y_train_class.values.ravel())


print(grid_search.best_params_)
print(grid_search.best_score_)
results = pd.DataFrame(grid_search.cv_results_)
results

knn = KNeighborsClassifier(n_neighbors = 1)
knn.fit(x_train_class, y_train_class)

from sklearn.metrics import recall_score, accuracy_score

y_train_predict = knn.predict(x_train_class)
print('Accuracy of Training: {:.2f}'.format(accuracy_score(y_train_class, y_train_predict)))
y_predict = knn.predict(x_test_class)
print('Accuracy: {:.2f}'.format(accuracy_score(y_test_class, y_predict)))
print('Recall: {:.5f}'.format(recall_score(y_test_class, y_predict, average="weighted")))


from sklearn.metrics import classification_report

print(classification_report(y_test_class, y_predict))