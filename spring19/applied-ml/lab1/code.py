#read in req libs
import pandas as pd

#read in datasets
df1 = pd.read_csv('adult.csv')
df2 = pd.read_csv('auto.csv')

#replace null fields with nan
df1 = df1.replace(' ?', np.nan)
df2 = df2.replace('?', np.nan)

#check and display nan containing columns
df1.isna().any()
df2.isna().any()

#dataset1
#subset for united states and find count of diff salary groups
df1_us = df1[df1['native-country']==' United-States']
df1_us_agg = df1_us.groupby('Salary')
df1_us_agg['Salary'].count()

#dataset2
#remove rows with nan horsepower and set horsepower to float
df2t = df2[np.isnan(df2['horsepower'])]
#agg by cylinders and find average horsepower of 4 and 6 cyl cars
df2t_agg = df2t.groupby('cylinders')
df2t_agg['horsepower'].mean()
