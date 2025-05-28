import pandas as pd

df = pd.read_csv('raw_datasets/raw_cost_of_living.csv')
df = df.iloc[:, [1, 2]]
df.columns = ['country', 'cost']

month_cost = 1710 # https://www.numbeo.com/cost-of-living/in/New-York
month_rent = 1736 # https://www.rentcafe.com/average-rent-market-trends/us/
total = month_rent + month_cost

df['cost'] = (df['cost']/100)*total*12
df.to_csv('Cost_of_living_anual.csv', index=False)
