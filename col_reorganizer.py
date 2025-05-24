import pandas as pd

df = pd.read_csv('cost_of_living.csv')
df = df.iloc[:, [1, 2]]
df.columns = ['country', 'cost_index']
df['cost_index'] = (df['cost_index']/100)*5700 # 5700 ma o meno lo que cuesta 1 mes en NYC por persona
df.to_csv('cost_of_living2.csv', index=False)   # 1700 (cost de vida) + 4000 (alquiler (diavlo))
