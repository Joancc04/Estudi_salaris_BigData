import pandas as pd

main_df = pd.read_csv('raw_datasets/Data_Science_Salaries_big.csv')
main_df = pd.read_csv("Salaries_anual_clean.csv")
cost_df = pd.read_csv('cost_of_living_anual.csv')

# Petit ajustament pels joins que sino no va
# main_df.loc[main_df['Employee Residence'] == 'Viet Nam', 'Employee Residence'] = 'Vietnam'
# main_df.loc[main_df['Employee Residence'] == 'Russian Federation', 'Employee Residence'] = 'Russia'
# main_df.loc[main_df['Company Location'] == 'Viet Nam', 'Company Location'] = 'Vietnam'
# main_df.loc[main_df['Company Location'] == 'Russian Federation', 'Company Location'] = 'Russia'

# main_df.to_csv('Data_Science_Salaries.csv')

## Operació join
df_merged = pd.merge(main_df, cost_df, left_on='Employee Residence', right_on='country', how='left')
df_merged = df_merged.drop(df_merged.columns[-2], axis=1)

df_merged.to_csv('Salaries_anual_clean.csv', index=False)