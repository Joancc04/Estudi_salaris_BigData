import pandas as pd

# Cargar el dataset original
df = pd.read_csv('Salaries_anual_clean.csv')

# Eliminar la columna cost_x si existe
if 'cost_x' in df.columns:
    df.drop(columns=['cost_x'], inplace=True)

# Renombrar cost_y a cost si existe
if 'cost_y' in df.columns:
    df.rename(columns={'cost_y': 'cost'}, inplace=True)

# Guardar el DataFrame limpio
df.to_csv('Salaries_anual_clean.csv', index=False)

print("Archivo limpio guardado como 'Salaries_anual_clean.csv'")