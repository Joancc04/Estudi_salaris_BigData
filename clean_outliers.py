import pandas as pd

# Crear un DataFrame de ejemplo
df = pd.read_csv('Salaries_anual.csv')

# Mostrar el DataFrame original
print("DataFrame original:")
print(df)

# Contar las ocurrencias de cada país
conteo_paises = df['Company Location'].value_counts()

# Filtrar el DataFrame para eliminar países mencionados solo una vez
df_filtrado = df[df['Company Location'].isin(conteo_paises[conteo_paises > 1].index)]

# Mostrar el DataFrame filtrado
print("\nDataFrame filtrado:")
print(df_filtrado)

# Guardar el DataFrame filtrado en un archivo CSV
df_filtrado.to_csv('Salaries_anual_clean.csv', index=False)

print("\nEl DataFrame filtrado ha sido guardado en 'Salaries_anual_clean.csv'.")
print("\nCon los paises siguientes:\n")
paises_unicos = df_filtrado['Company Location'].unique()
print(paises_unicos)
