import pandas as pd

# Cargar el archivo CSV
df = pd.read_csv("Salaries.csv")

# Obtener la lista de países únicos (suponiendo que la columna se llama "pais")
paises_unicos = df["Company Location"].unique()

# Convertir a lista si lo necesitas
lista_paises = list(paises_unicos)

print(lista_paises)