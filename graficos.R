# Cargar librerías necesarias
library(ggplot2)
library(dplyr)
library(readr)

# Leer el dataset
df <- read_csv("Data_Science_Salaries.csv")

# 1. Boxplot: Salario vs Nivel de experiencia
ggplot(df, aes(x = "Experience Level", y = `Salary in USD`, fill = "Experience Level")) +
  geom_boxplot() +
  scale_y_log10() +
  labs(title = "Distribución del salario por nivel de experiencia",
       y = "Salario (USD, escala log)",
       x = "Nivel de experiencia") +
  theme_minimal()

# 2. Mapa de calor: Media salarial por país y tamaño de empresa
df %>%
  group_by(`Company Location`, `Company Size`) %>%
  summarise(Salario_Medio = mean(`Salary in USD`, na.rm = TRUE)) %>%
  ggplot(aes(x = `Company Location`, y = `Company Size`, fill = Salario_Medio)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Salario medio según país y tamaño de empresa",
       x = "País de la empresa",
       y = "Tamaño de empresa") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 3. Gráfico de barras: Salario medio por título de trabajo (top 10)
df %>%
  group_by(`Job Title`) %>%
  summarise(Salario_Medio = mean(`Salary in USD`, na.rm = TRUE)) %>%
  arrange(desc(Salario_Medio)) %>%
  slice(1:10) %>%
  ggplot(aes(x = reorder(`Job Title`, Salario_Medio), y = Salario_Medio, fill = Salario_Medio)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 trabajos mejor pagados",
       y = "Salario medio (USD)", x = "Título de trabajo") +
  theme_minimal()

# 4. Scatter: Salario relativo vs Nivel de experiencia (codificado)
ggplot(df, aes(x = `Experience Level Code`, y = `Salary Relative`)) +
  geom_jitter(width = 0.2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Relación entre experiencia y salario relativo",
       x = "Nivel de experiencia (codificado)", y = "Salario relativo") +
  theme_minimal()
