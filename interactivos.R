# Instalar si no tienes las librerías
# install.packages(c("plotly", "dplyr", "readr"))

library(plotly)
library(dplyr)
library(readr)

# Leer datos
df <- read_csv("Data_Science_Salaries.csv")

# 1. Gráfico interactivo: Salario vs nivel de experiencia
p1 <- df %>%
  plot_ly(
    x = ~`Experience Level`,
    y = ~`Salary in USD`,
    type = 'box',
    color = ~`Experience Level`
  ) %>%
  layout(title = "Salario por nivel de experiencia (interactivo)",
         yaxis = list(title = "Salario (USD)"))

# 2. Salario medio por título de trabajo (Top 10)
top_jobs <- df %>%
  group_by(`Job Title`) %>%
  summarise(Salario_Medio = mean(`Salary in USD`, na.rm = TRUE)) %>%
  arrange(desc(Salario_Medio)) %>%
  slice(1:10)

p2 <- plot_ly(top_jobs, 
              x = ~Salario_Medio, 
              y = ~reorder(`Job Title`, Salario_Medio), 
              type = 'bar', 
              orientation = 'h') %>%
  layout(title = "Top 10 trabajos mejor pagados",
         xaxis = list(title = "Salario medio (USD)"),
         yaxis = list(title = "Título de trabajo"))

# Mostrar ambos
p1
p2

# Qué variables tienen mayor impacto en el salario
# Cuánto cambia el salario según pasar de "Entry" a "Senior"
# Si el país de residencia influye más que el tipo de empresa
modelo <- lm(`Salary in USD` ~ `Experience Level` + `Company Size` + `Employee Residence` + `Job Title`, data = df)
summary(modelo)

# ¿La experiencia paga más en empresas grandes que en pequeñas?
# ¿Hay países donde ser "Senior" no mejora tanto el salario
modelo_interaccion <- lm(`Salary in USD` ~ `Experience Level` * `Company Size`, data = df)
summary(modelo_interaccion)


library(cluster)

df_cluster <- df %>%
  select(`Experience Level Code`, `Salary in USD`, `Company Size`) %>%
  na.omit()

kmeans_result <- kmeans(scale(df_cluster), centers = 4)

df$cluster <- as.factor(kmeans_result$cluster)

ggplot(df, aes(x = `Experience Level Code`, y = `Salary in USD`, color = cluster)) +
  geom_point(alpha = 0.5) +
  labs(title = "Cluster de perfils professionals")


library(rpart)
modelo_arbol <- rpart(`Salary in USD` ~ `Experience Level` + `Company Size` + `Employee Residence`, data = df)
plot(modelo_arbol)
text(modelo_arbol, pretty = 0)
