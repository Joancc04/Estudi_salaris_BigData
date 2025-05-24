# Install required packages if you haven't already
# install.packages(c("dplyr", "readr", "plotly"))

# Load libraries
library(dplyr)
library(readr)
library(plotly)

# Load the CSV file
df <- read_csv("Data_Science_Salaries.csv")

# Group by employee residence and compute mean salary
mean_salary_by_country <- df %>%
  group_by(`Employee Residence`) %>%
  summarise(`Mean Salary (USD)` = mean(`Salary in USD`, na.rm = TRUE)) %>%
  rename(Country = `Employee Residence`)

# Create the choropleth map
fig <- plot_ly(
  data = mean_salary_by_country,
  locations = ~Country,
  locationmode = "country names",
  z = ~`Mean Salary (USD)`,
  type = "choropleth",
  colorscale = "Viridis"
) %>%
  layout(
    title = "Mean Data Science Salaries by Country",
    geo = list(showframe = FALSE, showcoastlines = FALSE)
  )

fig


# num data for each row
country_counts <- df %>%
  group_by(`Employee Residence`) %>%
  summarise(`Row Count` = n()) %>%
  arrange(desc(`Row Count`))

# Show countries with fewer than 10 rows
low_data_table <- country_counts %>%
  filter(`Row Count` < 10)









