# Instalar si no tienes estos paquetes
# install.packages(c("shiny", "dplyr", "ggplot2", "readr", "plotly", "DT"))

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(DT)

# Cargar el dataset
df <- read_csv("Data_Science_Salaries.csv")

# UI - Interfaz de usuario
ui <- fluidPage(
  titlePanel("Explorador de Salarios en el Sector de les Dades"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("experience", "Nivell d'experiència:",
                  choices = unique(df$`Experience Level`),
                  selected = "Mid"),
      
      selectInput("company_size", "Mida de l'empresa:",
                  choices = unique(df$`Company Size`),
                  selected = "Medium")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Gràfic Salarial per País",
                 plotlyOutput("salary_plot")),
        
        tabPanel("Top 10 Treballs Millor Pagats",
                 plotlyOutput("title_plot")),
        
        tabPanel("Estadístiques Resum",
                 dataTableOutput("summary_table")),
        
        tabPanel("Recomanacions",
                 h4("Millors països segons el teu perfil:"),
                 dataTableOutput("recom_countries"),
                 
                 h4("Títol de treball millor pagat:"),
                 verbatimTextOutput("recom_title"),
                 
                 h4("Recomanació personalitzada:"),
                 textOutput("recom_text")
        )
        
      )
    )
  )
)

# SERVER - Lògica de la app
server <- function(input, output) {
  
  # Filtrado reactivo de dades
  filtered_data <- reactive({
    df %>%
      filter(`Experience Level` == input$experience,
             `Company Size` == input$company_size)
  })
  
  # Gráfico interactivo por país
  output$salary_plot <- renderPlotly({
    filtered_data() %>%
      group_by(`Company Location`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      plot_ly(
        x = ~Salari_Mig,
        y = ~reorder(`Company Location`, Salari_Mig),
        type = "bar",
        orientation = "h",
        marker = list(color = "steelblue")
      ) %>%
      layout(
        title = "Salari mitjà per país",
        xaxis = list(title = "Salari mitjà (USD)"),
        yaxis = list(title = "País")
      )
  })
  
  # Gráfico top 10 trabajos mejor pagados
  output$title_plot <- renderPlotly({
    filtered_data() %>%
      group_by(`Job Title`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      slice_max(order_by = Salari_Mig, n = 10) %>%
      plot_ly(
        x = ~Salari_Mig,
        y = ~reorder(`Job Title`, Salari_Mig),
        type = "bar",
        orientation = "h",
        marker = list(color = "darkorange")
      ) %>%
      layout(
        title = "Top 10 treballs millor pagats (segons filtre)",
        xaxis = list(title = "Salari mitjà (USD)"),
        yaxis = list(title = "Títol del lloc de treball")
      )
  })
  
  # Tabla resumen de estadísticas
  output$summary_table <- renderDataTable({
    filtered_data() %>%
      summarise(
        Observacions = n(),
        Salari_Mitja = round(mean(`Salary in USD`), 2),
        Salari_Màxim = max(`Salary in USD`),
        Salari_Mínim = min(`Salary in USD`)
      )
  })
  
  # Mejores países recomendados
  output$recom_countries <- renderDataTable({
    filtered_data() %>%
      group_by(`Company Location`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      arrange(desc(Salari_Mig)) %>%
      slice_head(n = 3)
  })
  
  # Mejor trabajo
  output$recom_title <- renderPrint({
    best_title <- filtered_data() %>%
      group_by(`Job Title`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      arrange(desc(Salari_Mig)) %>%
      slice_head(n = 1) %>%
      pull(`Job Title`)
    
    best_title
  })
  
  # Recomendación personalizada
  output$recom_text <- renderText({
    exp <- input$experience
    size <- input$company_size
    
    if (exp == "Entry") {
      if (size == "Small") {
        return("Començant a una empresa petita et permetrà adquirir coneixements amplis i responsabilitats ràpidament.")
      } else {
        return("Com a júnior en una empresa mitjana o gran, podràs aprendre d’equips grans i estabilitzar la teva carrera.")
      }
    } else if (exp == "Mid") {
      if (size == "Large") {
        return("Amb experiència mitjana i en una empresa gran, és un bon moment per escalar posicions i negociar el teu salari.")
      } else {
        return("Empreses petites o mitjanes et poden oferir flexibilitat i lideratge a mitjà termini.")
      }
    } else if (exp == "Senior") {
      return("Amb experiència alta, busca països amb alts salaris i empreses grans que valorin la teva trajectòria.")
    } else {
      return("Explora opcions globals i aprofita la teva experiència per aconseguir projectes d'alt impacte.")
    }
  })
  
}

# Llançar la aplicació
shinyApp(ui = ui, server = server)
