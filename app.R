# Versión 3 - Explorador de Salarios con diseño profesional

# Instalar si no tienes estos paquetes
# install.packages(c("shiny", "dplyr", "ggplot2", "readr", "plotly", "DT", "scales", "bslib"))

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(DT)
library(scales)
library(bslib)

# Cargar el dataset
df <- read_csv("Data_Science_Salaries.csv")

# Añadir columna de región y teletrabajo
df <- df %>% mutate(
  Region = case_when(
    `Company Location` %in% c('United States', 'Canada', 'Mexico', 'Puerto Rico') ~ "América del Norte",
    `Company Location` %in% c('Colombia', 'Brazil', 'Argentina') ~ "América del Sur",
    `Company Location` %in% c('United Kingdom', 'Portugal', 'Ireland', 'Germany', 'Spain', 'Poland', 'France', 'Netherlands', 'Luxembourg', 'Gibraltar', 'Ukraine', 'Slovenia', 'Greece', 'Latvia', 'Italy', 'Estonia', 'Czechia', 'Switzerland', 'Russia', 'Denmark', 'Sweden', 'Finland', 'Austria', 'Belgium', 'Romania') ~ "Europa",
    `Company Location` %in% c('India', 'Vietnam', 'Philippines', 'Turkey', 'Japan', 'Singapore', 'Pakistan', 'Indonesia', 'Malaysia', 'Israel') ~ "Asia",
    `Company Location` %in% c('South Africa', 'Nigeria') ~ "África",
    `Company Location` %in% c('Australia', 'American Samoa') ~ "Oceanía",
    TRUE ~ "Altres"
  ),
  Teletrabajo = if_else(`Company Location` == `Employee Residence`, "Presencial", "Remot")
)

# UI mejorado con layout moderno
theme_config <- bs_theme(bootswatch = "flatly", base_font = font_google("Roboto"))

ui <- fluidPage(
  theme = theme_config,
  
  div(
    class = "container",
    br(),
    div(
      style = "text-align: center;",
      h1("\U1F4BC Explorador de Salarios en el Sector de les Dades", style = "font-weight: 700;"),
      p("Una aplicació interactiva per analitzar salaris globals en el sector de la ciència de dades.", style = "font-size: 18px;")
    ),
    br(),
    
    fluidRow(
      column(
        width = 3,
        wellPanel(
          h4("\U1F50D Filtres", style = "font-weight: bold;"),
          selectInput("experience", "Nivell d'experiència:",
                      choices = c("Qualsevol", "Entry", "Mid", "Senior", "Executive"), 
                      selected = "Qualsevol"),
          selectInput("company_size", "Mida de l'empresa:",
                      choices = c("Qualsevol", "Small", "Medium", "Large"), 
                      selected = "Qualsevol"),
          selectInput("region", "Regió (opcional):",
                      choices = c("Totes", sort(unique(df$Region))), selected = "Totes"),
          selectInput("remote", "Modalitat de treball:",
                      choices = c("Qualsevol", "Presencial", "Remot"), selected = "Qualsevol"),
          br(),
          downloadButton("download_data", "Descarregar dades filtrades", class = "btn btn-success")
        )
      ),
      
      column(
        width = 9,
        tabsetPanel(
          tabPanel(tagList(icon("info-circle"), span("Inici")),
                   br(),
                   h3("Benvingut/da al panell interactiu de salaris!"),
                   p("Aquesta aplicació t'ajuda a explorar les dades salarials globals per a professionals del món de la ciència de dades.", style = "font-size: 16px;"),
                   p("Utilitza els filtres de l'esquerra per veure les estadístiques personalitzades.", style = "font-size: 16px;")),
          tabPanel(tagList(icon("calculator"), span("Simulador de salari")),
                   br(),
                   h4("Calcula una estimació del teu salari"),
                   p("Aquesta estimació es basa en els filtres seleccionats a l'esquerra."),
                   br(),
                   h4("Salari estimat:"),
                   verbatimTextOutput("simulated_salary")
          ),
          tabPanel(tagList(icon("chart-bar"), span("Salari mitjà per país")),
                   br(),
                   plotlyOutput("salary_plot", height = "800px")),
          
          tabPanel(tagList(icon("dollar-sign"), span("Treballs millor pagats")),
                   br(),
                   plotlyOutput("title_plot", height = "800px")),
          
          tabPanel(tagList(icon("chart-area"), span("Distribució salarial")),
                   br(),
                   plotlyOutput("dist_plot", height = "800px")),
          
          tabPanel(tagList(icon("balance-scale"), span("Comparació global")),
                   br(),
                   plotlyOutput("comparativa_plot", height = "800px")),
          
          tabPanel(tagList(icon("pie-chart"), span("Remot vs Presencial")),
                   br(),
                   plotlyOutput("remote_plot", height = "400px")),
          
          tabPanel(tagList(icon("table"), span("Estadístiques resum")),
                   br(),
                   dataTableOutput("summary_table")),
          
          tabPanel(tagList(icon("lightbulb"), span("Recomanacions")),
                   br(),
                   h4("Millors països segons el teu perfil:"),
                   dataTableOutput("recom_countries"),
                   h4("Títol de treball millor pagat:"),
                   verbatimTextOutput("recom_title"),
                   h4("Recomanació personalitzada:"),
                   textOutput("recom_text")
          )
        )
      )
    ),
    br()
  )
)

server <- function(input, output) {
  
  # Filtrado reactivo de dades amb region i modalitat de treball
  filtered_data <- reactive({
    data <- df %>%
      filter((input$experience == "Qualsevol" | `Experience Level` == input$experience),
             (input$company_size == "Qualsevol" | `Company Size` == input$company_size))
    
    if (input$region != "Totes") {
      data <- data %>% filter(Region == input$region)
    }
    
    if (input$remote != "Qualsevol") {
      data <- data %>% filter(Teletrabajo == input$remote)
    }
    
    data
  })
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste("salarios_filtrados.csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$salary_plot <- renderPlotly({
    filtered_data() %>%
      group_by(`Company Location`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      arrange(desc(Salari_Mig)) %>%
      plot_ly(
        y = ~reorder(`Company Location`, Salari_Mig),
        x = ~Salari_Mig,
        type = 'bar',
        orientation = 'h',
        marker = list(color = 'rgba(38, 166, 154, 0.8)',
                      line = list(color = 'rgba(38, 166, 154, 1.0)', width = 1)),
        text = ~paste0("$", formatC(Salari_Mig, format = "f", big.mark = ",", digits = 0)),
        hoverinfo = 'text+y'
      ) %>%
      layout(
        title = list(text = "Salari mitjà per país", font = list(size = 20)),
        xaxis = list(title = "Salari mitjà (USD)", tickfont = list(size = 12)),
        yaxis = list(title = "País", tickfont = list(size = 10)),
        margin = list(t = 30, l = 140)
      )
  })
  
  output$title_plot <- renderPlotly({
    filtered_data() %>%
      group_by(`Job Title`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      slice_max(order_by = Salari_Mig, n = 10) %>%
      arrange(Salari_Mig) %>%
      plot_ly(
        y = ~reorder(`Job Title`, Salari_Mig),
        x = ~Salari_Mig,
        type = 'bar',
        orientation = 'h',
        marker = list(color = 'rgba(255, 140, 0, 0.8)',
                      line = list(color = 'rgba(255, 140, 0, 1.0)', width = 1)),
        text = ~paste0("$", formatC(Salari_Mig, format = "f", big.mark = ",", digits = 0)),
        hoverinfo = 'text+y'
      ) %>%
      layout(
        title = list(text = "Top 10 feines més ben pagades (segons filtre)", font = list(size = 20)),
        xaxis = list(title = "Salari mitjà (USD)", tickfont = list(size = 12)),
        yaxis = list(title = "Títol del lloc de treball", tickfont = list(size = 12)),
        margin = list(t = 30)
      )
  })
  
  output$dist_plot <- renderPlotly({
    plot_ly(
      filtered_data(),
      x = ~`Salary in USD`,
      type = "histogram",
      nbinsx = 30,
      marker = list(color = 'rgba(142, 68, 173, 0.8)',
                    line = list(color = 'rgba(142, 68, 173, 1.0)', width = 1))
    ) %>%
      layout(title = "Distribució del salari (USD)",
             xaxis = list(title = "Salari"),
             yaxis = list(title = "Comptatge"))
  })
  
  output$comparativa_plot <- renderPlotly({
    mean_global <- mean(df$`Salary in USD`, na.rm = TRUE)
    mean_filtrado <- mean(filtered_data()$`Salary in USD`, na.rm = TRUE)
    
    plot_ly(
      x = c("Global", "Filtrat"),
      y = c(mean_global, mean_filtrado),
      type = "bar",
      marker = list(color = c("rgba(127, 140, 141, 0.8)", "rgba(39, 174, 96, 0.8)"),
                    line = list(color = c("rgba(127, 140, 141, 1.0)", "rgba(39, 174, 96, 1.0)"), width = 1)),
      text = c(dollar(mean_global), dollar(mean_filtrado)),
      hoverinfo = "text+x"
    ) %>%
      layout(title = "Comparació del salari mitjà",
             xaxis = list(title = ""),
             yaxis = list(title = "Salari mitjà (USD)", rangemode = "tozero"))
  })
  
  output$summary_table <- renderDataTable({
    filtered_data() %>%
      summarise(
        Observacions = n(),
        Salari_Mitja = dollar(mean(`Salary in USD`, na.rm = TRUE)),
        Salari_Màxim = dollar(max(`Salary in USD`, na.rm = TRUE)),
        Salari_Mínim = dollar(min(`Salary in USD`, na.rm = TRUE))
      )
  })
  
  output$recom_countries <- renderDataTable({
    filtered_data() %>%
      group_by(`Company Location`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      arrange(desc(Salari_Mig)) %>%
      slice_head(n = 3)
  })
  
  output$recom_title <- renderPrint({
    best_title <- filtered_data() %>%
      group_by(`Job Title`) %>%
      summarise(Salari_Mig = mean(`Salary in USD`, na.rm = TRUE)) %>%
      arrange(desc(Salari_Mig)) %>%
      slice_head(n = 1) %>%
      pull(`Job Title`)
    
    best_title
  })
  
  output$remote_plot <- renderPlotly({
    df_remote <- filtered_data() %>%
      count(Teletrabajo)
    
    plot_ly(df_remote, labels = ~Teletrabajo, values = ~n, type = 'pie') %>%
      layout(title = 'Distribució del tipus de treball',
             showlegend = TRUE,
             legend = list(orientation = 'h', x = 0.3, y = -0.1))
  })
  
  output$simulated_salary <- renderPrint({
  data <- df %>%
    filter((input$experience == "Qualsevol" | `Experience Level` == input$experience),
           (input$company_size == "Qualsevol" | `Company Size` == input$company_size))

  if (!is.null(input$region) && input$region != "Totes") {
    data <- data %>% filter(Region == input$region)
  }

  if (!is.null(input$remote) && input$remote != "Qualsevol") {
    data <- data %>% filter(Teletrabajo == input$remote)
  }

  if (nrow(data) == 0) {
    return("No hi ha dades disponibles per aquesta combinació.")
  }

  mean_salary <- mean(data$`Salary in USD`, na.rm = TRUE)

  paste0("$", formatC(mean_salary, format = "f", digits = 0, big.mark = ","))
})
  
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

shinyApp(ui = ui, server = server)
