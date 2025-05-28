# Versión 3 - Explorador de Salarios con diseño profesional

# Instalar si no tienes estos paquetes
# install.packages(c("shiny", "dplyr", "ggplot2", "readr", "plotly", "DT", "scales", "bslib", "shinyWidgets"))

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)
library(plotly)
library(DT)
library(scales)
library(bslib)
library(shinyWidgets)

# Cargar el dataset
# df <- read_csv("Data_Science_Salaries.csv")
df <- read_csv("Salaries_anual_clean.csv")

# Añadir columna de región y teletrabajo
df <- df %>% mutate(
  Region = case_when(
    `Company Location` %in% c('United States', 'Canada', 'Mexico', 'Puerto Rico') ~ "América del Norte",
    `Company Location` %in% c('Colombia', 'Brazil', 'Argentina') ~ "América del Sur",
    `Company Location` %in% c('United Kingdom', 'Portugal', 'Ireland', 'Germany', 'Spain', 'Poland', 'France', 'Netherlands', 'Luxembourg', 'Gibraltar', 'Ukraine', 'Slovenia', 'Greece', 'Latvia', 'Italy', 'Estonia', 'Czechia', 'Switzerland', 'Russia', 'Denmark', 'Sweden', 'Finland', 'Austria', 'Belgium', 'Romania', 'Lithuania', 'Norway', 'Russian Federation', 'Croatia', 'Hungary') ~ "Europa",
    `Company Location` %in% c('India', 'Vietnam', 'Philippines', 'Turkey', 'Japan', 'Singapore', 'Pakistan', 'Indonesia', 'Malaysia', 'Israel', 'Saudi Arabia', 'United Arab Emirates', 'South Korea', 'Thailand') ~ "Asia",
    `Company Location` %in% c('South Africa', 'Nigeria', 'Kenya', 'Ghana') ~ "África",
    `Company Location` %in% c('Australia', 'American Samoa', 'New Zealand') ~ "Oceanía",
    TRUE ~ "Altres"
  ),
  Teletrabajo = if_else(`Company Location` == `Employee Residence`, "Presencial", "Remot"),
  SalariDisponible = `Salary in USD` - cost
)

# Base sin outliers
base_data <- df %>% 
  group_by(`Company Location`) %>% 
  filter(n() >= 5) %>% 
  ungroup()

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
          selectizeInput("region", "Regió:",
                      choices = c(sort(unique(df$Region))), selected = NULL, multiple = TRUE, options = list(placeholder = 'Totes')),
          selectizeInput("countries", "Països:",
               choices = sort(unique(df$`Company Location`)),
               selected = NULL,
               multiple = TRUE,
               options = list(placeholder = 'Tots')),
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
            p("Utilitza els filtres de l'esquerra per veure estadístiques personalitzades sobre salaris, costos de vida i treball remot.", style = "font-size: 16px;"),
            p(tags$b("Consell útil:"), " Pots fer doble clic sobre qualsevol regió o país seleccionat per eliminar-lo del filtre.", style = "font-size: 15px;"),
            br(),
            checkboxInput("remove_outliers", "Eliminar països amb menys de 5 observacions", value = FALSE),
            conditionalPanel(
              condition = "input.remove_outliers == true",
              div(style = "color: red; font-style: italic; font-size: 14px;",
                  "⚠️ En eliminar els outliers, diversos països desapareixeran i les dades poden no representar tan bé la distribució global."))),
          tabPanel(tagList(icon("calculator"), span("Simulador de salari")),
                   br(),
                   h4("Calcula una estimació del teu salari"),
                   p("Aquesta estimació es basa en els filtres seleccionats a l'esquerra."),
                   br(),
                   h4("Salari brut estimat:"),
                   verbatimTextOutput("simulated_salary"),
                   br(),
                   h4("Cost de vida estimat:"),
                   verbatimTextOutput("simulated_life_cost"),
                   br(),
                   h4("Salari disponible estimat:"),
                   verbatimTextOutput("simulated_net_salary")
          ),
          tabPanel(tagList(icon("chart-bar"), span("Salari mitjà per país")),
                   br(),
                   plotlyOutput("salary_plot", height = "800px"),
                   div(
                    tags$style(HTML("
                      .radio-inline + .radio-inline {
                        margin-left: 20px;
                      }
                    ")),
                    radioButtons(
                      inputId = "salary_type",
                      label = NULL,
                      choices = c("Brut" = "brut", "Disponible" = "disponible"),
                      selected = "brut",
                      inline = TRUE
                    )
                  )),
          
          tabPanel(tagList(icon("dollar-sign"), span("Treballs millor pagats")),
                   br(),
                   plotlyOutput("title_plot", height = "800px")),
          
          tabPanel(tagList(icon("chart-area"), span("Distribució salarial")),
                   br(),
                   plotlyOutput("dist_plot", height = "800px")),
          
          tabPanel(tagList(icon("pie-chart"), span("Remot vs Presencial")),
                   br(),
                   plotlyOutput("remote_plot", height = "400px")),
          
          tabPanel(tagList(icon("table"), span("Estadístiques resum")),
                   br(),
                   radioGroupButtons(
                     inputId = "group_by",
                     label = NULL,
                     choices = c("PAIS" = "Company Location",
                                "EXPERIENCIA" = "Experience Level",
                                "POSICIÓ" = "Job Title",
                                "EMPRESA" = "Company Size"),
                     selected = "Company Location",
                     direction = "horizontal",
                     justified = TRUE),
                   br(),
                   DTOutput("summary_table")),

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
  ),

  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      function enableDeselectOnDoubleClick(selectId) {
        var input = $('#' + selectId)[0];
        if (!input) return;
        var selectize = input.selectize;

        $('#' + selectId + ' + .selectize-control .item').each(function() {
          var $item = $(this);
          $item.off('dblclick').on('dblclick', function() {
            var value = $item.attr('data-value');
            selectize.removeItem(value);
          });
        });

        selectize.on('change', function() {
          setTimeout(function() {
            $('#' + selectId + ' + .selectize-control .item').each(function() {
              var $item = $(this);
              $item.off('dblclick').on('dblclick', function() {
                var value = $item.attr('data-value');
                selectize.removeItem(value);
              });
            });
          }, 0);
        });
      }

      // Aplica a ambos selectores
      setTimeout(function() {
        enableDeselectOnDoubleClick('region');
        enableDeselectOnDoubleClick('countries');
      }, 1000);
    });
  ")),

  tags$script(HTML("
    Shiny.addCustomMessageHandler('expandRow', function(message) {
      var table = $('#summary_table').DataTable();
      var rowIndex = -1;
      table.rows().every(function(index, element) {
        var data = this.data();
        if (data[0] == message.id) {
          rowIndex = index;
          return false;
        }
      });

      if (rowIndex === -1) return;

      var tr = $(table.row(rowIndex).node());
      var next = tr.next();

      if (next.hasClass('details')) {
        next.remove();
      } else {
        tr.after(message.html);
      }
    });
  ")),

  tags$style(HTML("
    /* Estilo por defecto */
    .btn-group .btn {
      background-color: #f5f5f5 !important;
      color: #000000 !important;
      border: none !important;
      border-radius: 0 !important;
      font-weight: bold;
    }

    /* Hover */
    .btn-group .btn:hover {
      background-color: #e0e0e0 !important;
      color: #000000 !important;
    }

    /* Estado activo REAL de radioGroupButtons */
    .btn-check:checked + .btn,
    .btn-group .btn.active {
      background-color: #d0d0d0 !important;
      color: #000000 !important;
      box-shadow: none !important;
    }

    /* Borde del grupo */
    .btn-group {
      border: 1px solid #ccc;
      border-radius: 6px;
      overflow: hidden;
    }
  "))

)

server <- function(input, output, session) {
  
  observe({
    # Dataset base o filtrado por outliers
    current_data <- if (isTRUE(input$remove_outliers)) {
      df %>% group_by(`Company Location`) %>%
        filter(n() >= 5)
    } else {
      df
    }

    # Obtener los países según las regiones seleccionadas
    region_countries <- if (!is.null(input$region) && length(input$region) > 0) {
      current_data %>% filter(Region %in% input$region) %>% pull(`Company Location`) %>% unique()
    } else {
      character(0)
    }

    all_countries <- sort(unique(current_data$`Company Location`))
    available_countries <- setdiff(all_countries, region_countries)

    # Mantener selección previa válida
    current_selection <- isolate(input$countries)
    valid_selection <- intersect(current_selection, available_countries)

    updateSelectizeInput(session, "countries",
                        choices = available_countries,
                        selected = valid_selection,
                        server = TRUE)
  })

  observeEvent(input$expand_row, {
    req(input$expand_row)

    group_var <- input$group_by
    details <- filtered_data() %>%
      filter(.data[[group_var]] == input$expand_row) %>%
      select(`Company Location`, `Job Title`, `Experience Level`, `Salary in USD`, `Company Size`, Teletrabajo)

    table_html <- paste0(
      "<tr class='details'><td colspan='", ncol(details) + 1, "'>",
      "<table class='table table-sm table-bordered' style='margin:10px;'>",
      "<thead><tr>",
      paste0(lapply(colnames(details), function(col) paste0("<th>", col, "</th>")), collapse = ""),
      "</tr></thead><tbody>",
      paste0(apply(details, 1, function(row) {
        paste0("<tr>", paste0(lapply(row, function(cell) paste0("<td>", cell, "</td>")), collapse = ""), "</tr>")
      }), collapse = ""),
      "</tbody></table>",
      "</td></tr>"
    )

  session$sendCustomMessage("expandRow", list(id = input$expand_row, html = table_html))
  })

  # Filtrado reactivo de dades amb region i modalitat de treball
  filtered_data <- reactive({
    data <- if (input$remove_outliers) base_data else df
    data <- data %>%
      filter((input$experience == "Qualsevol" | `Experience Level` == input$experience),
             (input$company_size == "Qualsevol" | `Company Size` == input$company_size))
    
    # Determinar países a partir de regiones seleccionadas
    region_countries <- if (!is.null(input$region) && length(input$region) > 0) {
      df %>% filter(Region %in% input$region) %>% pull(`Company Location`) %>% unique()
    } else {
      character(0)
    }

    # Obtener países seleccionados manualmente
    selected_countries <- if (!is.null(input$countries) && length(input$countries) > 0) {
      input$countries
    } else {
      character(0)
    }

    # Lógica combinada
    if (length(region_countries) > 0 || length(selected_countries) > 0) {
      valid_countries <- union(region_countries, selected_countries)
      data <- data %>% filter(`Company Location` %in% valid_countries)
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
  
  region_colors <- c(
    "Europa"            = "#4B6C8A",  # Azul oscuro pastel
    "Asia"              = "#F5D76E",  # Amarillo pastel
    "África"            = "#A1866F",  # Marrón pastel
    "América del Sur"   = "#3E7D5E",  # Verde oscuro pastel
    "Oceanía"           = "#A2CBE3",  # Azul claro pastel
    "América del Norte" = "#E57373",   # Rojo pastel
    "Altres"            = "#7f7f7f"   # gris
  )


  output$salary_plot <- renderPlotly({
    df_plot <- filtered_data() %>%
      group_by(`Company Location`, Region) %>%
      summarise(
        Salari_Mig = mean(`Salary in USD`, na.rm = TRUE),
        Salari_Disponible = mean(`Salary in USD` - cost, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(if (input$salary_type == "disponible") Salari_Disponible else Salari_Mig))

    y_value <- if (input$salary_type == "disponible") df_plot$Salari_Disponible else df_plot$Salari_Mig
    text_value <- if (input$salary_type == "disponible") "Salari disponible" else "Salari brut"

    plot_ly(
      data = df_plot,
      y = ~reorder(`Company Location`, y_value),
      x = ~y_value,
      type = 'bar',
      orientation = 'h',
      color = ~Region,
      colors = region_colors,
      text = ~paste0("$", formatC(y_value, format = "f", big.mark = ",", digits = 0)),
      hoverinfo = 'text+y'
    ) %>% layout(
      title = list(text = paste0(text_value, " per país"), font = list(size = 20)),
      xaxis = list(title = paste0(text_value, " (USD)"), tickfont = list(size = 12)),
      yaxis = list(title = "País", tickfont = list(size = 10)),
      margin = list(t = 30, l = 140),
      legend = list(title = list(text = "Regió"))
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
  
  output$summary_table <- renderDT({
    req(input$group_by)

    summary_df <- filtered_data() %>%
      group_by(.data[[input$group_by]]) %>%
      summarise(
        Observacions = n(),
        Salari_Mitja = mean(`Salary in USD`, na.rm = TRUE),
        Cost_Vida = mean(cost, na.rm = TRUE),
        Salari_Disponible = mean(`Salary in USD` - cost, na.rm = TRUE),
        Salari_Màxim = max(`Salary in USD`, na.rm = TRUE),
        Salari_Mínim = min(`Salary in USD`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Salari_Mitja))

    colnames(summary_df)[1] <- input$group_by

    DT::datatable(
      summary_df,
      class = 'display',
      escape = FALSE,
      selection = "none",
      rownames = FALSE,
      options = list(
        paging = TRUE,
        ordering = TRUE,
        info = TRUE,
        autoWidth = TRUE
      )) %>%
      formatCurrency(c("Salari_Mitja", "Cost_Vida", "Salari_Disponible", "Salari_Màxim", "Salari_Mínim"), currency = "$")
  })
  
  output$recom_countries <- renderDataTable({
    filtered_data() %>%
      group_by(`Company Location`) %>%
      summarise(
        Salari_Mig = mean(`Salary in USD`, na.rm = TRUE),
        Cost_Vida = mean(cost, na.rm = TRUE),
        Salari_Disponible = mean(`Salary in USD` - cost, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(Salari_Disponible)) %>%
      slice_head(n = 3) %>%
      datatable(
        class = 'compact',
        rownames = FALSE,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          info = FALSE
        )
      ) %>%
      formatCurrency(c("Salari_Mig", "Cost_Vida", "Salari_Disponible"), currency = "$", digits = 2)
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

    # Determinar países a partir de regiones seleccionadas
    region_countries <- if (!is.null(input$region) && length(input$region) > 0) {
      df %>% filter(Region %in% input$region) %>% pull(`Company Location`) %>% unique()
    } else {
      character(0)
    }

    # Obtener países seleccionados manualmente
    selected_countries <- if (!is.null(input$countries) && length(input$countries) > 0) {
      input$countries
    } else {
      character(0)
    }

    # Lógica combinada
    if (length(region_countries) > 0 || length(selected_countries) > 0) {
      valid_countries <- union(region_countries, selected_countries)
      data <- data %>% filter(`Company Location` %in% valid_countries)
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

output$simulated_life_cost <- renderPrint({
  data <- filtered_data()

  mean_cost <- mean(data$`cost`, na.rm = TRUE)

  paste0("$", formatC(mean_cost, format = "f", digits = 0, big.mark = ","))
})

output$simulated_net_salary <- renderPrint({
  data <- filtered_data()

  mean_salary <- mean(data$`Salary in USD`, na.rm = TRUE)
  mean_cost <- mean(data$`cost`, na.rm = TRUE)  # Asegúrate que la columna se llame así

  net_salary <- mean_salary - mean_cost

  paste0("$", formatC(net_salary, format = "f", digits = 0, big.mark = ","))
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
