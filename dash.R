# Pacotes necessários
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(bslib)

data <- read.csv("~/Documents/UP/24 25/1ºsemestre/VisualizaçãoDados/Projeto/NYC_311_Data_20241009.csv", 
                 header = TRUE, sep = ";", na.strings = c("", " ", "N/A"))

data <- data %>%
  filter(Created.Date < '2016-09-30')

data <- data %>%
  mutate(
    Created.Date = parse_date_time(Created.Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC"),
    Closed.Date = parse_date_time(Closed.Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC"),
    Resolution.Time = as.numeric(difftime(Closed.Date, Created.Date, units = "hours")),
    Period.Of.Day = case_when(
      hour(Created.Date) >= 6 & hour(Created.Date) < 12 ~ "Morning",
      hour(Created.Date) >= 12 & hour(Created.Date) < 18 ~ "Afternoon",
      hour(Created.Date) >= 18 & hour(Created.Date) < 24 ~ "Evening",
      TRUE ~ "Night"
    ),
    Weekday = weekdays(Created.Date)
  ) %>%
  filter(!is.na(Resolution.Time))


data$Complaint.Type.Clean <- tolower(data$Complaint.Type)

data$Complaint.Type.Clean <- gsub(".*noise.*", "noise", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*highway sign.*", "highway sign", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*street sign.*", "street sign", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*dof property.*", "dof property", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*illegal.*", "illegal", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*advocate.*", "advocate", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*dof parking.*", "dof parking", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*water.*", "water", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*animal.*", "animal", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*derelict.*", "derelict", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*homeless.*", "homeless", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*ferry.*", "ferry", data$Complaint.Type.Clean)

data$Complaint.Type.Clean <- gsub(".*food.*", "food", data$Complaint.Type.Clean)

# Interface do Usuário
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NYC 311 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Distribuição Geral", tabName = "distribution", icon = icon("chart-bar")),
      menuItem("Por Período do Dia", tabName = "period", icon = icon("clock")),
      menuItem("Heatmap Temporal", tabName = "heatmap", icon = icon("calendar")),
      menuItem("Série Temporal", tabName = "timeseries", icon = icon("line-chart")),
      hr(),
      dateRangeInput("date_range", "Intervalo de Datas:",
                     start = min(data$Created.Date),
                     end = max(data$Created.Date)),
      selectInput("period_filter", "Período do Dia:",
                  choices = c("Todos", unique(data$Period.Of.Day)), 
                  selected = "Todos"),
      selectInput("weekday_filter", "Dia da Semana:",
                  choices = c("Todos", unique(data$Weekday)),
                  selected = "Todos"),
      selectInput("type_filter", "Tipo de Reclamação:",
                  choices = c("Todos", unique(tolower(data$Complaint.Type))), 
                  selected = "Todos")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "distribution",
              fluidRow(
                box(title = "Distribuição dos Tempos de Resolução", 
                    status = "primary", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("histogram_plot"))
              )),
      tabItem(tabName = "period",
              fluidRow(
                box(title = "Tempos de Resolução por Período do Dia", 
                    status = "success", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("period_plot"))
              )),
      tabItem(tabName = "heatmap",
              fluidRow(
                box(title = "Heatmap Temporal", 
                    status = "warning", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("heatmap_plot"))
              )),
      tabItem(tabName = "timeseries",
              fluidRow(
                box(title = "Série Temporal do Tempo de Resolução", 
                    status = "info", 
                    solidHeader = TRUE, 
                    width = 12,
                    plotlyOutput("time_series_plot"))
              ))
    )
  )
)

#Servidor
server <- function(input, output) {
  
  filtered_data <- reactive({
    filtered <- data
    
    if (input$period_filter != "Todos") {
      filtered <- filtered %>% filter(Period.Of.Day == input$period_filter)
    }
    
    if (input$weekday_filter != "Todos") {
      filtered <- filtered %>% filter(Weekday == input$weekday_filter)
    }
    
    if (input$type_filter != "Todos") {
      filtered <- filtered %>% filter(tolower(Complaint.Type) == input$type_filter)
    }
    
    filtered <- filtered %>%
      filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    
    return(filtered)
  })
  
  # Histograma
  output$histogram_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Resolution.Time)) +
      geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.8) +
      labs(title = "Distribuição Geral dos Tempos de Resolução",
           x = "Tempo de Resolução (horas)", y = "Frequência") +
      theme_minimal(base_size = 14)
    ggplotly(gg)
  })
  
  # Gráfico por Período do Dia
  output$period_plot <- renderPlotly({
    gg <- ggplot(filtered_data(), aes(x = Period.Of.Day, y = Resolution.Time, fill = Period.Of.Day)) +
      geom_boxplot(alpha = 0.8, outlier.color = "red") +
      labs(title = "Tempos de Resolução por Período do Dia",
           x = "Período do Dia", y = "Tempo de Resolução (horas)") +
      theme_minimal(base_size = 14)
    ggplotly(gg)
  })
  
  # Heatmap
  output$heatmap_plot <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      mutate(Hour = hour(Created.Date)) %>%
      group_by(Weekday, Hour) %>%
      summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE)) %>%
      ungroup()
    
    gg <- ggplot(heatmap_data, aes(x = Hour, y = Weekday, fill = Average.Resolution.Time)) +
      geom_tile() +
      scale_fill_gradient(low = "lightblue", high = "darkblue") +
      labs(title = "Heatmap: Tempo Médio de Resolução por Hora e Dia da Semana",
           x = "Hora do Dia", y = "Dia da Semana", fill = "Tempo Médio (horas)") +
      theme_minimal(base_size = 14)
    ggplotly(gg)
  })
  
  # Série Temporal
  output$time_series_plot <- renderPlotly({
    time_series <- filtered_data() %>%
      mutate(Date = as.Date(Created.Date)) %>%
      group_by(Date) %>%
      summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE))
    
    gg <- ggplot(time_series, aes(x = Date, y = Average.Resolution.Time)) +
      geom_line(color = "blue", size = 1) +
      geom_point(color = "red", size = 2) +
      labs(title = "Série Temporal do Tempo de Resolução",
           x = "Data", y = "Tempo Médio (horas)") +
      theme_minimal(base_size = 14)
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)

