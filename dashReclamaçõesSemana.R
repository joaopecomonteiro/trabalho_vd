# Carregar pacotes
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(lubridate)
library(data.table)

data <- read.csv("~/Documents/UP/24 25/1ºsemestre/VisualizaçãoDados/Projeto/NYC_311_Data_20241009.csv", 
                 header = TRUE, sep = ";", na.strings = c("", " ", "N/A"))

data <- data %>%
  mutate(across(where(is.character), tolower)) %>%
  mutate(Created.Date = parse_date_time(Created.Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC"),
         Closed.Date = parse_date_time(Closed.Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC"),
         Resolution.Time = round(as.numeric(difftime(Closed.Date, Created.Date, units = "hours")), 2),
      ) %>%
  filter(!is.na(Resolution.Time), Resolution.Time >= 0)

data <- data %>%
  filter(Created.Date < '2016-09-30', Resolution.Time >=0)

data <- data %>%
  mutate(Created.Date.Day = as.Date(Created.Date))


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

data$Period.Of.Day <- case_when(
  hour(data$Created.Date) >= 0 & hour(data$Created.Date) < 6  ~ "Early Morning",
  hour(data$Created.Date) >= 6 & hour(data$Created.Date) < 12 ~ "Morning",
  hour(data$Created.Date) >= 12 & hour(data$Created.Date) < 18 ~ "Afternoon",
  TRUE ~ "Nigth"
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "NYC 311 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Filtros", tabName = "filtros", icon = icon("filter")),
      dateRangeInput("date_range", "Intervalo de Datas", start = min(data$Created.Date), end = max(data$Created.Date)),
      selectInput("complaint_type", "Tipo de Reclamação", choices = c("Todos", unique(data$Complaint.Type.Clean))),
      selectInput("borough", "Bairro", choices = c("Todos", unique(data$Borough))),
      selectInput("period_filter", "Período do Dia", choices = c("Todos", "Morning", "Afternoon", "Nigth", "Early Morning")),
      selectInput("weekday_filter", "Dia da Semana", choices = c("Todos", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
    )
  ),
  dashboardBody(
    fluidRow(
      box(plotlyOutput("violino"), width = 6),
      box(plotlyOutput("heatmap"), width = 6)
    ),
    fluidRow(
      box(plotlyOutput("line_chart"), width = 6),
      box(plotlyOutput("polar_chart"), width = 6)
    ),
    fluidRow(
      box(plotlyOutput("bar_chart"), width = 6),
      box(plotlyOutput("stacked_bar"), width = 6)
    ),
    fluidRow(
      box(plotlyOutput("thursday_trend"), width = 12)
    )
  )
)

# Server
server <- function(input, output, session) {
  filtered_data <- reactive({
    data_filtered <- data
    
    if (!is.null(input$date_range)) {
      data_filtered <- data_filtered %>% 
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    }
    
    if (input$complaint_type != "Todos") {
      data_filtered <- data_filtered %>% filter(Complaint.Type.Clean == input$complaint_type)
    }
    
    if (input$borough != "Todos") {
      data_filtered <- data_filtered %>% filter(Borough == input$borough)
    }
    
    if (input$period_filter != "Todos") {
      data_filtered <- data_filtered %>% filter(Period.Of.Day == input$period_filter)
    }
    
    if (input$weekday_filter != "Todos") {
      data_filtered <- data_filtered %>% filter(weekdays(Created.Date) == input$weekday_filter)
    }
    
    data_filtered$Weekday <- weekdays(data_filtered$Created.Date)
    
    data_filtered$Hour <- hour(data_filtered$Created.Date)
    
    return(data_filtered)
  })
  
  # Gráfico de Violino
  output$violino <- renderPlotly({
    violin_data <- filtered_data()
    
    violin_plot <- violin_data %>%
      filter(!is.na(Resolution.Time)) %>%
      plot_ly(
        x = ~Weekday,
        y = ~Resolution.Time,
        split = ~Weekday,
        type = 'violin',
        box = list(visible = TRUE),
        meanline = list(visible = TRUE),
        points = "all",
        jitter = 0.3,
        scalemode = "width"
      ) %>%
      layout(
        title = "Distribution of Resolution Times by Day of the Week",
        xaxis = list(title = "Day of the week"),
        yaxis = list(title = "Resolution Time (hours)")
      )
    violin_plot
  })
  
  # Heatmap Interativo
  output$heatmap <- renderPlotly({
    heatmap_data <- filtered_data() %>%
      group_by(Weekday, Hour) %>%
      summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE))
    
    heatmap_plot <- plot_ly(
      heatmap_data,
      x = ~Hour,
      y = ~Weekday,
      z = ~Average.Resolution.Time,
      type = "heatmap",
      colorscale = "Viridis"
    ) %>%
      layout(
        title = "Average Resolution Time",
        xaxis = list(title = "time of day"),
        yaxis = list(title = "Day of the week")
      )
    heatmap_plot
  })
  
  # Linha de Evolução ao Longo do Tempo
  output$line_chart <- renderPlotly({
    line_data <- filtered_data() %>%
      group_by(Date = as.Date(Created.Date)) %>%
      summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE))
    
    line_plot <- plot_ly(
      line_data,
      x = ~Date,
      y = ~Average.Resolution.Time,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'darkgreen', width = 3),
      marker = list(size = 8, color = 'blue')
    ) %>%
      layout(
        title = "Tempo Médio de Resolução ao Longo do Tempo",
        xaxis = list(title = "Data", tickformat = "%d-%m", tickangle = 45),
        yaxis = list(title = "Tempo Médio (horas)")
      )
    line_plot
  })
  
  # Gráfico Polar
  output$polar_chart <- renderPlotly({
    polar_data <- filtered_data() %>%
      group_by(Hour) %>%
      summarise(Count = n())
    
    polar_plot <- plot_ly(
      polar_data,
      type = 'barpolar',
      r = ~Count,
      theta = ~Hour * 15,
      text = ~paste("Hora:", Hour, "<br>Frequência:", Count),
      hoverinfo = "text",
      marker = list(color = ~Count, colorscale = "Viridis")
    ) %>%
      layout(
        polar = list(
          angularaxis = list(
            tickvals = seq(0, 345, 15),
            ticktext = as.character(seq(0, 23)),
            direction = "clockwise"
          ),
          radialaxis = list(title = "Frequência")
        )
      )
    polar_plot
  })
  
  # Frequência por Período do Dia
  output$bar_chart <- renderPlotly({
    bar_data <- data %>%
      mutate(Period.Of.Day = factor(Period.Of.Day, levels = c("Early Morning", "Morning", "Afternoon", "Nigth"))) %>%
      group_by(Period.Of.Day) %>%
      summarise(Frequency = n(), .groups = 'drop')
    
    bar_plot <- plot_ly(
      bar_data,
      x = ~Period.Of.Day,
      y = ~Frequency,
      type = "bar",
      marker = list(color = 'darkgreen', line = list(color = 'rgba(58, 71, 80, 1)', width = 2))
    ) %>%
      layout(
        title = "Frequency of Complaints by Time of Day",
        xaxis = list(title = "Time of Day"),
        yaxis = list(title = "Frequency")
      )
    
    bar_plot
  })
  
  # Tipos de Reclamações por Dia
  output$stacked_bar <- renderPlotly({
    complaint_data <- filtered_data() %>%
      count(Weekday, Complaint.Type.Clean, sort = TRUE) %>%
      top_n(10, n) %>%
      group_by(Weekday, Complaint.Type.Clean) %>%
      summarise(Volume = n())
    
    stacked_bar_plot <- ggplot(complaint_data, aes(x = Weekday, y = Volume, fill = Complaint.Type.Clean)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d(name = " Type of complaint") +
      labs(title = "Types of Complaints by Day of the Week", x = "Day of the week", y = "Volume") +
      theme_minimal()
    
    ggplotly(stacked_bar_plot)
  })
  
  output$thursday_trend <- renderPlotly({
    thursday_data <- filtered_data() %>%
      filter(weekdays(Created.Date) == "Thursday") %>%
      group_by(Date = as.Date(Created.Date)) %>%
      summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE))
    
    thursday_plot <- ggplot(thursday_data, aes(x = Date, y = Average.Resolution.Time)) +
      geom_line(color = "blue", size = 1.2) +
      geom_point(color = "darkblue", size = 3) +
      labs(title = "Evolução do Tempo Médio de Resolução às Quintas-feiras", x = "Data", y = "Tempo Médio de Resolução") +
      theme_minimal()
    
    ggplotly(thursday_plot)
  })
}

shinyApp(ui = ui, server = server)