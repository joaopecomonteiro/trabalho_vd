# Pacotes necessários
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(bslib)
library(data.table)

#####

data = read.csv("C:/Users/castr/Desktop/Uni/VD/PROJ1/NYC_311_Data_20241009.csv", header=TRUE, sep=";", na.strings=c("", " ", "N/A"))

setDT(data)

data = data[, .(Created.Date, Closed.Date, Agency, Agency.Name, Complaint.Type, Descriptor, City, Borough, Longitude, Latitude, Status, Street.Name)]

data = data %>%
  mutate(across(where(is.character), tolower))

data[, Created.Date := parse_date_time(`Created.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]
data[, Closed.Date := parse_date_time(`Closed.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]

data[, Time.Open := as.period(difftime(Closed.Date, Created.Date), units = "hours")]


set(data, j = "Resolution.Time", value = round(as.numeric(difftime(data$Closed.Date, data$Created.Date), units = "hours"), 2))

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


#####

# Pacotes necessários
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(bslib)
library(data.table)

#####

data = read.csv("C:/Users/castr/Desktop/Uni/VD/PROJ1/NYC_311_Data_20241009.csv", header=TRUE, sep=";", na.strings=c("", " ", "N/A"))

setDT(data)

data = data[, .(Created.Date, Closed.Date, Agency, Agency.Name, Complaint.Type, Descriptor, City, Borough, Longitude, Latitude, Status, Street.Name)]

data = data %>%
  mutate(across(where(is.character), tolower))

data[, Created.Date := parse_date_time(`Created.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]
data[, Closed.Date := parse_date_time(`Closed.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]

data[, Time.Open := as.period(difftime(Closed.Date, Created.Date), units = "hours")]


set(data, j = "Resolution.Time", value = round(as.numeric(difftime(data$Closed.Date, data$Created.Date), units = "hours"), 2))

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


#####

# Interface do Usuário
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NYC 311 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      sliderInput(inputId = "num_top_cases", label = "Number of Top Complaint Types:", value = 10, min = 5, max = 50),
      sliderInput(inputId = "log_range_filter", label = "Filter by Log Range (min difference):", min = 0, max = 5, value = 1.5, step = 0.1),
      uiOutput("complaint_type_selector"),
      uiOutput("borough_selector"),
      dateRangeInput(inputId = "date_range", label = "Select Date Range:", start = min(data$Created.Date, na.rm = TRUE), end = max(data$Created.Date, na.rm = TRUE)),
      radioButtons(inputId = "time_resolution", label = "Choose resolution time representation:", choices = list("Daily" = "daily", "Hourly" = "hourly"), selected = "daily")
    )
  ),
  dashboardBody(
    plotlyOutput("heatmap"),
    plotlyOutput("borough_lineplot"),
    plotlyOutput("time_based_lineplot"),
    plotlyOutput("time_ncomplaints")
  )
)




server <- function(input, output) {
  
  processed_data <- reactive({
    data_c <- data %>%
      filter(Borough != 'unspecified', !is.na(Borough))
    
    top_complaints <- data_c %>%
      count(Complaint.Type.Clean, sort = TRUE) %>%
      top_n(input$num_top_cases, n)
    
    filtered_data <- data_c %>%
      filter(Complaint.Type.Clean %in% top_complaints$Complaint.Type.Clean)
    
    avg_resolution_by_borough <- filtered_data %>%
      group_by(Borough, Complaint.Type.Clean) %>%
      summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
    
    summary_data <- avg_resolution_by_borough %>%
      mutate(Log.Avg.Resolution.Time = log1p(Avg.Resolution.Time)) %>%
      filter(!is.na(Borough))
    
    log_range_per_type <- summary_data %>%
      group_by(Complaint.Type.Clean) %>%
      summarise(
        Log.Resolution.Time.Max = max(Log.Avg.Resolution.Time, na.rm = TRUE),
        Log.Resolution.Time.Min = min(Log.Avg.Resolution.Time, na.rm = TRUE),
        Log.Range = Log.Resolution.Time.Max - Log.Resolution.Time.Min,
        .groups = "drop"
      )
    
    filtered_log_range <- log_range_per_type %>%
      filter(Log.Range > input$log_range_filter)
    
    filtered_summary_data <- summary_data %>%
      filter(Complaint.Type.Clean %in% filtered_log_range$Complaint.Type.Clean)
    
    list(
      full_data = summary_data,
      filtered_data = filtered_summary_data,
      daily_data = filtered_data
    )
  })
  
  
  output$complaint_type_selector <- renderUI({
    req(processed_data()$filtered_data)
    complaint_types <- unique(processed_data()$filtered_data$Complaint.Type.Clean)
    checkboxGroupInput(
      inputId = "selected_complaint_types", 
      label = "Select Complaint Types:", 
      choices = complaint_types, 
      selected = complaint_types
    )
  })
  
  
  
  output$borough_selector <- renderUI({
    boroughs <- unique(processed_data()$filtered_data$Borough)
    checkboxGroupInput(inputId = "selected_boroughs", label = "Select Boroughs:", choices = boroughs, selected = boroughs)
  })
  
  output$heatmap <- renderPlotly({
    plot_data <- processed_data()$filtered_data
    
    pl <- ggplot(plot_data, aes(x = Borough, y = Complaint.Type.Clean, fill = Log.Avg.Resolution.Time)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(
        title = "Heatmap of Log-Transformed Avg Resolution Time",
        x = "Borough",
        y = "Complaint Type",
        fill = "Log(Avg Res Time)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(pl)
    
  })
  
  
  
  output$borough_lineplot <- renderPlotly({
    plot_data <- processed_data()$filtered_data
    
    filtered_plot_data <- plot_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs
      )
    
    pl <- ggplot(filtered_plot_data, aes(x = Borough, y = Avg.Resolution.Time, color = Complaint.Type.Clean, group = Complaint.Type.Clean)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_fill_viridis_d() +
      labs(title = "Line Plot of Avg Resolution Time by Borough and Complaint Type", x = "Borough", y = "Avg Resolution Time (hours)", color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    ggplotly(pl)
  })
  
  
  output$time_based_lineplot <- renderPlotly({
    filtered_data <- processed_data()$daily_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs,
        Created.Date.Day >= input$date_range[1],
        Created.Date.Day <= input$date_range[2]
      )
    
    
    
    if (input$time_resolution == "daily") {
      plot_data <- filtered_data %>%
        group_by(Created.Date.Day, Borough, Complaint.Type.Clean) %>%
        summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
      x_label <- "Dia"
      title <- "Daily Avg Resolution Time by Borough and Complaint Type"
    } else {
      plot_data <- filtered_data %>%
        mutate(Created.Hour = floor_date(Created.Date, "hour")) %>%
        group_by(Created.Hour, Borough, Complaint.Type.Clean) %>%
        summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
      x_label <- "Hora"
      title <- "Hourly Avg Resolution Time by Borough and Complaint Type"
    }
    
    pl <- ggplot(plot_data, aes(x = if (input$time_resolution == "daily") Created.Date.Day else Created.Hour,
                                y = Avg.Resolution.Time, color = Complaint.Type.Clean, group = Complaint.Type.Clean)) +
      geom_line(size = 0.5) +
      facet_wrap(~ Borough) +
      labs(title = title, x = x_label, y = "Avg Resolution Time (hours)", color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    ggplotly(pl)
  })
  
  
  output$time_ncomplaints <- renderPlotly({
    
    filtered_data <- processed_data()$daily_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs,
        Created.Date.Day >= input$date_range[1],
        Created.Date.Day <= input$date_range[2]
      )
    
    
    filtered_plot_data <- filtered_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs
      )
    
    grouped_data <- filtered_plot_data %>%
      group_by(Borough, Complaint.Type.Clean) %>%
      summarise(
        Total.Complaints = n(),
        Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
        .groups = "drop"
      )
    
    grouped_data$Log.Avg.Resolution.Time <- log1p(grouped_data$Avg.Resolution.Time)
    
    pl <- ggplot(grouped_data, aes(x = Total.Complaints, y = Log.Avg.Resolution.Time, shape = Borough)) +
      geom_point(aes(color = Complaint.Type.Clean), size = 3) +
      labs(
        title = "Avg Resolution Time vs Total Complaints by Borough and Complaint Type",
        x = "Number of Complaints",
        y = "Log Avg Resolution Time",
        shape = "Borough",
        color = "Complaint Type"
      ) +
      theme_minimal()
    
    ggplotly(pl)
  })
  
}


shinyApp(ui = ui, server = server)





server <- function(input, output) {
  
  processed_data <- reactive({
    data_c <- data %>%
      filter(Borough != 'unspecified', !is.na(Borough))
    
    top_complaints <- data_c %>%
      count(Complaint.Type.Clean, sort = TRUE) %>%
      top_n(input$num_top_cases, n)
    
    filtered_data <- data_c %>%
      filter(Complaint.Type.Clean %in% top_complaints$Complaint.Type.Clean)
    
    avg_resolution_by_borough <- filtered_data %>%
      group_by(Borough, Complaint.Type.Clean) %>%
      summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
    
    summary_data <- avg_resolution_by_borough %>%
      mutate(Log.Avg.Resolution.Time = log1p(Avg.Resolution.Time)) %>%
      filter(!is.na(Borough))
    
    log_range_per_type <- summary_data %>%
      group_by(Complaint.Type.Clean) %>%
      summarise(
        Log.Resolution.Time.Max = max(Log.Avg.Resolution.Time, na.rm = TRUE),
        Log.Resolution.Time.Min = min(Log.Avg.Resolution.Time, na.rm = TRUE),
        Log.Range = Log.Resolution.Time.Max - Log.Resolution.Time.Min,
        .groups = "drop"
      )
    
    filtered_log_range <- log_range_per_type %>%
      filter(Log.Range > input$log_range_filter)
    
    filtered_summary_data <- summary_data %>%
      filter(Complaint.Type.Clean %in% filtered_log_range$Complaint.Type.Clean)
    
    list(
      full_data = summary_data,
      filtered_data = filtered_summary_data,
      daily_data = filtered_data
    )
  })
  
  
  output$complaint_type_selector <- renderUI({
    req(processed_data()$filtered_data)
    complaint_types <- unique(processed_data()$filtered_data$Complaint.Type.Clean)
    checkboxGroupInput(
      inputId = "selected_complaint_types", 
      label = "Select Complaint Types:", 
      choices = complaint_types, 
      selected = complaint_types
    )
  })
  
  
  
  output$borough_selector <- renderUI({
    boroughs <- unique(processed_data()$filtered_data$Borough)
    checkboxGroupInput(inputId = "selected_boroughs", label = "Select Boroughs:", choices = boroughs, selected = boroughs)
  })
  
  output$heatmap <- renderPlotly({
    plot_data <- processed_data()$filtered_data
    
    pl <- ggplot(plot_data, aes(x = Borough, y = Complaint.Type.Clean, fill = Log.Avg.Resolution.Time)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(
        title = "Heatmap of Log-Transformed Avg Resolution Time",
        x = "Borough",
        y = "Complaint Type",
        fill = "Log(Avg Res Time)"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(pl)
    
    })
  
  
  
  output$borough_lineplot <- renderPlotly({
    plot_data <- processed_data()$filtered_data
    
    filtered_plot_data <- plot_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs
      )
    
    pl <- ggplot(filtered_plot_data, aes(x = Borough, y = Avg.Resolution.Time, color = Complaint.Type.Clean, group = Complaint.Type.Clean)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      scale_fill_viridis_d() +
      labs(title = "Line Plot of Avg Resolution Time by Borough and Complaint Type", x = "Borough", y = "Avg Resolution Time (hours)", color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    ggplotly(pl)
  })
  
  
  output$time_based_lineplot <- renderPlotly({
    filtered_data <- processed_data()$daily_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs,
        Created.Date.Day >= input$date_range[1],
        Created.Date.Day <= input$date_range[2]
      )
    
    
    
    if (input$time_resolution == "daily") {
      plot_data <- filtered_data %>%
        group_by(Created.Date.Day, Borough, Complaint.Type.Clean) %>%
        summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
      x_label <- "Dia"
      title <- "Daily Avg Resolution Time by Borough and Complaint Type"
    } else {
      plot_data <- filtered_data %>%
        mutate(Created.Hour = floor_date(Created.Date, "hour")) %>%
        group_by(Created.Hour, Borough, Complaint.Type.Clean) %>%
        summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
      x_label <- "Hora"
      title <- "Hourly Avg Resolution Time by Borough and Complaint Type"
    }
    
    pl <- ggplot(plot_data, aes(x = if (input$time_resolution == "daily") Created.Date.Day else Created.Hour,
                                  y = Avg.Resolution.Time, color = Complaint.Type.Clean, group = Complaint.Type.Clean)) +
      geom_line(size = 0.5) +
      facet_wrap(~ Borough) +
      labs(title = title, x = x_label, y = "Avg Resolution Time (hours)", color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
    
    ggplotly(pl)
  })
  
  
  output$time_ncomplaints <- renderPlotly({
    
    filtered_data <- processed_data()$daily_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs,
        Created.Date.Day >= input$date_range[1],
        Created.Date.Day <= input$date_range[2]
      )
    
    
    filtered_plot_data <- filtered_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs
      )
    
    grouped_data <- filtered_plot_data %>%
      group_by(Borough, Complaint.Type.Clean) %>%
      summarise(
        Total.Complaints = n(),
        Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
        .groups = "drop"
      )
    
    grouped_data$Log.Avg.Resolution.Time <- log1p(grouped_data$Avg.Resolution.Time)
    
    pl <- ggplot(grouped_data, aes(x = Total.Complaints, y = Log.Avg.Resolution.Time, shape = Borough)) +
      geom_point(aes(color = Complaint.Type.Clean), size = 3) +
      labs(
        title = "Avg Resolution Time vs Total Complaints by Borough and Complaint Type",
        x = "Number of Complaints",
        y = "Log Avg Resolution Time",
        shape = "Borough",
        color = "Complaint Type"
      ) +
      theme_minimal()
    
    ggplotly(pl)
  })
  
}


shinyApp(ui = ui, server = server)
