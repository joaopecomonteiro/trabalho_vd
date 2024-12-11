library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(bslib)
library(data.table)
library(RColorBrewer)
library(leaflet)
library(tidyr)
library(sf)

# Carregar e preparar os dados
data <- fread("C:/Users/diogo/OneDrive/Ambiente de Trabalho/FCUP - Data Science/1º Ano/1º Semestre/Visualização de Dados/Project/Dataset-20241021/NYC_311_Data_20241009/NYC_311_Data_20241009.csv", 
              sep = ";", fill = TRUE)

data <- data[, .(Created_Date, Closed_Date, Agency, Incident_Address, Incident_Zip, Community_Board, Agency_Name, Complaint_Type, Descriptor, City, Borough, Longitude, Latitude)]

# Formatando datas
data[, Created.Date := parse_date_time(Created_Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M"), tz = "UTC")]
data[, Closed.Date := parse_date_time(Closed_Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M"), tz = "UTC")]

data[, Resolution.Time := round(as.numeric(difftime(Closed.Date, Created.Date, units = "hours")), 2)]

# Limpar e categorizar Complaint_Type
data$Complaint_Type.Clean <- tolower(data$Complaint_Type)
data$Complaint_Type.Clean <- gsub(".*noise.*", "noise", data$Complaint_Type.Clean)
data$Complaint_Type.Clean <- gsub(".*highway sign.*", "highway sign", data$Complaint_Type.Clean)
data$Complaint_Type.Clean <- gsub(".*street sign.*", "street sign", data$Complaint_Type.Clean)
data$Complaint_Type.Clean <- gsub(".*illegal.*", "illegal", data$Complaint_Type.Clean)

# Filtrar datas inválidas
data <- data %>%
  mutate(Created.Date = as.Date(Created.Date)) %>%
  filter(!is.na(Created.Date) & Borough != "Unspecified" & Borough != "")

# Classificar dias como Dia de Semana ou Fim de Semana
data <- data %>%
  mutate(Day_Type = case_when(
    wday(Created.Date) %in% c(1, 7) ~ "Weekend/Holidays",
    TRUE ~ "Weekdays"
  ))

# Função para identificar outliers
find_outliers <- function(data, group_var, target_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      Q1 = quantile({{ target_var }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ target_var }}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1
    ) %>%
    mutate(Lower_Bound = Q1 - 1.5 * IQR, Upper_Bound = Q3 + 1.5 * IQR) %>%
    right_join(data, by = as.character(rlang::ensym(group_var))) %>%
    filter({{ target_var }} < Lower_Bound | {{ target_var }} > Upper_Bound)
}

# Identificar outliers
outliers_borough <- find_outliers(data, Borough, Resolution.Time)
outliers_complaint <- find_outliers(data, Complaint_Type, Resolution.Time)
outliers_complaintclean <- find_outliers(data, Complaint_Type.Clean, Resolution.Time)
outliers_day_type <- find_outliers(data, Day_Type, Resolution.Time)

# Criar subconjuntos de dados para os mapas
outliers_top5 <- outliers_borough %>% head(5)
extreme_outliers_top5 <- outliers_complaint %>% head(5)
outliers_top5_8760 <- outliers_day_type %>% head(5)



# Function to calculate outliers for a given grouping variable
find_ext_outliers <- function(data, group_var, target_var) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      Q1 = quantile({{ target_var }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ target_var }}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1
    ) %>%
    mutate(Lower_Bound = Q1 - 3 * IQR, Upper_Bound = Q3 + 3 * IQR) %>%
    right_join(data, by = as.character(rlang::ensym(group_var))) %>%
    filter({{ target_var }} < Lower_Bound | {{ target_var }} > Upper_Bound) %>%
    select(-c(Q1, Q3, IQR, Lower_Bound, Upper_Bound))  # Keep only relevant columns
}

# Identify outliers for each category
ext_outliers_borough <- find_ext_outliers(data, Borough, Resolution.Time)
ext_outliers_complaint <- find_ext_outliers(data, Complaint_Type, Resolution.Time)
ext_outliers_complaintclean <- find_ext_outliers(data, Complaint_Type.Clean, Resolution.Time)
ext_outliers_day_type <- find_ext_outliers(data, Day_Type, Resolution.Time)

find_outliers_custom_conditions <- function(data, group_var, target_var, lower_limit) {
  data %>%
    group_by({{ group_var }}) %>%
    summarise(
      Q1 = quantile({{ target_var }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ target_var }}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1
    ) %>%
    mutate(Lower_Bound = Q1 - 3 * IQR, Upper_Bound = Q3 + 3 * IQR) %>%
    right_join(data, by = as.character(rlang::ensym(group_var))) %>%
    filter({{ target_var }} > lower_limit) %>%  # Aplica a condição para detectar valores acima do limite
    select(-c(Q1, Q3, IQR, Lower_Bound, Upper_Bound))  # Remove colunas adicionais
}


# Identificar outliers para valores superiores a 8760 - 1 ano
outliers_borough_8760 <- find_outliers_custom_conditions(data, Borough, Resolution.Time, 8760)
outliers_complaint_8760 <- find_outliers_custom_conditions(data, Complaint_Type, Resolution.Time, 8760)
outliers_complaintclean_8760 <- find_outliers_custom_conditions(data, Complaint_Type.Clean, Resolution.Time, 8760)
outliers_day_type_8760 <- find_outliers_custom_conditions(data, Day_Type, Resolution.Time, 8760)

# Carregar os dados dos boroughs
nyc_boroughs <- st_read("C:/Users/diogo/OneDrive/Ambiente de Trabalho/FCUP - Data Science/1º Ano/1º Semestre/Visualização de Dados/Project/nybb.shp") %>%
  st_transform(crs = 4326)

top5_complaints_outliers <- outliers_complaint %>%
  count(Complaint_Type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  rename(Most_Common_Outlier = Complaint_Type)

# Outliers extremos
top5_complaints_extreme_outliers <- ext_outliers_complaint %>%
  count(Complaint_Type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  rename(Most_Common_Extreme_Outlier = Complaint_Type)

# Outliers com mais de 1 ano
top5_complaints_outliers_8760 <- outliers_complaint_8760 %>%
  count(Complaint_Type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  rename(Most_Common_1year = Complaint_Type)


# 2. Filtrar os Dados para os Top 5 Tipos de Queixa
# Outliers gerais
outliers_top5 <- outliers_complaint %>%
  filter(Complaint_Type %in% top5_complaints_outliers$Most_Common_Outlier) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Borough != "Unspecified")

# Outliers extremos
extreme_outliers_top5 <- ext_outliers_complaint %>%
  filter(Complaint_Type %in% top5_complaints_extreme_outliers$Most_Common_Extreme_Outlier) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Borough != "Unspecified")

# Outliers com mais de 1 ano
outliers_top5_8760 <- outliers_complaint_8760 %>%
  filter(Complaint_Type %in% top5_complaints_outliers_8760$Most_Common_1year) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Borough != "Unspecified")

# 3. Ajustar o Shapefile com os Dados dos Boroughs
# Outliers gerais
nyc_boroughs_outliers <- nyc_boroughs %>%
  left_join(outliers_top5, by = c("BoroName" = "Borough"))

# Outliers extremos
ext_outliers_summary <- ext_outliers_complaint %>%
  group_by(Borough) %>%
  summarise(
    Most_Common_Extreme_Outlier = Complaint_Type[which.max(table(Complaint_Type))],
    Total_Outliers = n()
  )

nyc_boroughs_extreme_outliers <- nyc_boroughs %>%
  left_join(ext_outliers_summary, by = c("BoroName" = "Borough"))

# Outliers com mais de 1 ano
nyc_boroughs_outliers_8760 <- nyc_boroughs %>%
  left_join(outliers_top5_8760, by = c("BoroName" = "Borough"))


# Criar uma paleta de cores para os tipos de queixa
all_complaint_types <- unique(c(
  outliers_top5$Complaint_Type,
  extreme_outliers_top5$Complaint_Type,
  outliers_top5_8760$Complaint_Type
))

color_palette <- setNames(brewer.pal(n = length(all_complaint_types), "Paired"), all_complaint_types)

# Criar os mapas
map_outliers <- ggplot() +
  geom_sf(data = nyc_boroughs_outliers, color = "black", size = 0.2) +
  geom_point(data = outliers_top5, aes(x = Longitude, y = Latitude, color = Complaint_Type), size = 2, alpha = 0.4) +
  labs(
    title = "Top 5 outliers in New York map",
    color = "Complaint type (Top 5)"
  ) +
  theme_minimal() +
  scale_color_manual(values = color_palette)

map_extreme_outliers <- ggplot() +
  geom_sf(data = nyc_boroughs_extreme_outliers, color = "black", size = 0.2) +
  geom_point(data = extreme_outliers_top5, aes(x = Longitude, y = Latitude, color = Complaint_Type), size = 2, alpha = 0.4) +
  labs(
    title = "Top 5 extreme outliers in New York map",
    color = "Complaint type (Top 5)"
  ) +
  theme_minimal() +
  scale_color_manual(values = color_palette)

map_outliers_8760 <- ggplot() +
  geom_sf(data = nyc_boroughs_outliers_8760, color = "black", size = 0.2) +
  geom_point(data = outliers_top5_8760, aes(x = Longitude, y = Latitude, color = Complaint_Type), size = 2, alpha = 0.4) +
  labs(
    title = "Top 5 observations with more than 1 year resolution time in New York map",
    color = "Complaint type (Top 5)"
  ) +
  theme_minimal() +
  scale_color_manual(values = color_palette)


# Passo 1: Limpar e verificar os nomes nos dados de outliers
# Remover Boroughs não especificados
filtered_outliers_complaint <- outliers_complaint %>%
  filter(Borough != "Unspecified")

ext_outliers_complaint <- ext_outliers_complaint %>%
  filter(Borough != "Unspecified")

# Padronizar nomes (se necessário, ajuste manualmente caso os nomes estejam diferentes)
filtered_outliers_complaint$Borough <- tolower(filtered_outliers_complaint$Borough)
ext_outliers_complaint$Borough <- tolower(ext_outliers_complaint$Borough)
nyc_boroughs$BoroName <- tolower(nyc_boroughs$BoroName)

# Passo 2: Determinar os tipos de outliers mais comuns por Borough
outliers_by_borough <- filtered_outliers_complaint %>%
  group_by(Borough, Complaint_Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Borough, desc(Count)) %>%
  group_by(Borough) %>%
  slice_max(Count, n = 1) %>%
  rename(Most_Common_Outlier = Complaint_Type)

extreme_outliers_by_borough <- ext_outliers_complaint %>%
  group_by(Borough, Complaint_Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Borough, desc(Count)) %>%
  group_by(Borough) %>%
  slice_max(Count, n = 1) %>%
  rename(Most_Common_Extreme_Outlier = Complaint_Type)

# Passo 3: Mesclar as informações com o shapefile
# A mesclagem agora será feita com os nomes padronizados
nyc_boroughs <- nyc_boroughs %>%
  left_join(outliers_by_borough, by = c("BoroName" = "Borough")) %>%
  left_join(extreme_outliers_by_borough, by = c("BoroName" = "Borough"))



# Passo 4: Criar os gráficos

# Paleta de cores consistente para ambos os gráficos
# Combinar os tipos de outliers e extreme outliers em um vetor único
all_complaint_types <- unique(c(
  nyc_boroughs$Most_Common_Outlier,
  nyc_boroughs$Most_Common_Extreme_Outlier
))

# Criar uma paleta de cores personalizada (usar RColorBrewer ou uma função de cores)
library(RColorBrewer)
set.seed(42)  # Garantir consistência
color_palette <- setNames(brewer.pal(length(all_complaint_types), "Set3"), all_complaint_types)

# Mapa 1: Outliers mais comuns
map_outliers2 <- ggplot(data = nyc_boroughs) +
  geom_sf(aes(fill = Most_Common_Outlier), color = "black", size = 0.3) +
  scale_fill_manual(values = color_palette, na.value = "grey80") +
  labs(
    title = "Most common outlier by borough",
    fill = "Tipo de Outlier"
  ) +
  theme_minimal()

# Mapa 2: Extreme outliers mais comuns
map_extreme_outliers2 <- ggplot(data = nyc_boroughs) +
  geom_sf(aes(fill = Most_Common_Extreme_Outlier), color = "black", size = 0.3) +
  scale_fill_manual(values = color_palette, na.value = "grey80") +
  labs(
    title = "Most common extreme outlier by borough",
    fill = "Tipo de Extreme Outlier"
  ) +
  theme_minimal()


# Determinar o tipo de outlier mais comum por borough para >1 ano
common_outlier_8760 <- outliers_complaint_8760 %>%
  group_by(Borough, Complaint_Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Borough, desc(Count)) %>%
  filter(!is.na(Borough)) %>%
  group_by(Borough) %>%
  slice_max(Count, n = 1)

# Padronizar nomes dos boroughs em common_outlier_8760
common_outlier_8760$Borough <- tolower(common_outlier_8760$Borough)

# Adicionar a informação ao shapefile
nyc_boroughs <- nyc_boroughs %>%
  left_join(common_outlier_8760, by = c("BoroName" = "Borough")) %>%
  rename(Most_Common_Outlier_1Year = Complaint_Type)

# Paleta de cores consistente para todos os gráficos
all_complaint_types <- unique(c(
  nyc_boroughs$Most_Common_Outlier,
  nyc_boroughs$Most_Common_Extreme_Outlier,
  nyc_boroughs$Most_Common_Outlier_1Year
))

# Criar a paleta de cores
library(RColorBrewer)
set.seed(42)
color_palette <- setNames(brewer.pal(length(all_complaint_types), "Set3"), all_complaint_types)

# Mapa 3: Outliers com mais de 1 ano
map_outliers_1year2 <- ggplot(data = nyc_boroughs) +
  geom_sf(aes(fill = Most_Common_Outlier_1Year), color = "black", size = 0.3) +
  scale_fill_manual(values = color_palette, na.value = "grey80") +
  labs(
    title = "Most common observation with more than 1 year resolution time by borough",
    fill = "Resolution Type (> 1 Ano)"
  ) +
  theme_minimal()




# UI
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NYC 311 Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      id="menu",
      menuItem("All observations vs Outliers", tabName = "outliers_chart", icon = icon("exclamation-triangle")),
      menuItem("Outliers", tabName = "outliers_chart2", icon = icon("exclamation-triangle")),
      menuItem("Extreme Outliers", tabName = "ext_outliers_chart", icon = icon("exclamation-triangle")),
      menuItem("More than 1 year observations", tabName = "outliers_chart_8760", icon = icon("calendar")),
      menuItem("Maps - Top 5 by Outliers", tabName = "interactive_maps", icon = icon("map")),
      menuItem("Maps - Top 1 by Outliers", tabName = "interactive_maps2", icon = icon("map")),
      menuItem("Extreme (> 1 year) observations", tabName = "combined_graphs", icon = icon("search")),
      menuItem("Outliers distribution by Borough", tabName = "top10_complaints", icon = icon("table")),
      menuItem("Homeless Assistance Map", tabName = "homeless", icon = icon("search")),
      hr(),
      conditionalPanel(
        condition = "input.menu === 'outliers_chart' || input.menu === 'outliers_chart2' || input.menu === 'ext_outliers_chart' || input.menu === 'outliers_chart_8760' || input.menu === 'combined graphs' || input.menu === 'top10_complaints' || input.menu === 'homeless'",  # Defina as abas desejadas
        dateRangeInput(
          "date_range",
          "Date Range:",
          start = min(data$Created.Date),
          end = max(data$Created.Date)
        )
      ),
      
      conditionalPanel(
        condition = "input.menu === 'outliers_chart'",
        selectInput("outliers_group", "Outliers by:",
                    choices = c(
                      "Borough" = "Borough",
                      "Complaint Type (Top 10)" = "Complaint_Type",
                      "Complaint Type - Grouped (Top 10)" = "Complaint_Type.Clean",
                      "Day Type" = "Day_Type"
                    ),
                    selected = "Borough")
      ),
      conditionalPanel(
        condition = "input.menu === 'outliers_chart2'",
        selectInput("outliers_group", "Outliers by:",
                    choices = c(
                      "Borough" = "Borough",
                      "Complaint Type (Top 10)" = "Complaint_Type",
                      "Complaint Type - Grouped (Top 10)" = "Complaint_Type.Clean",
                      "Day Type" = "Day_Type"
                    ),
                    selected = "Borough")
      ),
      conditionalPanel(
        condition = "input.menu === 'ext_outliers_chart'",
        selectInput("ext_outliers_group", "Extreme Outliers by:",
                    choices = c(
                      "Borough" = "Borough",
                      "Complaint Type (Top 10)" = "Complaint_Type",
                      "Complaint Type - Grouped (Top 10)" = "Complaint_Type.Clean",
                      "Day Type" = "Day_Type"
                    ),
                    selected = "Borough")
      ),
      conditionalPanel(
        condition = "input.menu === 'outliers_chart_8760'",
        selectInput("outliers_group_8760", "Observations (> 1 year) by:",
                    choices = c(
                      "Borough" = "Borough",
                      "Complaint Type (Top 10)" = "Complaint_Type",
                      "Complaint Type - Grouped (Top 10)" = "Complaint_Type.Clean",
                      "Day Type" = "Day_Type"
                    ),
                    selected = "Borough")
      )
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "outliers_chart",
              fluidRow(
                box(
                  title = "All observations vs Outliers plots",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("outliers_plot")
                )
              )
      ),
      
      tabItem(tabName = "outliers_chart2",
              fluidRow(
                box(
                  title = "Outliers plots",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("outliers_plot2")
                )
              )
      ),
      
      tabItem(tabName = "ext_outliers_chart",
              fluidRow(
                box(
                  title = "Extreme outliers plots",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("ext_outliers_plot")
                )
              )
      ),
      
      tabItem(tabName = "outliers_chart_8760",
              fluidRow(
                box(
                  title = "More than 1 year observations plots",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("outliers_plot_8760")
                )
              )
      ),
      
      tabItem(
        tabName = "combined_graphs",
        fluidRow(
          box(
            title = "More than 1 year observations extreme values boxplots",
            status = "primary",
            solidHeader = TRUE,
            width = 12, # Ajusta automaticamente à largura da coluna
            plotlyOutput("boxplot_chart") # Define altura do gráfico
          ),
          box(
            title = "More than 1 year observations extreme values map",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotlyOutput("map_chart")
          )
        )
      ),
      
      tabItem(tabName = "top10_complaints",
              fluidRow(
                box(
                  title = "Outliers distribution plot",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("top10_plot")
                )
              )
      ),
      
      tabItem(tabName = "homeless",
              fluidRow(
                box(
                  title = "Homeless Person Assistance map distrbution",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotlyOutput("homeless")
                )
              )
      ),
      
      tabItem(tabName = "interactive_maps",
              fluidRow(
                box(
                  title = "Top 5 by different types of outliers maps",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput(
                    "map_choice",
                    "Choose the map:",
                    choices = c(
                      "Outliers Gerais" = "map_outliers",
                      "Outliers Extremos" = "map_extreme_outliers",
                      "Outliers com Mais de 1 Ano" = "map_outliers_8760"
                    )
                  ),
                  plotOutput("ggplot_map_display", height = 600)
                )
              )),
      
      tabItem(tabName = "interactive_maps2",
              fluidRow(
                box(
                  title = "Top 1 by different types of outliers maps",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  selectInput(
                    "map_choice2",
                    "Choose the map:",
                    choices = c(
                      "Outliers Gerais" = "map_outliers2",
                      "Outliers Extremos" = "map_extreme_outliers2",
                      "Outliers com Mais de 1 Ano" = "map_outliers_1year2"
                    )
                  ),
                  plotOutput("ggplot_map_display2", height = 600)
                )
              ))
    )
  )
)

# Server
server <- function(input, output) {
  get_top10 <- function(data, group_var) {
    data %>%
      count({{ group_var }}, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      pull({{ group_var }})
  }
  
  # Dados filtrados
  filtered_data <- reactive({
    data %>%
      filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
  })
  
  # Gráfico de Outliers
  output$outliers_plot <- renderPlotly({
    selected_group <- input$outliers_group
    
    # Filtrar os Top 10, se necessário
    get_top10_filtered <- function(data, group_var) {
      top10_values <- data %>%
        count({{ group_var }}, sort = TRUE) %>%
        slice_head(n = 10) %>%
        pull({{ group_var }})
      data %>% filter({{ group_var }} %in% top10_values)
    }
    
    # Selecionar os dados de acordo com o grupo escolhido
    original_data <- filtered_data()
    
    if (selected_group %in% c("Complaint_Type", "Complaint_Type.Clean")) {
      original_data <- get_top10_filtered(original_data, !!sym(selected_group))
    }
    
    # Contagens de valores originais
    original_counts <- original_data %>%
      group_by(across(all_of(selected_group))) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Contagens de outliers
    outlier_data <- switch(
      selected_group,
      "Borough" = outliers_borough %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]),
      "Complaint_Type" = outliers_complaint %>%
        filter(Complaint_Type %in% get_top10_filtered(filtered_data(), Complaint_Type)$Complaint_Type),
      "Complaint_Type.Clean" = outliers_complaintclean %>%
        filter(Complaint_Type.Clean %in% get_top10_filtered(filtered_data(), Complaint_Type.Clean)$Complaint_Type.Clean),
      "Day_Type" = outliers_day_type %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    )
    
    outlier_counts <- outlier_data %>%
      group_by(across(all_of(selected_group))) %>%
      summarise(Outlier_Count = n(), .groups = "drop")
    
    # Combinar os dados originais e de outliers
    combined_data <- bind_rows(
      mutate(original_counts, Type = "Original", Value = Count),
      mutate(outlier_counts, Type = "Outliers", Value = Outlier_Count)
    )
    
    # Gráfico
    gg <- ggplot(combined_data, aes_string(x = selected_group, y = "Value", fill = "Type")) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Number of observations and outliers by", selected_group),
        x = selected_group,
        y = "Count",
        fill = "Tipo"
      ) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg)
  })
  
  
  # Gráfico de Apenas Outliers
  output$outliers_plot2 <- renderPlotly({
    selected_group <- input$outliers_group
    
    # Filtrar os dados de acordo com o grupo escolhido, mas sem aplicar o filtro de Top 10 ainda
    original_data <- filtered_data()
    
    # Contagens de outliers para todos os dados, sem restrição de Top 10
    outlier_data <- switch(
      selected_group,
      "Borough" = outliers_borough %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]),
      "Complaint_Type" = outliers_complaint %>%
        filter(Complaint_Type %in% unique(original_data$Complaint_Type))%>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]),  # Não restringe ao Top 10 aqui
      "Complaint_Type.Clean" = outliers_complaintclean %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]) %>%
        filter(Complaint_Type.Clean %in% unique(original_data$Complaint_Type.Clean)),  # Mesmo para a versão limpa
      "Day_Type" = outliers_day_type %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    )
    
    # Contagens de outliers por grupo
    outlier_counts <- outlier_data %>%
      group_by(across(all_of(selected_group))) %>%
      summarise(Outlier_Count = n(), .groups = "drop")
    
    # Selecionar o Top 10 das categorias com mais outliers
    top10_outliers <- outlier_counts %>%
      arrange(desc(Outlier_Count)) %>%
      slice_head(n = 10)
    
    # Gráfico
    gg <- ggplot(top10_outliers, aes_string(x = selected_group, y = "Outlier_Count", fill = selected_group)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Number of outliers by", selected_group),
        x = selected_group,
        y = "Count",
      ) +
      scale_fill_brewer(palette = "Set3") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg)
  })
  
  
  # Gráfico de Extreme Outliers
  output$ext_outliers_plot <- renderPlotly({
    selected_group <- input$ext_outliers_group
    # Filtrar os dados de acordo com o grupo escolhido, mas sem aplicar o filtro de Top 10 ainda
    original_data <- filtered_data()
    
    # Contagens de outliers para todos os dados, sem restrição de Top 10
    ext_outlier_data <- switch(
      selected_group,
      "Borough" = ext_outliers_borough %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]),
      "Complaint_Type" = ext_outliers_complaint %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]) %>%
        filter(Complaint_Type %in% unique(original_data$Complaint_Type)),  # Não restringe ao Top 10 aqui
      "Complaint_Type.Clean" = ext_outliers_complaintclean %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]) %>%
        filter(Complaint_Type.Clean %in% unique(original_data$Complaint_Type.Clean)),  # Mesmo para a versão limpa
      "Day_Type" = ext_outliers_day_type %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    )
    
    # Contagens de outliers por grupo
    ext_outlier_counts <- ext_outlier_data %>%
      group_by(across(all_of(selected_group))) %>%
      summarise(ext_Outlier_Count = n(), .groups = "drop")
    
    # Selecionar o Top 10 das categorias com mais outliers
    top10_ext_outliers <- ext_outlier_counts %>%
      arrange(desc(ext_Outlier_Count)) %>%
      slice_head(n = 10)
    
    # Gráfico
    gg <- ggplot(top10_ext_outliers, aes_string(x = selected_group, y = "ext_Outlier_Count", fill = selected_group)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Number of extreme outliers by", selected_group),
        x = selected_group,
        y = "Count",
      ) +
      scale_fill_brewer(palette = "Set3") + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg)
  })
  
  
  # Gráfico de More than a year
  output$outliers_plot_8760 <- renderPlotly({
    selected_group <- input$outliers_group_8760
    
    # Filtrar os dados de acordo com o grupo escolhido, mas sem aplicar o filtro de Top 10 ainda
    original_data <- filtered_data()
    
    # Contagens de outliers para todos os dados, sem restrição de Top 10
    outlier_data_8760 <- switch(
      selected_group,
      "Borough" = outliers_borough_8760 %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]),
      "Complaint_Type" = outliers_complaint_8760 %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]) %>%
        filter(Complaint_Type %in% unique(original_data$Complaint_Type)),  # Não restringe ao Top 10 aqui
      "Complaint_Type.Clean" = outliers_complaintclean_8760 %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]) %>%
        filter(Complaint_Type.Clean %in% unique(original_data$Complaint_Type.Clean)),  # Mesmo para a versão limpa
      "Day_Type" = outliers_day_type_8760 %>%
        filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    )
    
    # Contagens de outliers por grupo
    outlier_counts_8760 <- outlier_data_8760 %>%
      group_by(across(all_of(selected_group))) %>%
      summarise(Outlier_Count_8760 = n(), .groups = "drop")
    
    # Selecionar o Top 10 das categorias com mais outliers
    top10_outliers_8760 <- outlier_counts_8760 %>%
      arrange(desc(Outlier_Count_8760)) %>%
      slice_head(n = 10)
    
    # Gráfico
    gg <- ggplot(top10_outliers_8760, aes_string(x = selected_group, y = "Outlier_Count_8760", fill = selected_group)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Number of observations with more than a year resolution time by", selected_group),
        x = selected_group,
        y = "Count"
      ) +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg)
  })
  
  
  
  # Gráfico de Boxplot
  output$boxplot_chart <- renderPlotly({
    
    
    filtered_boxplot_data <- filtered_data() %>%
      filter(Complaint_Type %in% c("Building/Use", "New Tree Request", "General Construction/Plumbing", "Overgrown Tree/Branches"))
    
    gg <- ggplot(filtered_boxplot_data, aes(x = Complaint_Type, y = Resolution.Time, fill = Complaint_Type)) +
      geom_boxplot() +
      labs(
        title = "Boxplots by complaint type",
        x = "Complaint type",
        y = "Resolution time",
        fill = "Tipo"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(gg)
  })
  
  
  
  
  # Gráfico de Mapa
  output$map_chart <- renderPlotly({
    
    map_data <- outliers_complaint_8760 %>%
      filter(Complaint_Type %in% c("Building/Use", "New Tree Request", "General Construction/Plumbing", "Overgrown Tree/Branches")) %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%  
      filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    
    gg <- ggplot() +
      geom_sf(data = nyc_boroughs, fill = "gray80", color = "black", size = 0.2) +
      geom_point(data = map_data, aes(x = Longitude, y = Latitude, color = Complaint_Type), size = 2, alpha = 0.3) +
      labs(
        title = "Complaint type map",
        x = "Longitude",
        y = "Latitude",
        color = "Complaint type"
      ) +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  
  # Gráfico do Top 10 Queixas
  output$top10_plot <- renderPlotly({
    # Filtrar dados com base no intervalo de datas
    filtered_outliers_complaint <- outliers_complaint %>%
      filter(
        Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2]
      )
    
    # Selecionar os 10 tipos de queixa mais frequentes no intervalo
    top_10_complaints <- filtered_outliers_complaint %>%
      count(Complaint_Type, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(Complaint_Type)
    
    # Filtrar para o top 10 e excluir Borough = "Unspecified"
    outliers_complaint_top10 <- filtered_outliers_complaint %>%
      filter(Complaint_Type %in% top_10_complaints, Borough != "Unspecified")
    
    # Criar o gráfico
    gg <- ggplot(outliers_complaint_top10, aes(x = Complaint_Type, fill = Borough)) +
      geom_bar(position = "fill") +
      labs(
        title = "Proportion of Outliers by Complaint Type (Top 10)",
        x = "Complaint type",
        y = "Proportion of Outliers",
        fill = "Borough"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(palette = "Set2")
    
    ggplotly(gg)
  })
  
  
  # Homeless
  output$homeless <- renderPlotly({
    # Filtrar os dados para "Homeless Person Assistance" com mais de 1 ano
    homeless_use_outliers <- outliers_complaint %>%
      filter(Complaint_Type == "Homeless Person Assistance") %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%  # Remover dados sem coordenadas
      filter(Borough != "Unspecified")  %>%  # Remover boroughs não especificados
      filter(Created.Date >= input$date_range[1] & Created.Date <= input$date_range[2])
    
    # Ajustar o shapefile de NYC para incluir informações de "Homeless Person Assistance"
    nyc_boroughs <- nyc_boroughs %>%
      mutate(Most_Common_Building_Use_Outlier = ifelse(
        BoroName %in% homeless_use_outliers$Borough, "Homeless Person Assistance", NA
      ))
    
    # Criar o mapa com as queixas de "Homeless Person Assistance"
    gg <- ggplot() +
      geom_sf(data = nyc_boroughs, color = "black", size = 0.2) +
      geom_point(
        data = homeless_use_outliers,
        aes(x = Longitude, y = Latitude),
        color = "blue", size = 2, alpha = 0.6
      ) +
      labs(
        title = "'Homeless Person Assistance' outliers map distribution",
        x = "Longitude",
        y = "Latitude",
        fill = "Homeless Person Assistance Outliers"
      ) +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  
  
  # Mapas iterativos
  output$ggplot_map_display <- renderPlot({
    map_choice <- input$map_choice
    
    # Escolher o mapa com base na seleção do usuário
    ggplot_map <- switch(
      map_choice,
      "map_outliers" = map_outliers,
      "map_extreme_outliers" = map_extreme_outliers,
      "map_outliers_8760" = map_outliers_8760
    )
    
    # Renderizar o mapa selecionado
    ggplot_map
  })
  
  
  # Mapas iterativos2
  output$ggplot_map_display2 <- renderPlot({
    map_choice2 <- input$map_choice2
    print(map_choice2)  # Verifique o valor de input$map_choice2
    
    if (map_choice2 == "map_outliers2") {
      ggplot_map2 <- map_outliers2
    } else if (map_choice2 == "map_extreme_outliers2") {
      ggplot_map2 <- map_extreme_outliers2
    } else if (map_choice2 == "map_outliers_1year2") {
      ggplot_map2 <- map_outliers_1year2
    }
    
    ggplot_map2  # Exibir o gráfico
  })
  
  
}

# Rodar a aplicação
shinyApp(ui = ui, server = server)