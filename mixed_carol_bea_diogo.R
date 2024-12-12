# Pacotes necessários
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(bslib)
library(data.table)
library(rlang)

########################################## DATA PREPARATION
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

data_bea <- data

data_diogo <- data

data_carol <- data


#####

########################################## DIOGO

#####
# Carregar e preparar os dados
data_d = read.csv("C:/Users/castr/Desktop/Uni/VD/PROJ1/NYC_311_Data_20241009.csv", header=TRUE, sep=";", na.strings=c("", " ", "N/A"))

setDT(data_d)

data_d <- data_d[, .(Created.Date, Closed.Date, Agency, Incident.Address, Incident.Zip, Community.Board, Agency.Name, Complaint.Type, Descriptor, City, Borough, Longitude, Latitude)]

# Formatando datas
data_d[, Created.Date := parse_date_time(Created.Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M"), tz = "UTC")]
data_d[, Closed.Date := parse_date_time(Closed.Date, orders = c("m/d/Y I:M:S p", "m/d/Y H:M"), tz = "UTC")]

data_d[, Resolution.Time := round(as.numeric(difftime(Closed.Date, Created.Date, units = "hours")), 2)]

# Limpar e categorizar Complaint.Type
data_d$Complaint.Type.Clean <- tolower(data_d$Complaint.Type)
data_d$Complaint.Type.Clean <- gsub(".*noise.*", "noise", data_d$Complaint.Type.Clean)
data_d$Complaint.Type.Clean <- gsub(".*highway sign.*", "highway sign", data_d$Complaint.Type.Clean)
data_d$Complaint.Type.Clean <- gsub(".*street sign.*", "street sign", data_d$Complaint.Type.Clean)
data_d$Complaint.Type.Clean <- gsub(".*illegal.*", "illegal", data_d$Complaint.Type.Clean)

# Filtrar datas inválidas
data_d <- data_d %>%
  mutate(Created.Date = as.Date(Created.Date)) %>%
  filter(!is.na(Created.Date) & Borough != "Unspecified" & Borough != "")

# Classificar dias como Dia de Semana ou Fim de Semana
data_d <- data_d %>%
  mutate(Day_Type = case_when(
    wday(Created.Date) %in% c(1, 7) ~ "Weekend/Holidays",
    TRUE ~ "Weekdays"
  ))

# Função para identificar outliers
find_outliers <- function(data_d, group_var, target_var) {
  data_d %>%
    group_by({{ group_var }}) %>%
    summarise(
      Q1 = quantile({{ target_var }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ target_var }}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1
    ) %>%
    mutate(Lower_Bound = Q1 - 1.5 * IQR, Upper_Bound = Q3 + 1.5 * IQR) %>%
    right_join(data_d, by = as.character(rlang::ensym(group_var))) %>%
    filter({{ target_var }} < Lower_Bound | {{ target_var }} > Upper_Bound)
}

# Identificar outliers
outliers_borough <- find_outliers(data_d, Borough, Resolution.Time)
outliers_complaint <- find_outliers(data_d, Complaint.Type, Resolution.Time)
outliers_complaintclean <- find_outliers(data_d, Complaint.Type.Clean, Resolution.Time)
outliers_day_type <- find_outliers(data_d, Day_Type, Resolution.Time)

# Criar subconjuntos de dados para os mapas
outliers_top5 <- outliers_borough %>% head(5)
extreme_outliers_top5 <- outliers_complaint %>% head(5)
outliers_top5_8760 <- outliers_day_type %>% head(5)



# Function to calculate outliers for a given grouping variable
find_ext_outliers <- function(data_d, group_var, target_var) {
  data_d %>%
    group_by({{ group_var }}) %>%
    summarise(
      Q1 = quantile({{ target_var }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ target_var }}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1
    ) %>%
    mutate(Lower_Bound = Q1 - 3 * IQR, Upper_Bound = Q3 + 3 * IQR) %>%
    right_join(data_d, by = as.character(rlang::ensym(group_var))) %>%
    filter({{ target_var }} < Lower_Bound | {{ target_var }} > Upper_Bound) %>%
    select(-c(Q1, Q3, IQR, Lower_Bound, Upper_Bound))  
}

# Identify outliers for each category
ext_outliers_borough <- find_ext_outliers(data_d, Borough, Resolution.Time)
ext_outliers_complaint <- find_ext_outliers(data_d, Complaint.Type, Resolution.Time)
ext_outliers_complaintclean <- find_ext_outliers(data_d, Complaint.Type.Clean, Resolution.Time)
ext_outliers_day_type <- find_ext_outliers(data_d, Day_Type, Resolution.Time)

find_outliers_custom_conditions <- function(data_d, group_var, target_var, lower_limit) {
  data_d %>%
    group_by({{ group_var }}) %>%
    summarise(
      Q1 = quantile({{ target_var }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ target_var }}, 0.75, na.rm = TRUE),
      IQR = Q3 - Q1
    ) %>%
    mutate(Lower_Bound = Q1 - 3 * IQR, Upper_Bound = Q3 + 3 * IQR) %>%
    right_join(data_d, by = as.character(rlang::ensym(group_var))) %>%
    filter({{ target_var }} > lower_limit) %>% 
    select(-c(Q1, Q3, IQR, Lower_Bound, Upper_Bound))  
}


# Identificar outliers para valores superiores a 8760 - 1 ano
outliers_borough_8760 <- find_outliers_custom_conditions(data_d, Borough, Resolution.Time, 8760)
outliers_complaint_8760 <- find_outliers_custom_conditions(data_d, Complaint.Type, Resolution.Time, 8760)
outliers_complaintclean_8760 <- find_outliers_custom_conditions(data_d, Complaint.Type.Clean, Resolution.Time, 8760)
outliers_day_type_8760 <- find_outliers_custom_conditions(data_d, Day_Type, Resolution.Time, 8760)

# Carregar os dados dos boroughs
nyc_boroughs <- st_read("C:/Users/castr/Desktop/Uni/VD/PROJ1/nybb.shp") %>%
  st_transform(crs = 4326)

top5_complaints_outliers <- outliers_complaint %>%
  count(Complaint.Type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  rename(Most_Common_Outlier = Complaint.Type)

# Outliers extremos
top5_complaints_extreme_outliers <- ext_outliers_complaint %>%
  count(Complaint.Type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  rename(Most_Common_Extreme_Outlier = Complaint.Type)

# Outliers com mais de 1 ano
top5_complaints_outliers_8760 <- outliers_complaint_8760 %>%
  count(Complaint.Type, sort = TRUE) %>%
  slice_max(n, n = 5) %>%
  rename(Most_Common_1year = Complaint.Type)


# 2. Filtrar os Dados para os Top 5 Tipos de Queixa
# Outliers gerais
outliers_top5 <- outliers_complaint %>%
  filter(Complaint.Type %in% top5_complaints_outliers$Most_Common_Outlier) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Borough != "Unspecified")

# Outliers extremos
extreme_outliers_top5 <- ext_outliers_complaint %>%
  filter(Complaint.Type %in% top5_complaints_extreme_outliers$Most_Common_Extreme_Outlier) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Borough != "Unspecified")

# Outliers com mais de 1 ano
outliers_top5_8760 <- outliers_complaint_8760 %>%
  filter(Complaint.Type %in% top5_complaints_outliers_8760$Most_Common_1year) %>%
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
    Most_Common_Extreme_Outlier = Complaint.Type[which.max(table(Complaint.Type))],
    Total_Outliers = n()
  )

nyc_boroughs_extreme_outliers <- nyc_boroughs %>%
  left_join(ext_outliers_summary, by = c("BoroName" = "Borough"))

# Outliers com mais de 1 ano
nyc_boroughs_outliers_8760 <- nyc_boroughs %>%
  left_join(outliers_top5_8760, by = c("BoroName" = "Borough"))


# Criar uma paleta de cores para os tipos de queixa
all_Complaint.Types <- unique(c(
  outliers_top5$Complaint.Type,
  extreme_outliers_top5$Complaint.Type,
  outliers_top5_8760$Complaint.Type
))

color_palette <- setNames(brewer.pal(n = length(all_Complaint.Types), "Paired"), all_Complaint.Types)

# Criar os mapas
map_outliers <- ggplot() +
  geom_sf(data = nyc_boroughs_outliers, color = "black", size = 0.2) +
  geom_point(data = outliers_top5, aes(x = Longitude, y = Latitude, color = Complaint.Type), size = 2, alpha = 0.4) +
  labs(
    title = "Top 5 outliers in New York map",
    color = "Complaint type (Top 5)"
  ) +
  theme_minimal() +
  scale_color_manual(values = color_palette)

map_extreme_outliers <- ggplot() +
  geom_sf(data = nyc_boroughs_extreme_outliers, color = "black", size = 0.2) +
  geom_point(data = extreme_outliers_top5, aes(x = Longitude, y = Latitude, color = Complaint.Type), size = 2, alpha = 0.4) +
  labs(
    title = "Top 5 extreme outliers in New York map",
    color = "Complaint type (Top 5)"
  ) +
  theme_minimal() +
  scale_color_manual(values = color_palette)

map_outliers_8760 <- ggplot() +
  geom_sf(data = nyc_boroughs_outliers_8760, color = "black", size = 0.2) +
  geom_point(data = outliers_top5_8760, aes(x = Longitude, y = Latitude, color = Complaint.Type), size = 2, alpha = 0.4) +
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

# Padronizar nomes 
filtered_outliers_complaint$Borough <- tolower(filtered_outliers_complaint$Borough)
ext_outliers_complaint$Borough <- tolower(ext_outliers_complaint$Borough)
nyc_boroughs$BoroName <- tolower(nyc_boroughs$BoroName)

# Passo 2: Determinar os tipos de outliers mais comuns por Borough
outliers_by_borough <- filtered_outliers_complaint %>%
  group_by(Borough, Complaint.Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Borough, desc(Count)) %>%
  group_by(Borough) %>%
  slice_max(Count, n = 1) %>%
  rename(Most_Common_Outlier = Complaint.Type)

extreme_outliers_by_borough <- ext_outliers_complaint %>%
  group_by(Borough, Complaint.Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Borough, desc(Count)) %>%
  group_by(Borough) %>%
  slice_max(Count, n = 1) %>%
  rename(Most_Common_Extreme_Outlier = Complaint.Type)

# Passo 3: Mesclar as informações com o shapefile
nyc_boroughs <- nyc_boroughs %>%
  left_join(outliers_by_borough, by = c("BoroName" = "Borough")) %>%
  left_join(extreme_outliers_by_borough, by = c("BoroName" = "Borough"))



# Passo 4: Criar os gráficos

# Paleta de cores consistente para ambos os gráficos
# Combinar os tipos de outliers e extreme outliers em um vetor único
all_Complaint.Types <- unique(c(
  nyc_boroughs$Most_Common_Outlier,
  nyc_boroughs$Most_Common_Extreme_Outlier
))

# Criar uma paleta de cores personalizada 
library(RColorBrewer)
set.seed(42)  
color_palette <- setNames(brewer.pal(length(all_Complaint.Types), "Set3"), all_Complaint.Types)

# Mapa 1: Outliers mais comuns
map_outliers2 <- ggplot(nyc_boroughs) +
  geom_sf(aes(fill = Most_Common_Outlier), color = "black", size = 0.3) +
  scale_fill_manual(values = color_palette, na.value = "grey80") +
  labs(
    title = "Most common outlier by borough",
    fill = "Tipo de Outlier"
  ) +
  theme_minimal()

# Mapa 2: Extreme outliers mais comuns
map_extreme_outliers2 <- ggplot(nyc_boroughs) +
  geom_sf(aes(fill = Most_Common_Extreme_Outlier), color = "black", size = 0.3) +
  scale_fill_manual(values = color_palette, na.value = "grey80") +
  labs(
    title = "Most common extreme outlier by borough",
    fill = "Tipo de Extreme Outlier"
  ) +
  theme_minimal()


# Determinar o tipo de outlier mais comum por borough para >1 ano
common_outlier_8760 <- outliers_complaint_8760 %>%
  group_by(Borough, Complaint.Type) %>%
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
  rename(Most_Common_Outlier_1Year = Complaint.Type)

# Paleta de cores consistente para todos os gráficos
all_Complaint.Types <- unique(c(
  nyc_boroughs$Most_Common_Outlier,
  nyc_boroughs$Most_Common_Extreme_Outlier,
  nyc_boroughs$Most_Common_Outlier_1Year
))

# Criar a paleta de cores
library(RColorBrewer)
set.seed(42)
color_palette <- setNames(brewer.pal(length(all_Complaint.Types), "Set3"), all_Complaint.Types)

# Mapa 3: Outliers com mais de 1 ano
map_outliers_1year2 <- ggplot(data = nyc_boroughs) +
  geom_sf(aes(fill = Most_Common_Outlier_1Year), color = "black", size = 0.3) +
  scale_fill_manual(values = color_palette, na.value = "grey80") +
  labs(
    title = "Most common observation with more than 1 year resolution time by borough",
    fill = "Resolution Type (> 1 Ano)"
  ) +
  theme_minimal()



############################################### CAROL


data$Period.Of.Day <- case_when(
  hour(data$Created.Date) >= 0 & hour(data$Created.Date) < 6  ~ "Early Morning",
  hour(data$Created.Date) >= 6 & hour(data$Created.Date) < 12 ~ "Morning",
  hour(data$Created.Date) >= 12 & hour(data$Created.Date) < 18 ~ "Afternoon",
  TRUE ~ "Nigth"
)


############################################## UI

#####
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "NYC 311 Dashboard"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
      .main-sidebar {
        position: fixed;
        max-height: 100%;
        overflow-y: auto;
      }
      .content-wrapper, .main-footer {
        margin-left: 230px; /* Width of the sidebar */
      }
    "))
    ),
    sidebarMenu(
      ############################ BEA
      conditionalPanel(
        condition = "input.tab_selector == 'Complaint Type'",
        sliderInput(inputId = "num_top_cases", label = "Number of Top Complaint Types:", value = 10, min = 5, max = 50),
        
        # Botão do log
        checkboxInput(inputId = "log_filter", label = "Log Filter", value = FALSE),
        
        # Isto é para mostrar só se tiver escolhido o log
        conditionalPanel(
          condition = "input.log_filter == true",
          sliderInput(inputId = "log_range_filter", label = "Filter by Log Range (min difference):", min = 0, max = 5, value = 0, step = 0.1)
        ),
        uiOutput("complaint_type_selector"),
        uiOutput("borough_selector"),
        dateRangeInput(inputId = "date_range_b", label = "Select Date Range:", start = min(data_bea$Created.Date, na.rm = TRUE), end = max(data_bea$Created.Date, na.rm = TRUE)),
        radioButtons(inputId = "time_resolution", label = "Choose resolution time representation:", choices = list("Daily" = "daily", "Hourly" = "hourly"), selected = "daily")
      ),
      conditionalPanel(
        condition = "input.tab_selector == 'Agency'",
        checkboxInput(inputId = "outliers_a", label = "Remove Outliers", value = FALSE),
        selectInput(inputId = "selected_agencies", 
                    label = "Select Agencies:", 
                    choices = unique((data_bea %>% filter(!is.na(Agency), !is.na(Borough), Borough != 'unspecified'))$Agency), 
                    selected = unique((data_bea %>% filter(!is.na(Agency), !is.na(Borough), Borough != 'unspecified'))$Agency), 
                    multiple = TRUE),
        checkboxGroupInput(inputId = "selected_boroughs_agency", 
                           label = "Select Boroughs:", 
                           choices = unique((data_bea %>% filter(!is.na(Borough), Borough != 'unspecified'))$Borough), 
                           selected = unique((data_bea %>% filter(!is.na(Borough), Borough != 'unspecified'))$Borough)),
      ),
      ############################ DIOGO
      conditionalPanel(
        condition = "input.tab_selector == 'Outliers'",
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
              "date_range_d",
              "Date Range:",
              start = min(data_d$Created.Date),
              end = max(data_d$Created.Date)
            )
          ),
          
          conditionalPanel(
            condition = "input.menu === 'outliers_chart'",
            selectInput("outliers_group", "Outliers by:",
                        choices = c(
                          "Borough" = "Borough",
                          "Complaint Type (Top 10)" = "Complaint.Type",
                          "Complaint Type - Grouped (Top 10)" = "Complaint.Type.Clean",
                          "Day Type" = "Day_Type"
                        ),
                        selected = "Borough")
          ),
          conditionalPanel(
            condition = "input.menu === 'outliers_chart2'",
            selectInput("outliers_group", "Outliers by:",
                        choices = c(
                          "Borough" = "Borough",
                          "Complaint Type (Top 10)" = "Complaint.Type",
                          "Complaint Type - Grouped (Top 10)" = "Complaint.Type.Clean",
                          "Day Type" = "Day_Type"
                        ),
                        selected = "Borough")
          ),
          conditionalPanel(
            condition = "input.menu === 'ext_outliers_chart'",
            selectInput("ext_outliers_group", "Extreme Outliers by:",
                        choices = c(
                          "Borough" = "Borough",
                          "Complaint Type (Top 10)" = "Complaint.Type",
                          "Complaint Type - Grouped (Top 10)" = "Complaint.Type.Clean",
                          "Day Type" = "Day_Type"
                        ),
                        selected = "Borough")
          ),
          conditionalPanel(
            condition = "input.menu === 'outliers_chart_8760'",
            selectInput("outliers_group_8760", "Observations (> 1 year) by:",
                        choices = c(
                          "Borough" = "Borough",
                          "Complaint Type (Top 10)" = "Complaint.Type",
                          "Complaint Type - Grouped (Top 10)" = "Complaint.Type.Clean",
                          "Day Type" = "Day_Type"
                        ),
                        selected = "Borough")
          )
        )
      ),
      ############################### CAROL
      conditionalPanel(
        condition = "input.tab_selector == 'Time'",
        menuItem("Filtros", tabName = "filtros", icon = icon("filter")),
        dateRangeInput("date_range_c", "Intervalo de Datas", start = min(data$Created.Date), end = max(data$Created.Date)),
        selectInput("complaint_type", "Tipo de Reclamação", choices = c("Todos", unique(data$Complaint.Type.Clean))),
        selectInput("borough", "Bairro", choices = c("Todos", unique(data$Borough))),
        selectInput("period_filter", "Período do Dia", choices = c("Todos", "Morning", "Afternoon", "Nigth", "Early Morning")),
        selectInput("weekday_filter", "Dia da Semana", choices = c("Todos", "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
      )
    )
  ),
  dashboardBody(
    tabsetPanel(
      id = "tab_selector",
      ############################## BEA
      tabPanel(
        "Complaint Type",
        plotlyOutput("heatmap_ct"),
        plotlyOutput("borough_lineplot"),
        plotlyOutput("time_based_lineplot"),
        plotlyOutput('bubble')
      ),
      tabPanel(
        "Agency",
        plotlyOutput("heatmap_a"),
        plotlyOutput("agency_lineplot"), 
        plotOutput('violins')
      ),
      ####################### DIOGO
      tabPanel(
        "Outliers",
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
                width = 12, 
                plotlyOutput("boxplot_chart") 
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
      ),
      ########################## CAROL
      tabPanel(
        "Time",
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
  )
)


server <- function(input, output) {
  
  ################################### BEA
  processed_data <- reactive({
    data_c <- data_bea %>%
      filter(Borough != 'unspecified', !is.na(Borough), Resolution.Time >= 0)
    
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
    
    avg_resolution_by_agency <- data_c %>%
      group_by(Agency, Borough) %>%
      summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
    
    summary_data_a <- avg_resolution_by_agency %>%
      mutate(Log.Avg.Resolution.Time = log1p(Avg.Resolution.Time)) %>%
      filter(!is.na(Agency), !is.na(Borough), Borough != 'unspecified')
    
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
    
    no_outliers <- data_c %>%
      group_by(Agency) %>%
      filter(Resolution.Time > (quantile(Resolution.Time, 0.25) - 1.5 * IQR(Resolution.Time)) & 
               Resolution.Time < (quantile(Resolution.Time, 0.75) + 1.5 * IQR(Resolution.Time))) %>%
      ungroup()
    
    avg_resolution_by_agency_no <- no_outliers %>%
      group_by(Agency, Borough) %>%
      summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
    
    summary_data_a_no <- avg_resolution_by_agency_no %>%
      mutate(Log.Avg.Resolution.Time = log1p(Avg.Resolution.Time)) %>%
      filter(!is.na(Agency), !is.na(Borough), Borough != 'unspecified')
    
    
    list(
      full_data = summary_data,
      filtered_data = filtered_summary_data,
      daily_data = filtered_data,
      full_data_a = summary_data_a,
      daily_data_a = data_c,
      no_outliers = no_outliers,
      full_data_a_no = summary_data_a_no
    )
  })
  
  output$complaint_type_selector <- renderUI({
    req(processed_data()$filtered_data)
    complaint_types <- unique(processed_data()$filtered_data$Complaint.Type.Clean)
    selectInput(inputId = "selected_complaint_types", 
                label = "Select Complaint Type:", 
                choices = unique(complaint_types), 
                selected = unique(complaint_types), 
                multiple = TRUE)
  })
  
  output$borough_selector <- renderUI({
    boroughs <- unique(processed_data()$filtered_data$Borough)
    checkboxGroupInput(inputId = "selected_boroughs", label = "Select Boroughs:", choices = boroughs, selected = boroughs)
  })
  
  output$heatmap_ct <- renderPlotly({
    plot_data <- processed_data()$filtered_data
    all_data <- processed_data()$full_data
    
    if (input$log_filter == TRUE) {
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
    } else {
      pl <- ggplot(all_data, aes(x = Borough, y = Complaint.Type.Clean, fill = Avg.Resolution.Time)) +
        geom_tile() +
        scale_fill_viridis_c() +
        labs(
          title = "Heatmap of Log-Transformed Avg Resolution Time",
          x = "Borough",
          y = "Complaint Type",
          fill = "Avg Res Time"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    ggplotly(pl)
    
  })
  
  output$heatmap_a <- renderPlotly({
    if(input$outliers_a == FALSE) {
      plot_data <- processed_data()$full_data_a %>%
        filter(
          Borough %in% input$selected_boroughs_agency,
          Agency %in% input$selected_agencies
        )
    } else {
      plot_data <- processed_data()$full_data_a_no %>%
        filter(
          Borough %in% input$selected_boroughs_agency,
          Agency %in% input$selected_agencies
        )
    }
    
    
    pl <- ggplot(plot_data, aes(y = Agency, x = Borough, fill = Avg.Resolution.Time)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(
        title = "Heatmap of Avg Resolution Time",
        x = "Borough",
        y = "Agency",
        fill = "Avg Res Time"
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
      geom_line(size = 0.7) +
      geom_point(size = 1.3) +
      scale_color_viridis_d() + 
      labs(title = "Line Plot of Avg Resolution Time by Borough and Complaint Type", 
           x = "Borough", 
           y = "Avg Resolution Time (hours)", 
           color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "bottom") 
    
    
    ggplotly(pl)
  })
  
  output$time_based_lineplot <- renderPlotly({
    filtered_data <- processed_data()$daily_data %>%
      filter(
        Complaint.Type.Clean %in% input$selected_complaint_types,
        Borough %in% input$selected_boroughs,
        Created.Date.Day >= input$date_range_b[1],
        Created.Date.Day <= input$date_range_b[2]
      )
    
    if (input$time_resolution == "daily") {
      plot_data <- filtered_data %>%
        group_by(Created.Date.Day, Borough, Complaint.Type.Clean) %>%
        summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
      x_label <- "Day"
      title <- "Daily Avg Resolution Time by Borough and Complaint Type"
    } else {
      filtered_data <- filtered_data %>%
        mutate(Created.Date = as.POSIXct(Created.Date))
        
      plot_data <- filtered_data %>%
        mutate(Created.Hour = floor_date(Created.Date, "hour")) %>%
        group_by(Created.Hour, Borough, Complaint.Type.Clean) %>%
        summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")
      x_label <- "Hour"
      title <- "Hourly Avg Resolution Time by Borough and Complaint Type"
    }
    
    pl <- ggplot(plot_data, aes(x = if (input$time_resolution == "daily") Created.Date.Day else Created.Hour,
                                y = Avg.Resolution.Time, color = Complaint.Type.Clean, group = Complaint.Type.Clean)) +
      geom_line(size = 0.5) +
      facet_wrap(~ Borough) +
      labs(title = title, x = x_label, y = "Avg Resolution Time (hours)", color = "Complaint Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      guides(color = guide_legend(override.aes = list(linetype = 1))) 
    
    
    ggplotly(pl)
  })
  
  output$agency_lineplot <- renderPlotly({
    if(input$outliers_a == FALSE) {
      plot_data <- processed_data()$full_data_a %>%
        filter(
          Borough %in% input$selected_boroughs_agency,
          Agency %in% input$selected_agencies
        )
    } else {
      plot_data <- processed_data()$full_data_a_no %>%
        filter(
          Borough %in% input$selected_boroughs_agency,
          Agency %in% input$selected_agencies
        )
    }
    
    
    pl <- ggplot(plot_data, aes(x = Borough, y = Avg.Resolution.Time, color = Agency, group = Agency)) +
      geom_line(size = 0.7) +
      geom_point(size = 1.3) +
      scale_color_viridis_d() + 
      labs(title = "Line Plot of Avg Resolution Time by Borough and Agency", 
           x = "Borough", 
           y = "Avg Resolution Time (hours)", 
           color = "Agency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), 
            legend.position = "bottom") 
    
    
    
    
    ggplotly(pl)
  })
  
  output$bubble <- renderPlotly({
    plot_data <- processed_data()$daily_data_a %>%
      filter(Complaint.Type.Clean %in% input$selected_complaint_types)
    
    bubble_data <- plot_data %>%
      group_by(Resolution.Time, Complaint.Type.Clean, Agency) %>%
      summarise(Count = n(), .groups = "drop")
    
    bubble_data <- bubble_data %>%
      mutate(Resolution.Time.Days = Resolution.Time %/% 24)
    
    
    
    pl <- ggplot(bubble_data, aes(x = Resolution.Time.Days, y = Complaint.Type.Clean, size = Count, color = Agency)) +
      geom_point(alpha = 0.7) +
      scale_size_continuous(range = c(2, 10)) +
      labs(
        title = "Bubble Chart of Resolution Time vs Complaints",
        x = "Resolution Time (days)",
        y = "Complaint Type"
      ) +
      theme_minimal()
    
    ggplotly(pl)
    
  })
  
  output$violins <- renderPlot({
    if(input$outliers_a == FALSE) {
      plot_data <- processed_data()$daily_data_a %>%
        filter(
          Borough %in% input$selected_boroughs_agency,
          Agency %in% input$selected_agencies
        )
    } else {
      plot_data <- processed_data()$no_outliers %>%
        filter(
          Borough %in% input$selected_boroughs_agency,
          Agency %in% input$selected_agencies
        )
    }
    ggplot(plot_data, aes(x = Borough, y = Resolution.Time, fill = Agency)) + 
      geom_violin(trim = FALSE, position = position_dodge(width = 0.8), alpha = 0.5) + 
      geom_boxplot(width = 0.3, position = position_dodge(width = 0.8), alpha = 0.5) +
      labs(title = "Boxplots of Agency and Borough", 
           x = "Borough", 
           y = "Resolution Time (hours)", 
           fill = "Borough") + 
      facet_wrap(~ Agency, scales = "free_y") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),  # Rotation and bold styling for x-axis
        axis.title.x = element_text(face = "bold"),  # Bold x-axis title
        legend.position = "bottom"
      )
    
    
  })
  
  ################################ DIOGO
  
  get_top10 <- function(data_d, group_var) {
    data_d %>%
      count({{ group_var }}, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      pull({{ group_var }})
  }
  
  # Dados filtrados
  filtered_data_diogo <- reactive({
    data_d %>%
      filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
  })
  
  # Gráfico de Outliers
  output$outliers_plot <- renderPlotly({
    selected_group <- input$outliers_group
    
    # Filtrar os Top 10, se necessário
    get_top10_filtered <- function(data_d, group_var) {
      top10_values <- data_d %>%
        count({{ group_var }}, sort = TRUE) %>%
        slice_head(n = 10) %>%
        pull({{ group_var }})
      data_d %>% filter({{ group_var }} %in% top10_values)
    }
    
    # Selecionar os dados de acordo com o grupo escolhido
    original_data <- filtered_data_diogo()
    
    if (selected_group %in% c("Complaint.Type", "Complaint.Type.Clean")) {
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
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]),
      "Complaint.Type" = outliers_complaint %>%
        filter(Complaint.Type %in% get_top10_filtered(filtered_data_diogo(), Complaint.Type)$Complaint.Type),
      "Complaint.Type.Clean" = outliers_complaintclean %>%
        filter(Complaint.Type.Clean %in% get_top10_filtered(filtered_data_diogo(), Complaint.Type.Clean)$Complaint.Type.Clean),
      "Day_Type" = outliers_day_type %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
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
    
    # Filtrar os dados de acordo com o grupo escolhido
    original_data <- filtered_data_diogo()
    
    # Contagens de outliers para todos os dados, sem restrição de Top 10
    outlier_data <- switch(
      selected_group,
      "Borough" = outliers_borough %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]),
      "Complaint.Type" = outliers_complaint %>%
        filter(Complaint.Type %in% unique(original_data$Complaint.Type))%>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]), 
      "Complaint.Type.Clean" = outliers_complaintclean %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]) %>%
        filter(Complaint.Type.Clean %in% unique(original_data$Complaint.Type.Clean)), 
      "Day_Type" = outliers_day_type %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
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
    # Filtrar os dados de acordo com o grupo escolhido
    original_data <- filtered_data_diogo()
    
    # Contagens de outliers para todos os dados
    ext_outlier_data <- switch(
      selected_group,
      "Borough" = ext_outliers_borough %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]),
      "Complaint.Type" = ext_outliers_complaint %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]) %>%
        filter(Complaint.Type %in% unique(original_data$Complaint.Type)), 
      "Complaint.Type.Clean" = ext_outliers_complaintclean %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]) %>%
        filter(Complaint.Type.Clean %in% unique(original_data$Complaint.Type.Clean)), 
      "Day_Type" = ext_outliers_day_type %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
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
    
    # Filtrar os dados de acordo com o grupo escolhido
    original_data <- filtered_data_diogo()
    
    # Contagens de outliers para todos os dados
    outlier_data_8760 <- switch(
      selected_group,
      "Borough" = outliers_borough_8760 %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]),
      "Complaint.Type" = outliers_complaint_8760 %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]) %>%
        filter(Complaint.Type %in% unique(original_data$Complaint.Type)), 
      "Complaint.Type.Clean" = outliers_complaintclean_8760 %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]) %>%
        filter(Complaint.Type.Clean %in% unique(original_data$Complaint.Type.Clean)),  
      "Day_Type" = outliers_day_type_8760 %>%
        filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
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
    
    filtered_boxplot_data <- filtered_data_diogo() %>%
      filter(Complaint.Type %in% c("Building/Use", "New Tree Request", "General Construction/Plumbing", "Overgrown Tree/Branches"))
    
    gg <- ggplot(filtered_boxplot_data, aes(x = Complaint.Type, y = Resolution.Time, fill = Complaint.Type)) +
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
      filter(Complaint.Type %in% c("Building/Use", "New Tree Request", "General Construction/Plumbing", "Overgrown Tree/Branches")) %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%  
      filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
    
    gg <- ggplot() +
      geom_sf(data = nyc_boroughs, fill = "gray80", color = "black", size = 0.2) +
      geom_point(data = map_data, aes(x = Longitude, y = Latitude, color = Complaint.Type), size = 2, alpha = 0.3) +
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
        Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2]
      )
    
    # Selecionar os 10 tipos de queixa mais frequentes no intervalo
    top_10_complaints <- filtered_outliers_complaint %>%
      count(Complaint.Type, sort = TRUE) %>%
      slice_head(n = 10) %>%
      pull(Complaint.Type)
    
    # Filtrar para o top 10 e excluir Borough = "Unspecified"
    outliers_complaint_top10 <- filtered_outliers_complaint %>%
      filter(Complaint.Type %in% top_10_complaints, Borough != "Unspecified")
    
    # Criar o gráfico
    gg <- ggplot(outliers_complaint_top10, aes(x = Complaint.Type, fill = Borough)) +
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
    print(outliers_complaint)
    # Filtrar os dados para "Homeless Person Assistance" com mais de 1 ano
    homeless_use_outliers <- outliers_complaint %>%
      filter(Complaint.Type == "Homeless Person Assistance") %>%
      filter(!is.na(Latitude) & !is.na(Longitude)) %>%  # Remover dados sem coordenadas
      filter(Borough != "Unspecified")  %>%  # Remover boroughs não especificados
      filter(Created.Date >= input$date_range_d[1] & Created.Date <= input$date_range_d[2])
    
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
    
    if (map_choice2 == "map_outliers2") {
      ggplot_map2 <- map_outliers2
    } else if (map_choice2 == "map_extreme_outliers2") {
      ggplot_map2 <- map_extreme_outliers2
    } else if (map_choice2 == "map_outliers_1year2") {
      ggplot_map2 <- map_outliers_1year2
    }
    
    ggplot_map2  # Exibir o gráfico
  })
  
 ######################################################## CAROL
  
  filtered_data_carol <- reactive({
    data_filtered_carol <- data
    
    if (!is.null(input$date_range_c)) {
      data_filtered_carol <- data_filtered_carol %>% 
        filter(Created.Date >= input$date_range_c[1] & Created.Date <= input$date_range_c[2])
    }
    
    if (input$complaint_type != "Todos") {
      data_filtered_carol <- data_filtered_carol %>% filter(Complaint.Type.Clean == input$complaint_type)
    }
    
    if (input$borough != "Todos") {
      data_filtered_carol <- data_filtered_carol %>% filter(Borough == input$borough)
    }
    
    if (input$period_filter != "Todos") {
      data_filtered_carol <- data_filtered_carol %>% filter(Period.Of.Day == input$period_filter)
    }
    
    if (input$weekday_filter != "Todos") {
      data_filtered_carol <- data_filtered_carol %>% filter(weekdays(Created.Date) == input$weekday_filter)
    }
    
    data_filtered_carol$Weekday <- weekdays(data_filtered_carol$Created.Date)
    
    data_filtered_carol$Hour <- hour(data_filtered_carol$Created.Date)
    
    return(data_filtered_carol)
  })
  
  # Gráfico de Violino
  output$violino <- renderPlotly({
    violin_data <- filtered_data_carol()
    
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
    heatmap_data <- filtered_data_carol() %>%
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
    line_data <- filtered_data_carol() %>%
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
    polar_data <- filtered_data_carol() %>%
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
    complaint_data <- filtered_data_carol() %>%
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
    thursday_data <- filtered_data_carol()
    
    thursday_data <- thursday_data %>%
      filter(Weekday == "Thursday") %>%
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


