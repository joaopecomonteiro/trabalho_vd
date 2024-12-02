library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)
library(data.table)
library(forcats)
library(dbscan)
library(ggmap)
library(purrr)
library(ggalluvial)


data = read.csv("~/Documents/UP/24 25/1ºsemestre/VisualizaçãoDados/Projeto/NYC_311_Data_20241009.csv", header=TRUE, sep=";", na.strings=c("", " ", "N/A"))

setDT(data)

data = data[, .(Created.Date, Closed.Date, Agency, Agency.Name, Complaint.Type, Descriptor, City, Borough, Longitude, Latitude, Status, Street.Name)]

data = data %>%
  mutate(across(where(is.character), tolower))

data[, Created.Date := parse_date_time(`Created.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]
data[, Closed.Date := parse_date_time(`Closed.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]

data[, Time.Open := as.period(difftime(Closed.Date, Created.Date), units = "hours")]


set(data, j = "Resolution.Time", value = round(as.numeric(difftime(data$Closed.Date, data$Created.Date), units = "hours"), 2))

data <- data %>%
  filter(Created.Date < '2016-09-30')

'''
# Calculate the lower and upper bounds for the outlier detection
lower_bound <- 0
upper_bound <- quantile(data$Resolution.Time, 0.75, na.rm = TRUE) + 1.5 * IQR(data$Resolution.Time, na.rm = TRUE)

# Filter out the outliers
data <- data %>%
  filter( Resolution.Time >= 0 &
            Resolution.Time >= lower_bound & Resolution.Time <= upper_bound
  )'''


# Identificar e tratar valores ausentes
data <- data %>%
  mutate(
    Resolved = !is.na(Closed.Date) # TRUE se foi resolvida, FALSE caso contrário
  )

# Separar reclamações não resolvidas para análise ou tratamento adicional
unresolved_cases <- data %>%
  filter(is.na(Closed.Date))

# Visualizar resultados
print("Dataset completo:")
print(data)

print("Reclamações não resolvidas:")
print(unresolved_cases)


############################################################

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

############################################################


# 1. Agrupar por período do dia
data <- data %>%
  mutate(
    Period.Of.Day = case_when(
      hour(Created.Date) >= 6 & hour(Created.Date) < 12 ~ "Morning",
      hour(Created.Date) >= 12 & hour(Created.Date) < 18 ~ "Afternoon",
      hour(Created.Date) >= 18 & hour(Created.Date) < 24 ~ "Evening",
      TRUE ~ "Night"
    )
  )

# Análise por período do dia
by_period_of_day <- data %>%
  group_by(Period.Of.Day) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
            Count = n())

# 2. Agrupar por dia da semana
data <- data %>%
  mutate(Weekday = weekdays(Created.Date))

by_weekday <- data %>%
  group_by(Weekday) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
            Count = n())

# 3. Agrupar por mês ou estação do ano
data <- data %>%
  mutate(
    Month = format(Created.Date, "%B"), # Nome completo do mês
    Season = case_when(
      month(Created.Date) %in% c(12, 1, 2) ~ "Winter",
      month(Created.Date) %in% c(3, 4, 5) ~ "Spring",
      month(Created.Date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  )

by_season <- data %>%
  group_by(Season) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
            Count = n())

# Visualizar resultados
print("Análise por período do dia:")
print(by_period_of_day)

print("Análise por dia da semana:")
print(by_weekday)

print("Análise por estação do ano:")
print(by_season)


# Calcular tempo de resolução e outras colunas úteis
data <- data %>%
  mutate(
    Resolution.Time = as.numeric(difftime(Closed.Date, Created.Date, units = "hours")),
    Resolved = !is.na(Closed.Date), # Indicador de resolução
    Weekday = weekdays(Created.Date),
    Period.Of.Day = case_when(
      hour(Created.Date) >= 6 & hour(Created.Date) < 12 ~ "Morning",
      hour(Created.Date) >= 12 & hour(Created.Date) < 18 ~ "Afternoon",
      hour(Created.Date) >= 18 & hour(Created.Date) < 24 ~ "Evening",
      TRUE ~ "Night"
    ),
    Month = format(Created.Date, "%B"),
    Season = case_when(
      month(Created.Date) %in% c(12, 1, 2) ~ "Winter",
      month(Created.Date) %in% c(3, 4, 5) ~ "Spring",
      month(Created.Date) %in% c(6, 7, 8) ~ "Summer",
      TRUE ~ "Fall"
    )
  )

#VISUALIZAÇÕES

# 1.Distribuição dos Tempos de Resolução
ggplot(data %>% filter(!is.na(Resolution.Time)), aes(x = Resolution.Time)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribuição dos Tempos de Resolução", x = "Tempo de Resolução (horas)", y = "Frequência") +
  theme_minimal()

# 2. Boxplots - Por Dia da Semana
ggplot(data %>% filter(!is.na(Resolution.Time)), aes(x = Weekday, y = Resolution.Time, fill = Weekday)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 1) +
  labs(title = "Dispersão dos Tempos de Resolução por Dia da Semana", x = "Dia da Semana", y = "Tempo de Resolução (horas)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#com interatividade USAR ESTEEEEEE!!!!!!
# Gráfico de violino interativo com plotly
interactive_violin <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  plot_ly(
    x = ~Weekday,
    y = ~Resolution.Time,
    split = ~Weekday, 
    type = 'violin',
    box = list(visible = TRUE),  
    meanline = list(visible = TRUE),  #linha - a média
    points = "all",  
    jitter = 0.3,  
    scalemode = "width"  
  ) %>%
  layout(
    title = "Distribuição dos Tempos de Resolução por Dia da Semana",
    xaxis = list(title = "Dia da Semana", categoryorder = "array",
                 categoryarray = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    yaxis = list(title = "Tempo de Resolução (horas)")
  )

# Exibir o gráfico
interactive_violin

# 3. Heatmap Temporal - duração média de resolução de queixas por dia da semana e hora do dia
heatmap_data <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  mutate(Hour = hour(Created.Date)) %>%
  group_by(Weekday, Hour) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Weekday = factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

ggplot(heatmap_data, aes(x = Hour, y = Weekday, fill = Average.Resolution.Time)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(title = "Heatmap: Tempo Médio de Resolução", x = "Hora do Dia", y = "Dia da Semana", fill = "Tempo Médio (h)") +
  theme_minimal()

'''
as áreas destacadas a amarelo indicam horários com maior tempo médio de resolução
há tempos mais longos nas quintas feiras durante a manhã 
para tentar perceber o porquê --- ver o numero de queixas recebidas nesse horario;
ver a natureza das queixas, se são mais complexas de se resolver.
'''

#com interatividade -------- USAR ESTEEEEEE!!!!!!
interactive_heatmap <- plot_ly(heatmap_data, x = ~Hour, y = ~Weekday, z = ~Average.Resolution.Time,
                               type = "heatmap", colors = c("lightblue", "blue")) %>%
  layout(title = "Heatmap Interativo: Tempo Médio de Resolução",
         xaxis = list(title = "Hora do Dia"),
         yaxis = list(title = "Dia da Semana"))
interactive_heatmap

'''
as áreas destacadas a azul escuro indicam horários com maior tempo médio de resolução
há tempos mais longos nas quintas feiras durante a manhã 
para tentar perceber o porquê --- ver o numero de queixas recebidas nesse horario;
ver a natureza das queixas, se são mais complexas de se resolver.
'''

# 4. Série Temporal - Tempo Médio ao Longo dos Dias
time_series_data <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  mutate(Date = as.Date(Created.Date)) %>%
  group_by(Date) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE)) %>%
  ungroup()

ggplot(time_series_data, aes(x = Date, y = Average.Resolution.Time)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Série Temporal: Tempo Médio de Resolução", x = "Data", y = "Tempo Médio de Resolução (horas)") +
  theme_minimal()

#com interatividade! ---- USAR ESTEEEEEE!!!!!!
interactive_line <- time_series_data %>%
  plot_ly(x = ~Date, y = ~Average.Resolution.Time, 
          type = 'scatter', mode = 'lines+markers', 
          line = list(color = 'darkgreen', width = 3),  # Linhas mais grossas
          marker = list(size = 8, color = 'blue', symbol = 'circle')) %>%  # Bolinhas maiores
  layout(title = "Tempo Médio de Resolução ao Longo do Tempo",
         xaxis = list(
           title = "Data",
           tickformat = "%d-%m",  # Formato de data (DD-MM)
           tickangle = 45,
           tickmode = 'array',  # Mostrar todos os dias
           tickvals = time_series_data$Date  # Usar todas as datas
         ),
         yaxis = list(title = "Tempo Médio (horas)"))  # Título do eixo Y

interactive_line



# 1. Gráfico de Barras Empilhadas
stacked_data <- data %>%
  mutate(Time.Bins = cut(Resolution.Time, breaks = c(0, 5, 10, 24, 48, Inf), 
                         labels = c("0-5h", "5-10h", "10-24h", "24-48h", ">48h"))) %>%
  group_by(Weekday, Time.Bins) %>%
  summarise(Frequency = n()) %>%
  ungroup()

ggplot(stacked_data, aes(x = Weekday, y = Frequency, fill = Time.Bins)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribuição de Tempos de Resolução por Dia da Semana",
       x = "Dia da Semana", y = "Frequência", fill = "Tempo de Resolução") +
  scale_y_continuous(expand = c(0, 0)) + # Garante que o eixo Y começa em 0
  theme_minimal(base_size = 14)



# Calcular média do tempo de resolução por dia da semana
line_data <- data %>%
  group_by(Weekday) %>%
  summarise(Mean.Resolution.Time = mean(Resolution.Time, na.rm = TRUE)) %>%
  arrange(match(Weekday, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# Plotar gráfico de linhas
ggplot(line_data, aes(x = Weekday, y = Mean.Resolution.Time, group = 1)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 3) +
  labs(title = "Tempo Médio de Resolução por Dia da Semana",
       x = "Dia da Semana", y = "Tempo Médio de Resolução (horas)") +
  theme_minimal(base_size = 14)


######

# Criar dados para gráfico polar
polar_data <- data %>%
  mutate(Hour = hour(Created.Date)) %>%
  group_by(Hour) %>%
  summarise(Count = n())

# Plotar gráfico polar
ggplot(polar_data, aes(x = factor(Hour), y = Count, fill = Count)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar(start = 0) +
  scale_fill_viridis_c() +
  labs(
    title = "Frequência de Reclamações por Hora",
    x = "Hora do Dia", y = "Frequência"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.title.y = element_blank())

#com interatividade. --- USAR ESTEEEEEEE!!!
# Frequência de reclamações por hora 
polar_data <- data %>%
  mutate(Hour = hour(Created.Date)) %>%
  group_by(Hour) %>%
  summarise(Count = n()) %>%
  arrange(Hour)

interactive_polar <- polar_data %>%
  plot_ly(
    type = 'barpolar',
    r = ~Count,       
    theta = ~Hour * 15,
    text = ~paste("Hora:", Hour, "<br>Frequência:", Count), 
    hoverinfo = "text", 
    marker = list(
      color = ~Count,   
      colorscale = "Viridis" 
    )
  ) %>%
  layout(
    polar = list(
      angularaxis = list(
        title = "",         
        tickmode = "array", 
        tickvals = seq(0, 360, 15), 
        ticktext = as.character(seq(0, 23)), 
        direction = "clockwise", 
        rotation = 90           
      ),
      radialaxis = list(
        title = "Frequência",
        tickangle = 0
      )
    )
  )

interactive_polar


################################### interatividade ########################################


library(plotly)

# Gráfico interativo 
interactive_line <- time_series_data %>%
  plot_ly(x = ~Date, y = ~Average.Resolution.Time, 
          type = 'scatter', mode = 'lines+markers', 
          line = list(color = 'purple', width = 3),  # Linhas mais grossas
          marker = list(size = 8, color = 'blue', symbol = 'circle')) %>%  # Bolinhas maiores
  layout(title = "Tempo Médio de Resolução ao Longo do Tempo",
         xaxis = list(
           title = "Data",
           tickformat = "%d-%m",  # Formato de data (DD-MM)
           tickangle = 45,
           tickmode = 'array',  # Mostrar todos os dias
           tickvals = time_series_data$Date  # Usar todas as datas
         ),
         yaxis = list(title = "Tempo Médio (horas)"))  # Título do eixo Y

interactive_line

#-----------------------ESTE!!!!!!!!!
#hora do dia por dia da semana
interactive_heatmap <- plot_ly(heatmap_data, x = ~Hour, y = ~Weekday, z = ~Average.Resolution.Time,
                               type = "heatmap", colors = c("lightblue", "blue")) %>%
  layout(title = "Heatmap Interativo: Tempo Médio de Resolução",
         xaxis = list(title = "Hora do Dia"),
         yaxis = list(title = "Dia da Semana"))
interactive_heatmap

'''
as áreas destacadas a azul escuro indicam horários com maior tempo médio de resolução
há tempos mais longos nas quintas feiras durante a manhã 
para tentar perceber o porquê --- ver o numero de queixas recebidas nesse horario;
ver a natureza das queixas, se são mais complexas de se resolver.
'''
#---------------------------------

#tempos de resolução por dia da semana
interactive_boxplot <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  plot_ly(x = ~Weekday, y = ~Resolution.Time, type = "box", color = ~Weekday) %>%
  layout(title = "Dispersão dos Tempos de Resolução por Dia da Semana",
         xaxis = list(title = "Dia da Semana"),
         yaxis = list(title = "Tempo de Resolução (horas)"))
interactive_boxplot

#-----------------------ESTE!!!!!!!!!
# Gráfico de violino interativo com plotly
interactive_violin <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  plot_ly(
    x = ~Weekday,
    y = ~Resolution.Time,
    split = ~Weekday, # Diferenciar por dia da semana
    type = 'violin',
    box = list(visible = TRUE),  # Exibe boxplot dentro do violino
    meanline = list(visible = TRUE),  # Adiciona uma linha para a média
    points = "all",  # Mostra os pontos de dados
    jitter = 0.3,  
    scalemode = "width"  
  ) %>%
  layout(
    title = "Distribuição dos Tempos de Resolução por Dia da Semana",
    xaxis = list(title = "Dia da Semana", categoryorder = "array",
                 categoryarray = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
    yaxis = list(title = "Tempo de Resolução (horas)")
  )

# Exibir o gráfico
interactive_violin

#-----------------------
  
# por período do dia
interactive_bar <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  group_by(Period.Of.Day) %>%
  summarise(Frequency = n()) %>%
  plot_ly(x = ~Period.Of.Day, y = ~Frequency, type = "bar", color = ~Period.Of.Day) %>%
  layout(title = "Frequência de Resoluções por Período do Dia",
         xaxis = list(title = "Período do Dia"),
         yaxis = list(title = "Frequência"))
interactive_bar


#dispersão 
interactive_scatter <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  plot_ly(x = ~Created.Date, y = ~Resolution.Time, type = "scatter", mode = "markers",
          color = ~Period.Of.Day) %>%
  layout(title = "Dispersão: Tempo de Resolução por Horário de Criação",
         xaxis = list(title = "Horário de Criação"),
         yaxis = list(title = "Tempo de Resolução (horas)"))
interactive_scatter



######################## análise mais detalhada ################################
# em relação ao heatmap 

# Heatmap do volume de reclamações por dia da semana e hora
volume_data <- data %>%
  mutate(Hour = hour(Created.Date), Weekday = weekdays(Created.Date)) %>%
  group_by(Weekday, Hour) %>%
  summarise(Volume = n()) %>%
  ungroup() %>%
  mutate(Weekday = factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# Criar o heatmap de volume
ggplot(volume_data, aes(x = Hour, y = Weekday, fill = Volume)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c() +
  labs(
    title = "Volume de Reclamações por Dia e Hora",
    x = "Hora do Dia",
    y = "Dia da Semana"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.title.y = element_blank())


# Selecionar os 10 tipos de reclamação mais frequentes
top_complaints <- data %>%
  count(Complaint.Type.Clean, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Complaint.Type.Clean)

# Filtrar apenas os tipos no Top 10
filtered_data <- data %>%
  filter(Complaint.Type.Clean %in% top_complaints)

# Agrupar por tipo de reclamação e dia da semana
complaint_data <- filtered_data %>%
  group_by(Weekday, Complaint.Type.Clean) %>%
  summarise(Volume = n()) %>%
  ungroup() %>%
  mutate(Weekday = factor(Weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

# Criar gráfico de barras empilhadas
ggplot(complaint_data, aes(x = Weekday, y = Volume, fill = Complaint.Type.Clean)) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(name = "Tipo de Reclamação") +
  labs(
    title = "Tipos de Reclamações por Dia da Semana",
    x = "Dia da Semana",
    y = "Volume"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Frequência de reclamações por hora 
polar_data <- data %>%
  mutate(Hour = hour(Created.Date)) %>%
  group_by(Hour) %>%
  summarise(Count = n()) %>%
  arrange(Hour)

interactive_polar <- polar_data %>%
  plot_ly(
    type = 'barpolar',
    r = ~Count,       
    theta = ~Hour * 15,
    text = ~paste("Hora:", Hour, "<br>Frequência:", Count), 
    hoverinfo = "text", 
    marker = list(
      color = ~Count,   
      colorscale = "Viridis" 
    )
  ) %>%
  layout(
    polar = list(
      angularaxis = list(
        title = "",         
        tickmode = "array", 
        tickvals = seq(0, 360, 15), 
        ticktext = as.character(seq(0, 23)), 
        direction = "clockwise", 
        rotation = 90           
      ),
      radialaxis = list(
        title = "Frequência",
        tickangle = 0
      )
    )
  )

interactive_polar



######################## outro gráfico ################################

data$Hour <- format(data$Created.Date, "%H")

# Gráfico de dispersão com cor e tamanho
ggplot(data, aes(x = Hour, y = Resolution.Time, color = Weekday, size = Resolution.Time)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_d() +
  labs(
    title = "Relação entre Tempo de Resolução e Hora do Dia",
    x = "Hora do Dia",
    y = "Tempo de Resolução (Horas)"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())

