library(data.table)

time_fread <- system.time({
  dataset <- fread(file.choose(), sep= ';', fill=TRUE)
})

head(dataset, 5)

library(dplyr)       
library(lubridate)  
library(ggplot2)     

summary(dataset)

#missing values
missing_values <- colSums(is.na(dataset))
missing_values[missing_values > 0]

missing_values <- rowSums(is.na(dataset))
head(missing_values)
linhas_com_ausentes <- dataset[missing_values > 0, ]
linhas_com_ausentes


#formatar datas
dataset[, Created_Date := parse_date_time(`Created Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]
dataset[, Closed_Date := parse_date_time(`Closed Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]

dataset[, duration := as.period(difftime(Closed_Date, Created_Date))]

dataset[, duration_formatted := paste(duration@year * 365 + duration@month * 30 + duration@day, 
                                      "dias,", 
                                      duration@hour, 
                                      "horas,", 
                                      duration@minute, 
                                      "minutos")]

head(dataset[, .(Created_Date, Closed_Date, duration_formatted)])

dataset[, `Created Date` := NULL]
dataset[, `Closed Date` := NULL]


#Visualizações

# Converter a duração para um formato que possa ser usado em histogramas
dataset[, duration_days := as.numeric(duration@year * 365 + duration@month * 30 + duration@day)]
dataset[, duration_hours := duration@hour]
dataset[, duration_minutes := duration@minute]

# Histograma do tempo de resolução
ggplot(dataset, aes(x = as.numeric(duration))) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  labs(title = "Distribuição do Tempo de Resolução", x = "Tempo de Resolução (dias)", y = "Frequência")

# tempo de resolução por tipo de reclamação
ggplot(dataset, aes(x = `Complaint Type`, y = as.numeric(duration))) +
  geom_boxplot() +
  labs(title = "Tempo de Resolução por Tipo de Reclamação", x = "Tipo de Reclamação", y = "Tempo de Resolução (dias)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# tempo médio de resolução por tipo de reclamação
mean_resolution_time <- dataset %>%
  group_by(`Complaint Type`) %>%
  summarize(mean_duration = mean(as.numeric(duration), na.rm = TRUE))

ggplot(mean_resolution_time, aes(x = reorder(`Complaint Type`, -mean_duration), y = mean_duration)) +
  geom_bar(stat = "identity", fill = "purple") +
  labs(title = "Tempo Médio de Resolução por Tipo de Reclamação", x = "Tipo de Reclamação", y = "Tempo Médio (dias)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# missing values - latitude, longitude
sum(is.na(dataset$Latitude))
sum(is.na(dataset$Longitude))

# eliminar linhas com missing values
dataset <- dataset[!is.na(Latitude) & !is.na(Longitude)]

# Mapa de calor usando geom_density_2d e geom_point para visualizar concentração de pontos
ggplot(dataset, aes(x = Longitude, y = Latitude)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", color = "white") +
  geom_point(alpha = 0.3, size = 0.5, color = "blue") +
  scale_fill_viridis_c() +
  labs(title = "Mapa de Calor das Reclamações", x = "Longitude", y = "Latitude") +
  theme_minimal()

library(leaflet)

city_location <- dataset %>%
  group_by(City) %>%
  summarize(count = n(), lat = mean(Latitude, na.rm = TRUE), lon = mean(Longitude, na.rm = TRUE)) %>%
  arrange(desc(count))

leaflet(city_location) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~lon, lat = ~lat,
    radius = ~sqrt(count) / 2, # Tamanho do círculo proporcional ao número de reclamações
    color = "red",
    label = ~paste(City, ": ", count, " reclamações"),
    popup = ~paste("<strong>", City, "</strong><br>", count, " reclamações")
  )


# 20 cidades com mais reclamações
top_cities <- dataset %>%
  group_by(City) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(20)

ggplot(top_cities, aes(x = reorder(City, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Número de Reclamações por Cidade", x = "Cidade", y = "Número de Reclamações") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#total de queixas por dia
daily_complaints <- dataset %>%
  group_by(date = as.Date(Created_Date)) %>%  
  summarize(count = n())

ggplot(daily_complaints, aes(x = date, y = count)) +
  geom_line(color = "blue") +
  labs(title = "Total de Queixas por Dia", x = "Data", y = "Número de Queixas") +
  theme_minimal()
