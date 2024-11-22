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


data = read.csv("C:/Users/castr/Desktop/Uni/VD/PROJ1/NYC_311_Data_20241009.csv", header=TRUE, sep=";", na.strings=c("", " ", "N/A"))

setDT(data)

data = data[, .(Created.Date, Closed.Date, Agency, Agency.Name, Complaint.Type, Descriptor, City, Borough, Longitude, Latitude, Status)]

data = data %>%
  mutate(across(where(is.character), tolower))

data[, Created.Date := parse_date_time(`Created.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]
data[, Closed.Date := parse_date_time(`Closed.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]

data[, Time.Open := as.period(difftime(Closed.Date, Created.Date), units = "hours")]


set(data, j = "Resolution.Time", value = round(as.numeric(difftime(data$Closed.Date, data$Created.Date), units = "hours"), 2))


ggplot(data = data, aes(x = Created.Date)) + 
  geom_bar(stat = 'count')


print((sort(unique(data$Complaint.Type))))


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


# Map representation of the complaints
data_map <- data %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%
  filter(Borough != 'unspecified')

borough_palette <- colorFactor(
  palette = c("blue", "green", "red", "purple", "orange"), # Escolha as cores para cada borough
  domain = data_map$Borough
)

leaflet(data = data_map) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    radius = 0.5,
    color = ~borough_palette(Borough),  # Aplicar a paleta de cores
    opacity = 0.0001
  ) %>%
  addLegend(
    position = "bottomright",            # Posição da legenda no mapa
    pal = borough_palette,                # Paleta de cores definida
    values = data_map$Borough,           # Valores que a legenda deve representar
    title = "Boroughs",                  # Título da legenda
    opacity = 1                          # Opacidade da legenda
  ) %>%
  setView(lng = -74.006, lat = 40.7128, zoom = 10)


ggplot(data = data, aes(x = fct_infreq(Agency))) + 
  geom_bar(fill = 'skyblue') + 
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Barplot of Agencies", x = "Agency", y = "Counts")



# ANOVA to check if Resolution Time varies by Borough
anova_borough <- aov(Resolution.Time ~ Borough, data = data)
summary(anova_borough)

# If you have other categorical variables, repeat the above:
anova_agency <- aov(Resolution.Time ~ Agency, data = data)
summary(anova_agency)

data_boroug <- data %>%
  filter(!is.na(Borough) & !is.na(Complaint.Type.Clean) & Borough != 'unspecified')

top_complaints <- data_boroug %>%
  count(Complaint.Type.Clean, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Complaint.Type.Clean)

library(RColorBrewer)

# Create a color palette for the top 10 complaint types
colors <- brewer.pal(n = 10, name = "Set1")  # You can choose any color palette
names(colors) <- top_complaints  # Name the colors according to complaint types
# Filter the data to include only the top complaint types for coloring

data_boroug <- data_boroug %>%
  filter(Complaint.Type.Clean %in% top_complaints)

# Create the plot
ggplot(data_boroug, aes(x = Borough, y = Resolution.Time, color = Complaint.Type.Clean)) +
  geom_jitter(size = 1.7, alpha = 0.7) + # Jitter for individual points
  # scale_color_manual(values = colors) +  # Apply the color palette
  labs(title = "Resolution Time by Borough and Complaint Type",
       x = "Borough",
       y = "Resolution Time (hours)") +
  theme_minimal() +
  theme(legend.title = element_blank())  # Optional: Remove legend title



average_resolution <- data_boroug %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  filter(
    Resolution.Time > quantile(Resolution.Time, 0.25, na.rm = TRUE) - 1.5 * IQR(Resolution.Time, na.rm = TRUE),
    Resolution.Time < quantile(Resolution.Time, 0.75, na.rm = TRUE) + 1.5 * IQR(Resolution.Time, na.rm = TRUE))
    
    
average_resolution <- average_resolution %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')


library(dplyr)

# Calculate the difference between the fastest and slowest borough for each complaint type
# largest_diffs <- average_resolution %>%
#   group_by(Complaint.Type.Clean) %>%
#   summarise(
#     Max.Time = max(Average.Time, na.rm = TRUE),
#     Min.Time = min(Average.Time, na.rm = TRUE),
#     Difference = Max.Time - Min.Time
#   ) %>%
#   arrange(desc(Difference)) %>%
#   head(n = 5)


# View the complaint types with the largest differences
# 
# average_resolution <- average_resolution %>%
#   filter(Complaint.Type.Clean %in% largest_diffs$Complaint.Type.Clean)



library(ggplot2)

# Create the bar plot for average resolution time
ggplot(average_resolution, aes(x = Complaint.Type.Clean, y = Average.Time, fill = Borough)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Average Resolution Time by Complaint Type and Borough",
       x = "Complaint Type",
       y = "Average Resolution Time (hours)") +
  scale_fill_viridis_d(option = "D", n = 5) +  # Use a discrete viridis color scale with 10 colors
  theme_minimal() +
  theme(legend.title = element_blank()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




ggplot(data_boroug, aes(x = Borough, y = Resolution.Time)) +
  geom_boxplot(fill = 'skyblue') +
  labs(title = "Boxplot of Resolution Time by Borough", x = "Borough", y = "Resolution Time") +
  theme_minimal()

# Violin plot for Agency
ggplot(data, aes(x = Agency, y = Resolution.Time)) +
  geom_violin(fill = 'lightgreen') +
  labs(title = "Violin Plot of Resolution Time by Agency", x = "Agency", y = "Resolution Time") +
  theme_minimal()



library(ggplot2)
library(sf)
library(dplyr)
library(tigris)

complaints_by_borough <- data %>%
  group_by(Borough) %>%
  summarise(Complaints = n())

# de acordo com os sensos de 2020
dem_borough <- data.frame(
  Borough = c('bronx', 'brooklyn', 'manhattan', 'queens', 'staten island'),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747)
)

combined_data <- merge(dem_borough, complaints_by_borough, by = "Borough")

# Calcule a proporção de reclamações por pessoa
combined_data <- combined_data %>%
  mutate(Complaints.Per.Capita = (Complaints / Population))

print(combined_data)


# Carregar o shapefile dos bairros de Nova Iorque
nyc_boroughs <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME %in% c("Bronx", "Kings", "New York", "Queens", "Richmond"))


# Ajuste o nome dos bairros para coincidirem com os do shapefile
combined_data$Borough <- recode(combined_data$Borough,
                                "bronx" = "Bronx",
                                "brooklyn" = "Kings",
                                "manhattan" = "New York",
                                "queens" = "Queens",
                                "staten island" = "Richmond")

library(shadowtext)


# Juntar o shapefile com os dados de reclamações per capita
nyc_boroughs_data <- nyc_boroughs %>%
  left_join(combined_data, by = c("NAME" = "Borough"))


# Calculate centroids for labeling
nyc_boroughs_data <- nyc_boroughs_data %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2])

# Create a new column with desired names for labeling
nyc_boroughs_data <- nyc_boroughs_data %>%
  mutate(
    custom_name = case_when(
      NAME == "Bronx" ~ "Bronx",
      NAME == "Kings" ~ "Brooklyn",
      NAME == "New York" ~ "Manhattan",
      NAME == "Queens" ~ "Queens",
      NAME == "Richmond" ~ "Staten Island"
    )
  )

# Plot with custom borough names (black text with white outline)
ggplot(nyc_boroughs_data) +
  geom_sf(aes(fill = Complaints.Per.Capita)) +
  scale_fill_viridis_c(option = 'D') +
  labs(
    title = "Proportion of Complaints per Capita for each Borough",
    fill = "Complaints per capita"
  ) +
  # Use custom_name for labeling
  geom_shadowtext(aes(x = lon, y = lat, label = custom_name), 
                  color = "black", bg.color = "white", size = 4, fontface = "bold", bg.r = 0.07) +
  theme_minimal() +
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())


data_clean <- data %>%
  filter(!is.na(Longitude) & 
         !is.na(Latitude) &
          Borough != 'unspecified')


# First, make sure that `nyc_boroughs` is already loaded as an sf object
nyc_boroughs <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME %in% c("Bronx", "Kings", "New York", "Queens", "Richmond"))

# Plot the map with points
ggplot() +
  # Map of NYC boroughs
  geom_sf(data = nyc_boroughs, fill = NA, color = "black") +
  # Points for complaints
  geom_point(data = data_clean, aes(x = Longitude, y = Latitude, color = Borough), alpha = 0.4, size = 0.5) +
  scale_fill_viridis_d(option = 'D') +
  labs(
    title = "Complaints Distribution by Borough",
    color = "Borough"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  theme_minimal() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())

library(tidyr)

daily_complaints <- data %>%
  filter(Borough != 'unspecified', !is.na(Borough)) %>%
  count(Borough) %>%
  group_by(Created.Date)


daily_complaints <- data %>%
  filter(Borough != 'unspecified', !is.na(Borough)) %>%
  group_by(Created.Date, Borough) %>%
  summarise(Daily.Count = n(), .groups = 'drop')

# Reshape data to make each borough a separate column
daily_complaints_wide <- daily_complaints %>%
  pivot_wider(names_from = Borough, values_from = Daily.Count, values_fill = 0)

library(ggplot2)

# Reshape data to long format for easy plotting
daily_complaints_long <- daily_complaints_wide %>%
  pivot_longer(cols = -Created.Date, names_to = "Borough", values_to = "Daily.Count")

# Plot each borough's complaints over time
ggplot(daily_complaints_long, aes(x = Created.Date, y = Daily.Count, color = Borough)) +
  geom_line() +
  labs(
    title = "Daily Complaint Counts by Borough",
    x = "Date",
    y = "Daily Complaint Count",
    color = "Borough"
  ) +
  theme_minimal()


# View the reshaped data
# head(daily_complaints_wide)

ggplot(daily_complaints, aes(x = Created.Date)) +
  geom_area(alpha = 0.8)


data$Date <- as.Date(data$Created.Date)

data_area <- data %>%
  group_by(Date, Borough) %>%
  summarise(Complaint_Count = n(), .groups = 'drop') %>%
  filter(Borough != 'unspecified', !is.na(Borough), Date != '2016-09-30' )


ggplot(data_area, aes(x = Date, y = Complaint_Count, fill = Borough)) +
  geom_area(alpha = 0.6) +
  labs(title = "Complaints Over Time by Borough",
       x = "Date",
       y = "Number of Complaints") +
  theme_minimal() +
  scale_fill_viridis_d(option = 'D') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 



proportions <- data %>%
  count(Status) %>%
  mutate(Proportion = round(n / sum(n), 5) * 100)



top_complaints <- data %>%
  group_by(Complaint.Type.Clean) %>%
  tally() %>%
  top_n(5, n) %>%
  pull(Complaint.Type.Clean)

data_map <- data %>%
  filter(!is.na(Latitude), !is.na(Longitude), Complaint.Type.Clean %in% top_complaints)

# First, make sure that `nyc_boroughs` is already loaded as an sf object
nyc_boroughs <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME %in% c("Bronx", "Kings", "New York", "Queens", "Richmond"))

# Plot the map with points
ggplot() +
  # Map of NYC boroughs
  geom_sf(data = nyc_boroughs, fill = NA, color = "black") +
  # Points for complaints
  geom_point(data = data_map, aes(x = Longitude, y = Latitude, color = Complaint.Type.Clean), alpha = 0.3, size = 0.6) +
  scale_fill_viridis_d() +
  labs(
    title = "Complaints Distribution by Borough",
    color = "Borough"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) +
  theme_minimal() + 
  theme(axis.title = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())



no_loc <- data %>%
  filter(is.na(Latitude), is.na(Longitude), !is.na(Complaint.Type.Clean), Borough != 'unspecified', !is.na(Borough))


top_complaints <- no_loc %>%
  group_by(Complaint.Type.Clean) %>%
  tally() %>%
  top_n(10, n) %>%
  pull(Complaint.Type.Clean)

no_loc <- no_loc %>%
  filter(Complaint.Type.Clean %in% top_complaints)

ggplot(no_loc, aes(x = Borough, y = Resolution.Time, color = Complaint.Type.Clean)) +
  geom_jitter(size = 1.7, alpha = 0.7) + # Jitter for individual points
  # scale_color_manual(values = colors) +  # Apply the color palette
  labs(title = "Resolution Time by Borough and Complaint Type",
       x = "Borough",
       y = "Resolution Time (hours)") +
  theme_minimal() +
  scale_fill_viridis_c() +
  theme(legend.title = element_blank())  # Optional: Remove legend title




# de acordo com os sensos de 2020
dem_borough <- data.frame(
  Borough = c('bronx', 'brooklyn', 'manhattan', 'queens', 'staten island'),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747)
)

combined_data <- merge(dem_borough, complaints_by_borough, by = "Borough")


problematic_types <- data %>%
  filter(Complaint.Type.Clean %in% c('paint/plaster', 'street light condition', 'unsanitary condition'))

problematic_types_si <- problematic_types %>%
  filter(Borough == 'staten island')


ggplot(data = problematic_types_si, aes(x = Complaint.Type.Clean, y = Resolution.Time)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.color = "red", outlier.shape = 1) +
  labs(
    title = "Resolution Time by Complaint Type",
    x = "Complaint Type",
    y = "Resolution Time"
  ) +
  facet_wrap(~ Complaint.Type.Clean, scales = "free_y") +  # Separate y-axis scales for each complaint type
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())  # Hide x-axis text for individual facets

  

# Calcule a proporção de reclamações por pessoa
combined_data <- combined_data %>%
  mutate(Complaints.Per.Capita = (Complaints / Population))



average_resolution <- data %>%
  group_by(Borough) %>%
  filter(
    Resolution.Time > quantile(Resolution.Time, 0.25, na.rm = TRUE) - 1.5 * IQR(Resolution.Time, na.rm = TRUE),
    Resolution.Time < quantile(Resolution.Time, 0.75, na.rm = TRUE) + 1.5 * IQR(Resolution.Time, na.rm = TRUE))


average_resolution <- average_resolution %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  filter(Borough != 'unspecified')


dem_borough <- data.frame(
  Borough = c('bronx', 'brooklyn', 'manhattan', 'queens', 'staten island'),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747),
  People.Sqr.Mile = c(34920, 39438,	74781, 22125, 8618)
)


# Create the bar plot for average resolution time
ggplot(average_resolution, aes(x = Borough, y = Average.Time)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = 'skyblue') +
  labs(title = "Average Resolution Time by Borough",
       x = "Borough",
       y = "Average Resolution Time (hours)") +
  #scale_fill_viridis_d() +  # Use a discrete viridis color scale with 10 colors
  theme_minimal() + 
  geom_text(
    aes(label = round(Average.Time, 2)),  # Label with one decimal place
    position = position_dodge(width = 0.9),  # Adjust for dodged bars
    vjust = -0.5,  # Position text above bars
    size = 3       # Adjust text size
  )



# Combine average_resolution and dem_borough datasets by Borough
combined_data <- average_resolution %>%
  inner_join(dem_borough, by = "Borough")

# Select only the relevant columns for the correlation
correlation_data <- combined_data %>%
  select(Average.Time, Population, People.Sqr.Mile)

# Calculate the correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")


library(patchwork)  # Optional: for arranging plots side-by-side

library(ggplot2)
library(patchwork)

# Scatter plot of Average.Time vs Population with borough names
plot_population <- ggplot(combined_data, aes(x = Population, y = Average.Time)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", color = "darkblue", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +  # Adding borough names
  labs(title = "Average Resolution Time vs Population",
       x = "Population",
       y = "Average Resolution Time (hours)") +
  theme_minimal()

# Scatter plot of Average.Time vs People per Sqr Mile with borough names
ggplot(combined_data, aes(x = People.Sqr.Mile, y = Average.Time)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +  # Adding borough names
  labs(title = "Average Resolution Time vs People per Square Mile",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")  # Prevent clipping of labels

# Combine plots side-by-side (optional)
plot_population + plot_density


# Combine plots side-by-side (optional)
plot_population + plot_density



bronx <- data %>%
  filter(Borough == 'bronx')

top_10_bronx_complaints <- bronx %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Average.Resolution.Time)) %>%
  slice_head(n = 5)

# View the result
print(top_10_bronx_complaints)


# Calculate average resolution time by Borough and Complaint Type
average_resolution_boroughs <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Filter the Bronx and other boroughs separately
bronx_complaints <- average_resolution_boroughs %>%
  filter(Borough == 'bronx') %>%
  arrange(desc(Average.Resolution.Time)) %>%
  slice_head(n = 5)

other_boroughs_complaints <- average_resolution_boroughs %>%
  filter(Borough != 'bronx') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Average.Resolution.Time, na.rm = TRUE), .groups = 'drop')

other_boroughs_complaints <- other_boroughs_complaints %>%
  filter(Complaint.Type.Clean %in% top_10_bronx_complaints$Complaint.Type.Clean)

# Combine the Bronx and other boroughs' data
combined_complaints <- bind_rows(
  bronx_complaints %>%
    mutate(Borough = 'Bronx'),
  other_boroughs_complaints %>%
    mutate(Borough = 'Other Boroughs')
) 



# Plot comparison
ggplot(combined_complaints, aes(x = Complaint.Type.Clean, y = Average.Resolution.Time, fill = Borough)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Comparison of Average Resolution Time by Complaint Type",
    x = "Complaint Type",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

data_no <- data %>%
  filter(Complaint.Type.Clean != 'adopt-a-basket')

average_resolution <- data_no %>%
  group_by(Borough) %>%
  filter(
    Resolution.Time > quantile(Resolution.Time, 0.25, na.rm = TRUE) - 1.5 * IQR(Resolution.Time, na.rm = TRUE),
    Resolution.Time < quantile(Resolution.Time, 0.75, na.rm = TRUE) + 1.5 * IQR(Resolution.Time, na.rm = TRUE))


average_resolution <- average_resolution %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  filter(Borough != 'unspecified')

ggplot(average_resolution, aes(x = Borough, y = Average.Time)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = 'skyblue') +
  labs(title = "Average Resolution Time by Borough",
       x = "Borough",
       y = "Average Resolution Time (hours)") +
  #scale_fill_viridis_d() +  # Use a discrete viridis color scale with 10 colors
  theme_minimal() + 
  geom_text(
    aes(label = round(Average.Time, 2)),  # Label with one decimal place
    position = position_dodge(width = 0.9),  # Adjust for dodged bars
    vjust = -0.5,  # Position text above bars
    size = 3       # Adjust text size
  )

combined_data <- average_resolution %>%
  inner_join(dem_borough, by = "Borough")



# Scatter plot of Average.Time vs People per Sqr Mile with borough names
ggplot(combined_data, aes(x = People.Sqr.Mile, y = Average.Time)) +
  geom_point(color = "darkgreen", size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +  # Adding borough names
  labs(title = "Average Resolution Time vs People per Square Mile",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")  # Prevent clipping of labels


# Step 1: Calculate average resolution time by Borough and Complaint Type
average_resolution_boroughs <- data %>%
  filter(!is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Filter the Bronx and other boroughs separately
bronx_complaints <- average_resolution_boroughs %>%
  filter(Borough == 'bronx') %>%
  select(Complaint.Type.Clean, Bronx.Avg.Resolution.Time = Average.Resolution.Time)

other_boroughs_complaints <- average_resolution_boroughs %>%
  filter(Borough != 'bronx') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Other.Boroughs.Avg.Resolution.Time = mean(Average.Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 3: Combine the two datasets
combined_complaints <- left_join(bronx_complaints, other_boroughs_complaints, by = "Complaint.Type.Clean")

# Step 4: Calculate the difference in average resolution time between Bronx and other boroughs
combined_complaints <- combined_complaints %>%
  mutate(Difference = abs(Bronx.Avg.Resolution.Time - Other.Boroughs.Avg.Resolution.Time))

# Step 5: Get the top 5 complaint types with the highest differences
top_5_complaints <- combined_complaints %>%
  arrange(desc(Difference)) %>%
  slice_head(n = 5)

# Step 6: Plot the results
ggplot(top_5_complaints, aes(x = reorder(Complaint.Type.Clean, -Difference), y = Difference, fill = Complaint.Type.Clean)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Top 5 Complaint Types with the Highest Difference in Resolution Time",
    x = "Complaint Type",
    y = "Difference in Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



################################################################

# Step 1: Calculate the overall average resolution time for the Bronx
bronx_overall_avg <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time)) %>%
  summarise(Overall.Avg = mean(Resolution.Time, na.rm = TRUE)) %>%
  pull(Overall.Avg)

print(bronx_overall_avg)

# Step 2: Calculate the mean resolution time for each complaint type in the Bronx
bronx_complaints_avg <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time)) %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 3: For each complaint type, calculate the mean excluding that complaint type
bronx_complaints_avg <- bronx_complaints_avg %>%
  mutate(
    Excluded.Avg = sapply(Complaint.Type.Clean, function(complaint) {
      data %>%
        filter(Borough == 'bronx', Complaint.Type.Clean != complaint, !is.na(Resolution.Time)) %>%
        summarise(Avg = mean(Resolution.Time, na.rm = TRUE)) %>%
        pull(Avg)
    }),
    Difference = abs(Excluded.Avg - bronx_overall_avg)  # Calculate the difference from the overall average
  )

# Step 4: Identify the complaint type that most affects the mean
most_influential_complaint <- bronx_complaints_avg %>%
  arrange(desc(Difference)) %>%
  slice_head(n = 3)

# Display the result
print(most_influential_complaint)





