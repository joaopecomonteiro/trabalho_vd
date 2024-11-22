
#########################################################

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

data = data[, .(Created.Date, Closed.Date, Agency, Agency.Name, Complaint.Type, Descriptor, City, Borough, Longitude, Latitude, Status, Street.Name)]

data = data %>%
  mutate(across(where(is.character), tolower))

data[, Created.Date := parse_date_time(`Created.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]
data[, Closed.Date := parse_date_time(`Closed.Date`, orders = c("m/d/Y I:M:S p", "m/d/Y H:M:S"), tz = "UTC")]

data[, Time.Open := as.period(difftime(Closed.Date, Created.Date), units = "hours")]


set(data, j = "Resolution.Time", value = round(as.numeric(difftime(data$Closed.Date, data$Created.Date), units = "hours"), 2))


data <- data %>%
  filter(Created.Date < '2016-09-30')

# Calculate the lower and upper bounds for the outlier detection
lower_bound <- 0
upper_bound <- quantile(data$Resolution.Time, 0.75, na.rm = TRUE) + 1.5 * IQR(data$Resolution.Time, na.rm = TRUE)

# Filter out the outliers
data <- data %>%
  filter( Resolution.Time >= 0 &
            Resolution.Time >= lower_bound & Resolution.Time <= upper_bound
  )

# Plot histogram of the filtered data
ggplot(data = data, aes(x = Resolution.Time)) + 
  geom_histogram(bins = 100) + 
  labs(title = "Histogram of Resolution Time", x = "Resolution Time", y = "Frequency")


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




dem_borough <- data.frame(
  Borough = c('bronx', 'brooklyn', 'manhattan', 'queens', 'staten island'),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747),
  People.Sqr.Mile = c(34920, 39438,	74781, 22125, 8618)
)

data_no <- data

average_resolution <- data_no %>%
  group_by(Borough)

average_resolution <- average_resolution %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

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



# Create the bar plot for average resolution time
ggplot(average_resolution, aes(x = reorder(Borough, Average.Time), y = Average.Time)) +
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


#############################################



# Load necessary libraries
library(ggplot2)
library(sf)
library(tigris) # for shapefiles of counties
library(viridis) # for color scale

# Ensure sf uses the EPSG 4326 (WGS 84) coordinate system for compatibility with longitude/latitude
bronx_shape <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME == "Bronx")

# Filter for Bronx data and remove NA resolution times
bronx_data <- data %>%
  filter(Borough == "bronx", !is.na(Resolution.Time)) %>%
  mutate(Resolution.Time = log1p(Resolution.Time))



# Plot the map
ggplot() +
  # Add the Bronx shapefile
  geom_sf(data = bronx_shape, fill = NA, color = "black") +
  # Add complaint points colored by resolution time
  geom_point(data = bronx_data, aes(x = Longitude, y = Latitude, color = Resolution.Time), size = 0.9, alpha = 0.5) +
  # Set the color scale for resolution time
  scale_color_viridis(option = "plasma", name = "log(Resolution Time) (hours)", na.value = "grey50") +
  labs(
    title = "Bronx Complaints by Resolution Time",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )


####################333

# Load necessary libraries
library(ggplot2)
library(sf)
library(tigris)
library(viridis)
library(lubridate)
library(dplyr)

# Load Bronx shapefile
bronx_shape <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME == "Bronx")

# Prepare data
bronx_data <- data %>%
  filter(Borough == "bronx", !is.na(Resolution.Time)) %>%
  mutate(
    Resolution.Time = log1p(Resolution.Time),    # Apply log transformation to Resolution Time
    Week = floor_date(as.Date(Created.Date), "week")  # Extract the week from Complaint.Date
  )

# Plot
ggplot() +
  geom_sf(data = bronx_shape, fill = NA, color = "black") +
  geom_point(data = bronx_data, aes(x = Longitude, y = Latitude, color = Resolution.Time), size = 0.9, alpha = 0.5) +
  scale_color_viridis(option = "plasma", name = "log(Resolution Time) (hours)", na.value = "grey50") +
  labs(
    title = "Bronx Complaints by Resolution Time (Weekly)",
    x = "Longitude",
    y = "Latitude"
  ) +
  facet_wrap(~ Week) +  # Separate plots by week
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )





##################################





# Find top 1% longest resolution times in the Bronx
high_res_time_cases <- data %>%
  filter(Borough == "bronx") %>%
  filter(Resolution.Time > quantile(Resolution.Time, 0.99, na.rm = TRUE))

# Check distribution by complaint type, agency, or time
high_res_time_cases %>%
  group_by(Complaint.Type.Clean, Agency) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), 
            Case.Count = n(), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time))


# Compare average resolution times for each complaint type across boroughs
complaint_type_comparison <- data %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  filter(Borough == "bronx") %>%
  arrange(desc(Average.Resolution.Time))

# Check if any complaint types have much higher averages in the Bronx than other boroughs
complaint_type_comparison


# Group data by day and borough to calculate average resolution time
ok <- data %>%
  mutate(Day = as.numeric(format(as.Date(Created.Date), "%d"))) %>%  # Convert Day to numeric
  group_by(Day, Borough) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Borough), Borough != 'unspecified')


# Plotting with updated grid and x-axis
ggplot(data = ok, aes(x = as.numeric(Day), y = Average.Resolution.Time, color = Borough, group = Borough)) +  # Group by Borough for separate lines
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(min(as.numeric(ok$Day)), max(as.numeric(ok$Day)), by = 1)) +  # Show every day
  labs(
    title = "Average Resolution Time by Day for Each Borough",
    x = "Day",
    y = "Average Resolution Time (Hours)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),  # Add grid lines for every day
    panel.grid.minor.x = element_blank(),                 # Remove minor x-axis grid lines for clarity
    axis.text.x = element_text(angle = 45,  hjust = 1)     # Rotate day labels for readability
  )




############################################################


# Calculate average resolution time by complaint type and borough
top_complaints_bronx <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time)) %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Average.Resolution.Time)) %>%
  head(10)


# Group data by day and borough to calculate average resolution time
ok <- data %>%
  mutate(Day = as.numeric(format(as.Date(Created.Date), "%d"))) %>%  # Convert Day to numeric
  group_by(Day, Borough, Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  filter(!is.na(Borough), Borough == 'bronx', Complaint.Type.Clean %in% top_complaints_bronx$Complaint.Type.Clean)


# Plotting with updated grid and x-axis
ggplot(data = ok, aes(x = as.numeric(Day), y = Average.Resolution.Time, color = Complaint.Type.Clean, Complaint.Type.Clean)) +  # Group by Borough for separate lines
  geom_line(size = 1) +
  scale_x_continuous(breaks = seq(min(as.numeric(ok$Day)), max(as.numeric(ok$Day)), by = 1)) +  # Show every day
  labs(
    title = "Average Resolution Time by Day for Each Borough",
    x = "Day",
    y = "Average Resolution Time (Hours)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_line(color = "grey80"),  # Add grid lines for every day
    panel.grid.minor.x = element_blank(),                 # Remove minor x-axis grid lines for clarity
    axis.text.x = element_text( hjust = 1)     # Rotate day labels for readability
  )





# Average resolution time by agency for Bronx complaints
agency_resolution_times <- data %>%
  filter(Borough == "bronx") %>%
  group_by(Agency) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time))

agency_resolution_times


# Find streets in the Bronx with the highest average resolution times
street_resolution_times <- data %>%
  filter(Borough == "bronx", !is.na(Street.Name)) %>%
  group_by(Street.Name) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time)) %>%
  slice_head(n = 10) # Top 10 streets

street_resolution_times


###########################################3


# Prepare data

week_data <- data %>%
  filter(Borough != "unspecified", !is.na(Resolution.Time), !is.na(Borough)) %>%
  mutate(Week = floor_date(as.Date(Created.Date), "week"))

dem_borough <- data.frame(
  Borough = c('bronx', 'brooklyn', 'manhattan', 'queens', 'staten island'),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747),
  People.Sqr.Mile = c(34920, 39438,	74781, 22125, 8618)
)

data_no <- week_data

average_resolution <- data_no %>%
  group_by(Borough)

average_resolution <- average_resolution %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

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










data_boroug <- data %>%
  filter(!is.na(Borough) & !is.na(Complaint.Type.Clean) & Borough != 'unspecified')

top_complaints <- data_boroug %>%
  count(Complaint.Type.Clean, sort = TRUE) %>%
  top_n(10, n) %>%
  pull(Complaint.Type.Clean)

library(RColorBrewer)

# Create a color palette for the top 10 complaint types
colors <- brewer.pal(n = 10, name = "Set3")  # You can choose any color palette
names(colors) <- top_complaints  # Name the colors according to complaint types
# Filter the data to include only the top complaint types for coloring

data_boroug <- data_boroug %>%
  filter(Complaint.Type.Clean %in% top_complaints)

data_clean <- data %>%
  filter(Borough != 'unspecified', !is.na(Borough))

# Create the plot
ggplot(data_clean, aes(x = Borough, y = Resolution.Time)) +
  geom_jitter(size = 0.7, alpha = 0.3, color = 'blue') + # Jitter for individual points
  # scale_color_manual(values = colors) +  # Apply the color palette
  labs(title = "Resolution Time by Borough",
       x = "Borough",
       y = "Resolution Time (hours)") +
  theme_minimal() +
  theme(legend.title = element_blank())  # Optional: Remove legend title
