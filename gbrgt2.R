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


###########################################################

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




######################################################

# Create the bar plot for average resolution time
ggplot(average_resolution, aes(x = reorder(Borough, Average.Time), y = Average.Time)) +
  geom_bar(stat = "identity", position = position_dodge(), fill = 'skyblue') +
  labs(title = "Average Resolution Time by Borough",
       x = "Borough",
       y = "Average Resolution Time (hours)") +
  #scale_fill_viridis_d() +  # Use a discrete viridis color scale with 10 colors
  theme_minimal() + 
  coord_cartesian(ylim = c(40, 80)) +
  geom_text(
    aes(label = round(Average.Time, 2)),  # Label with one decimal place
    position = position_dodge(width = 0.9),  # Adjust for dodged bars
    vjust = -0.5,  # Position text above bars
    size = 3       # Adjust text size
  )


###############################################

queens <- data %>%
  filter(Borough == 'queens')


top_10_queens_complaints <- queens %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Average.Time)) %>%
  slice_head(n = 5)



# Create the bar plot for average resolution time
ggplot(top_10_queens_complaints, aes(x = Complaint.Type.Clean, y = Average.Time)) +
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
  
  
  
###########################################

data_no_out <- data

average_resolution <- data_no_out %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  filter(Borough != 'unspecified')


# Step 1: Calculate the overall average resolution time for the queens
queens_overall_avg <- data_no_out %>%
  filter(Borough == 'queens') %>%
  summarise(Overall.Avg = mean(Resolution.Time, na.rm = TRUE)) %>%
  pull(Overall.Avg)

print(queens_overall_avg)

# Step 2: Calculate the mean resolution time for each complaint type in the queens
queens_complaints_avg <- data_no_out %>%
  filter(Borough == 'queens', !is.na(Resolution.Time)) %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Avg.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 3: For each complaint type, calculate the mean excluding that complaint type
queens_complaints_avg <- queens_complaints_avg %>%
  mutate(
    Excluded.Avg = sapply(Complaint.Type.Clean, function(complaint) {
      data_no_out %>%
        filter(Borough == 'queens', Complaint.Type.Clean != complaint, !is.na(Resolution.Time)) %>%
        summarise(Avg = mean(Resolution.Time, na.rm = TRUE)) %>%
        pull(Avg)
    }),
    Difference = abs(Excluded.Avg - queens_overall_avg)  # Calculate the difference from the overall average
  )

# Step 4: Identify the complaint type that most affects the mean
most_influential_complaint <- queens_complaints_avg %>%
  arrange(desc(Difference)) %>%
  slice_head(n = 10)

# Display the result
print(most_influential_complaint)


############################################################3

data_no_out <- data

total_cases_queens <- data_no_out  %>%
  filter(Borough == 'queens') %>%
  nrow()

# Calculate the weight for each complaint type in the queens
queens_complaint_weights <- data_no_out  %>%
  filter(Borough == 'queens') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(
    Total.Resolution.Time = sum(Resolution.Time, na.rm = TRUE),
    Weight = Total.Resolution.Time / total_cases_queens,
    .groups = 'drop'
  ) %>%
  arrange(desc(Weight)) 

# Display the resulting table
print(queens_complaint_weights)
  
  
###################################################
library(dplyr)
library(ggplot2)



# Step 1: Get the top 5 weighted complaint types in the queens
top_5_complaints_queens <- queens_complaint_weights %>%
  top_n(5, Weight) %>%
  pull(Complaint.Type.Clean)

# Step 2: Calculate the average resolution time for the top 5 complaint types for each borough
top_5_averages <- data_no_out %>%
  filter(Complaint.Type.Clean %in% top_5_complaints_queens) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 3: Calculate the average resolution time for "Other Complaints" (outside top 5) for each borough
other_complaints_averages <- data_no_out %>%
  filter(!Complaint.Type.Clean %in% top_5_complaints_queens) %>%
  group_by(Borough) %>%
  summarise(
    Complaint.Type.Clean = "Other Complaints",
    Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
    .groups = 'drop'
  )

# Step 4: Combine the top 5 and "Other Complaints" data
combined_averages <- bind_rows(top_5_averages, other_complaints_averages)

# Step 5: Create the bar plot
ggplot(combined_averages, aes(x = Complaint.Type.Clean, y = Average.Resolution.Time, fill = Borough)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(
    title = "Comparison of Average Resolution Times by Borough, Including 'Other Complaints'",
    x = "Complaint Type",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "D") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

########################################33

library(dplyr)

# Step 1: Calculate the average resolution time for each complaint type in each borough
average_resolution <- data_no_out %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Identify the maximum average resolution time for each complaint type across all boroughs
max_resolution_per_complaint <- average_resolution %>%
  group_by(Complaint.Type.Clean) %>%
  filter(Average.Time == max(Average.Time)) %>%
  ungroup()

# Step 3: Filter to include only cases where the Borough is queens
queens_max_resolution <- max_resolution_per_complaint %>%
  filter(Borough == "queens")

# Display the result
print(queens_max_resolution)


###########################################################

`%notin%` <- Negate(`%in%`)

data_no <- data 
average_resolution <- data_no %>%
  group_by(Borough)

average_resolution <- average_resolution %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

combined_data <- average_resolution %>%
  inner_join(dem_borough, by = "Borough")

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


################################3
#################################
#####################################

# Calculate the total number of complaints and weighted average per complaint type
queens_contributions <- data_no_out %>%
  filter(Borough == "queens") %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(
    Total.Resolution.Time = sum(Resolution.Time, na.rm = TRUE),
    Count = n(),
    Average.Time = mean(Resolution.Time, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(Weighted.Contribution = (Total.Resolution.Time / sum(Total.Resolution.Time)) * Average.Time) %>%
  arrange(desc(Weighted.Contribution))

# Display top contributors
print(queens_contributions)


# Calculate average resolution time per complaint type across boroughs
comparison <- data_no_out %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  spread(key = Borough, value = Average.Time)

# Check if queens has higher times across multiple types
print(comparison)




library(ggplot2)

# Identify outliers using IQR
queens_outliers <- data_no_out %>%
  filter(Borough == "queens") %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(
    Q1 = quantile(Resolution.Time, 0.25, na.rm = TRUE),
    Q3 = quantile(Resolution.Time, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  right_join(data_no_out, by = c("Complaint.Type.Clean")) %>%
  filter(Borough == "queens", 
         Resolution.Time > (Q3 + 1.5 * IQR) | Resolution.Time < (Q1 - 1.5 * IQR))

# Display outliers
print(queens_outliers)


#######################################


top_100_cases <- data %>%
  arrange(desc(Resolution.Time)) %>%
  slice_head(n = 100)

top_100_cases <- data %>%
  filter(Borough == 'bronx') %>%
  arrange(desc(Resolution.Time)) %>%
  slice_head(n = 1000)

# Summarize by Borough to get the most common features per Borough
summary_borough <- top_100_cases %>%
  group_by(Borough) %>%
  summarise(
    most_common_day = names(sort(table(as.Date(Created.Date)), decreasing = TRUE))[1],
    most_common_complaint_type = names(sort(table(Complaint.Type.Clean), decreasing = TRUE))[1],
    most_common_agency = names(sort(table(Agency), decreasing = TRUE))[1],
    .groups = 'drop'
  )

print(summary_borough)


print(summary_top_100)



###########################################################

# Load necessary libraries
library(ggplot2)
library(sf)
library(tigris) # for shapefiles of counties
library(viridis) # for color scale

# Ensure sf uses the EPSG 4326 (WGS 84) coordinate system for compatibility with longitude/latitude
bronx_shape <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME == "Queens")

# Filter for Bronx data and remove NA resolution times
bronx_data <- data %>%
  filter(Borough == "queens", !is.na(Resolution.Time)) %>%
  mutate(Resolution.Time = log1p(Resolution.Time))



# Plot the map
ggplot() +
  # Add the Bronx shapefile
  geom_sf(data = bronx_shape, fill = NA, color = "black") +
  # Add complaint points colored by resolution time
  geom_point(data = bronx_data, aes(x = Longitude, y = Latitude, color = Resolution.Time), size = 0.7, alpha = 0.5) +
  # Set the color scale for resolution time
  scale_color_viridis(option = "plasma", name = "log(Resolution Time) (hours)", na.value = "grey50") +
  labs(
    title = "Queens Complaints by Resolution Time",
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


################################################################


# Load necessary libraries
library(dplyr)
library(ggplot2)
library(lubridate)

# Ensure the data has the Created.Date column in date format
data$Created.Date <- as.Date(data$Created.Date)

# Filter for Bronx data and calculate average weekly resolution time
weekly_avg_resolution <- data %>%
  filter(Borough == "bronx", !is.na(Resolution.Time)) %>%
  # Group by week
  mutate(Week = floor_date(Created.Date, "week")) %>%
  group_by(Week) %>%
  # Calculate average resolution time for each week
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop")

# Plot the weekly average resolution time for the Bronx
ggplot(weekly_avg_resolution, aes(x = Week, y = Average.Resolution.Time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Weekly Resolution Time for Bronx Complaints",
    x = "Week",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#######################################################33

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Filter for Bronx data and calculate the average resolution time for each neighborhood or city
city_avg_resolution <- data %>%
  filter(Borough == "queens", !is.na(Resolution.Time), !is.na(City)) %>%
  group_by(City) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time))

# Plot the average resolution time for each city or neighborhood within the Bronx
ggplot(city_avg_resolution, aes(x = reorder(City, -Average.Resolution.Time), y = Average.Resolution.Time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Resolution Time by Neighborhood in the Bronx",
    x = "Neighborhood",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###################################################################3

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Filter for Bronx data and calculate the average resolution time for each street
street_avg_resolution <- data %>%
  filter(Borough == "queens", !is.na(Resolution.Time), !is.na(Street.Name)) %>%
  group_by(Street.Name) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time)) %>%
  slice_max(order_by = Average.Resolution.Time, n = 20)  # Select the top 20 streets

# Plot the average resolution time for the top 20 streets in the Bronx
ggplot(street_avg_resolution, aes(x = reorder(Street.Name, -Average.Resolution.Time), y = Average.Resolution.Time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Top 20 Streets by Average Resolution Time in the Bronx",
    x = "Street",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

###################################################

library(dplyr)
library(ggplot2)
library(sf) # For spatial data handling

# Load Bronx shapefile or boundary data
bronx_boundary <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME == "Queens")

# Filter Bronx data, calculate average resolution time per street, and select top 20
top20_streets_data <- data %>%
  filter(Borough == "queens", !is.na(Resolution.Time), !is.na(Street.Name), !is.na(Longitude), !is.na(Latitude)) %>%
  group_by(Street.Name) %>%
  summarise(
    Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE),
    Longitude = mean(Longitude, na.rm = TRUE),  # Use average coordinates per street
    Latitude = mean(Latitude, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(Average.Resolution.Time)) %>%
  slice_max(order_by = Average.Resolution.Time, n = 10000000000)  # Select top 20 streets

# Convert to spatial data frame if necessary
top20_streets_sf <- st_as_sf(top20_streets_data, coords = c("Longitude", "Latitude"), crs = st_crs(bronx_boundary))


# Assuming the top20_streets_sf contains geometry and Average.Resolution.Time for the top 20 streets

library(ggplot2)
library(sf)
library(viridis)

library(dplyr)
library(ggplot2)
library(sf)
library(viridis)

# Step 1: Normalize the Average Resolution Time
top20_streets_sf <- top20_streets_sf %>%
  mutate(Normalized_Avg_Resolution_Time = log1p(Average.Resolution.Time))

# Step 2: Create the plot
ggplot() +
  # Plot the Bronx boundary
  geom_sf(data = bronx_boundary, fill = NA, color = "black") +
  
  # Add points for each street, with circles filled by normalized average resolution time
  geom_sf(data = top20_streets_sf, aes(color = Normalized_Avg_Resolution_Time, fill = Normalized_Avg_Resolution_Time), 
          size = 2, shape = 21, alpha = 0.7) +  # alpha adjusts the transparency
  
  # Apply a color scale to represent the normalized average resolution time
  scale_fill_viridis_c(option = "plasma", name = "Log(Avg Resolution Time)") +
  scale_color_viridis_c(option = "plasma", name = "Log(Avg Resolution Time)", guide = "none") +  # Remove border legend
  
  # Add titles and axis labels
  labs(
    title = "Top 150 Queens Streets by Average Resolution Time",
  ) +
  
  # Theme for a minimal map
  theme_minimal() +
  
  # Optionally, you can adjust the coordinate system and boundaries for a better visual
  coord_sf(datum = NA)  # Removes coordinate system to avoid distortion




#######################################

library(dplyr)
library(ggplot2)
library(sf)

# Identify the top 20 streets in the Bronx by average resolution time
top20_streets <- data %>%
  filter(Borough == "bronx", !is.na(Resolution.Time), !is.na(Street.Name)) %>%
  group_by(Street.Name) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time)) %>%
  slice_max(order_by = Average.Resolution.Time, n = 50) %>%
  pull(Street.Name)  # Extract only the street names

# Filter original data for cases on these top 20 streets
top20_cases <- data %>%
  filter(Borough == "bronx", Street.Name %in% top20_streets, !is.na(Longitude), !is.na(Latitude))


# Load Bronx boundary shapefile or boundary data
bronx_boundary <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME == "Bronx")

# Plot each case in the top 20 streets by resolution time
ggplot() +
  # Bronx borough boundary
  geom_sf(data = bronx_boundary, fill = NA, color = "black") +
  # Plot each complaint location as a point, colored by resolution time
  geom_point(data = top20_cases, aes(x = Longitude, y = Latitude, color = Resolution.Time), alpha = 0.7, size = 1) +
  scale_color_viridis_c(option = "plasma", name = "Resolution Time (hrs)") +
  labs(
    title = "Individual Complaint Cases on Top 20 Bronx Streets by Resolution Time",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()


#############################################################################

# Filter for Bronx data and calculate the average resolution time for each neighborhood or city
city_avg_resolution <- data %>%
  filter(Borough == "bronx", !is.na(Resolution.Time), !is.na(Agency)) %>%
  group_by(Agency) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(Average.Resolution.Time))

# Plot the average resolution time for each city or neighborhood within the Bronx
ggplot(city_avg_resolution, aes(x = reorder(Agency, -Average.Resolution.Time), y = Average.Resolution.Time)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Average Resolution Time by Neighborhood in the Bronx",
    x = "Agency",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################################################


dem_borough <- data.frame(
  Borough = c('bronx', 'brooklyn', 'manhattan', 'queens', 'staten island'),
  Population = c(1472654, 2736074, 1694251, 2405464, 495747),
  People.Sqr.Mile = c(34920, 39438,	74781, 22125, 8618)
)

data_no <- data %>%
  filter(Agency != 'doitt')

average_resolution <- data_no %>%
  group_by(Borough)

average_resolution <- average_resolution %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

combined_data <- average_resolution %>%
  inner_join(dem_borough, by = "Borough")


ggpl


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


# Group data by month and calculate average resolution time in the Bronx
ok <- data %>%
  filter(Borough == "bronx") %>%
  mutate(Day = format(as.Date(Created.Date), "%d")) %>%
  group_by(Day) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE))
  
ggplot(data = ok, aes(x = Day, y = Average.Resolution.Time, group = 1)) +
  geom_line() +
  labs(title = "Average Resolution Time by Month in the Bronx", x = "Month", y = "Resolution Time (Hours)") +
  theme_minimal()



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

################################################
library(dplyr)
library(lubridate)  # Make sure to load the lubridate package
library(ggplot2)

# Calculate average resolution time per day for the Bronx
bronx_avg_daily <- data %>%
  filter(Borough == "bronx") %>%
  mutate(Created.Date = as.Date(Created.Date)) %>%  # Mutate to keep only the date part
  group_by(Created.Date) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Comparison = "bronx", Borough = 'bronx')

# Calculate average resolution time per day for other boroughs
other_boroughs_avg_daily <- data %>%
  filter(Borough != "bronx", Borough != 'unspecified', !is.na(Borough)) %>%
  mutate(Created.Date = as.Date(Created.Date)) %>%  # Mutate to keep only the date part
  group_by(Created.Date, Borough) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  mutate(Comparison = "Other Boroughs")

# Combine both datasets (Bronx and other boroughs)
combined_data_daily <- bind_rows(bronx_avg_daily, other_boroughs_avg_daily)

# Create the line plot to compare average resolution time by day for Bronx vs other boroughs
ggplot(combined_data_daily, aes(x = Created.Date, y = Average.Resolution.Time, color = Borough)) +
  geom_line(size = 1.5) +
  labs(
    title = "Average Resolution Time by Borough (Daily)",
    x = "Date",
    y = "Average Resolution Time (Hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate axis labels for better readability
  scale_color_viridis_d()
  



##################################################

library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Group by week and borough, and calculate average resolution time
data_no <- data

weekly_resolution <- data_no %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  mutate(Week = floor_date(Created.Date, unit = "week")) %>%  # Group by week
  group_by(Borough, Week) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Combine the weekly data with population data
combined_weekly_data <- weekly_resolution %>%
  inner_join(dem_borough, by = "Borough")

# Step 3: Create a scatter plot for each week

ggplot(combined_weekly_data, aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +  # Adding borough names
  facet_wrap(~ Week, scales = "free_x") +  # Separate plot for each week
  labs(title = "Average Resolution Time vs People per Square Mile by Week",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")  # Prevent clipping of labels


################################################################

library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Group by week and borough, and calculate average resolution time
data_no <- data

bronx_data <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time), Complaint.Type.Clean != 'unspecified') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Get the top 5 complaint types with the highest average resolution time in the Bronx
top_5_complaints <- bronx_data %>%
  arrange(desc(Average.Time)) %>%
  head(6)

weekly_resolution <- data_no %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Combine the weekly data with population data
combined_weekly_data <- weekly_resolution %>%
  inner_join(dem_borough, by = "Borough") %>%
  filter(Complaint.Type.Clean %in% top_5_complaints$Complaint.Type.Clean)

# Step 3: Create a scatter plot for each week

ggplot(combined_weekly_data, aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +  # Adding borough names
  facet_wrap(~ Complaint.Type.Clean, scales = "free") +  # Separate plot for each week
  labs(title = "Average Resolution Time vs People per Square Mile by Week",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")  # Prevent clipping of labels

####################################

library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Filter data for the Bronx and calculate average resolution time for each complaint type
bronx_data <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time), Complaint.Type.Clean != 'unspecified') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Get the top 5 complaint types with the highest average resolution time in the Bronx
top_5_complaints <- bronx_data %>%
  arrange(desc(Average.Time)) %>%
  head(6)

# Step 3: Create a new column for the "Other" complaints
data_no <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time)) %>%
  mutate(Complaint.Type.Clean = ifelse(Complaint.Type.Clean == 'scaffold safety', 'scaffold safety', 'Other'))

# Step 4: Calculate weekly resolution times for the complaint types of interest
weekly_resolution <- data_no %>%
  group_by(Borough, Complaint.Type.Clean, Week = floor_date(Created.Date, "week")) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 5: Combine the weekly data with population data
combined_weekly_data <- weekly_resolution %>%
  inner_join(dem_borough, by = "Borough") %>%
  filter(Complaint.Type.Clean %in% c('scaffold safety', 'Other'))  # Filter for "Scaffold Safety" and "Other"

# Step 6: Create separate plots for Scaffold Safety and Other complaints

# Plot for Scaffold Safety Complaints
ggplot(subset(combined_weekly_data, Complaint.Type.Clean == 'scaffold safety'), aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +
  labs(title = "Average Resolution Time for Scaffold Safety Complaints vs People per Square Mile",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")

# Plot for Other Complaints
ggplot(subset(combined_weekly_data, Complaint.Type.Clean == 'Other'), aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +
  labs(title = "Average Resolution Time for Other Complaints vs People per Square Mile",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")



################################

library(dplyr)

# Step 1: Filter out Scaffold Safety and other complaint types
# Dataset for 'Scaffold Safety' complaint type

take_off <- c('bus stop shelter complaint', 'scaffold safety')

what <- data %>%
  filter(Complaint.Type.Clean %in% take_off, Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Dataset for all other complaint types except 'Scaffold Safety'
other_complaints_data <- data %>%
  filter(Complaint.Type.Clean %notin% take_off, Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

what <- what %>%
  mutate(Complaint.Type.Clean = 'affected')

other_complaints_data <- other_complaints_data %>%
  mutate(Complaint.Type.Clean = 'other complaints')

# Combine the two datasets
combined_data <- bind_rows(what, other_complaints_data)

combined_data <- combined_data %>% 
  inner_join(dem_borough, by = "Borough")

# Plot for Other Complaints and Scaffold Safety with top and bottom representation
ggplot(combined_data, aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +
  facet_wrap(~ Complaint.Type.Clean, scales = "free", ncol = 1) +  # Reset y-axis for each facet
  labs(title = "Average Resolution Time for Scaffold Safety and Other Complaints",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")


##################################

library(dplyr)
library(ggplot2)
library(lubridate)

# Step 1: Group by week and borough, and calculate average resolution time
data_no <- data

bronx_data <- data %>%
  filter(Borough == 'bronx', !is.na(Resolution.Time), Complaint.Type.Clean != 'unspecified') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Get the top 5 complaint types with the highest average resolution time in the Bronx
top_5_complaints <- bronx_data %>%
  arrange(desc(Average.Time)) %>%
  head(6)

weekly_resolution <- data_no %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Combine the weekly data with population data
combined_weekly_data <- weekly_resolution %>%
  inner_join(dem_borough, by = "Borough") %>%
  filter(Complaint.Type.Clean %in% top_5_complaints$Complaint.Type.Clean)

# Step 3: Fit the smooth line and calculate the residuals
model <- lm(Average.Time ~ People.Sqr.Mile, data = combined_weekly_data)

# Calculate residuals
combined_weekly_data <- combined_weekly_data %>%
  mutate(Fitted.Values = predict(model), 
         Residuals = Average.Time - Fitted.Values)

# Identify the top 5 points with the highest absolute residuals
top_5_outliers <- combined_weekly_data %>%
  arrange(desc(abs(Residuals))) %>%
  head(5)

# Step 4: Plot the data
ggplot(combined_weekly_data, aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +
  geom_point(data = top_5_outliers, aes(x = People.Sqr.Mile, y = Average.Time), 
             color = "red", size = 4, shape = 1) +  # Highlight the outliers
  facet_wrap(~ Complaint.Type.Clean, scales = "free") +  # Separate plot for each week
  labs(title = "Average Resolution Time vs People per Square Mile by Week with Outliers",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")  # Prevent clipping of labels


#############################################
library(dplyr)
library(ggplot2)

# Step 1: Calculate average resolution time for each complaint type and borough
data_no <- data 

# Calculate frequency of each complaint type by borough
complaint_counts <- data_no %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Count = n(), .groups = 'drop')

# Calculate average resolution time by borough and complaint type
library(dplyr)

average_resolution <- data_no %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = sum(Resolution.Time, na.rm = TRUE) / n(), .groups = 'drop')


# Merge complaint counts with average resolution times
average_resolution <- average_resolution %>%
  inner_join(complaint_counts, by = c("Borough", "Complaint.Type.Clean"))

# Step 2: Calculate the contribution of each complaint type to the total resolution time by borough
# The contribution will be the weighted time for each complaint type (Average.Time * Count)

# Step 3: For each borough, filter to keep only the top 5 complaint types by weighted time
top_complaints_per_borough <- average_resolution %>%
  group_by(Borough) %>%
  arrange(desc(Average.Time)) %>%
  slice_head(n = 5)  # Select top 5 complaint types for each borough

# Step 4: Create the stacked bar chart for each borough's complaint type contributions (top 5 only)
ggplot(top_complaints_per_borough, aes(x = Borough, y = Average.Time, fill = Complaint.Type.Clean)) +
  geom_bar(stat = "identity") +
  labs(title = "Top 5 Complaint Types Contribution to Average Resolution Time by Borough",
       x = "Borough",
       y = "Weighted Resolution Time (hours)",
       fill = "Complaint Type") +
  theme_minimal() + 
  geom_text(
    aes(label = round(Average.Time, 1)),  # Label with one decimal place
    position = position_stack(vjust = 0.5),  # Position text in the middle of the stacked bars
    size = 3       # Adjust text size
  )



############################33

library(dplyr)

# Assuming `data` is your dataset

# Step 1: Calculate average resolution time for each complaint type by borough
average_resolution_by_complaint <- data %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Sort by Borough and Average.Time (ascending) and get top 5 complaint types for each borough
top_5_complaints_by_borough <- average_resolution_by_complaint %>%
  arrange(Borough, Average.Time) %>%  # Sort by Borough and Average.Time (ascending)
  group_by(Borough) %>%  # Group by Borough to apply the top_n function
  slice_head(n = 5)  # Select top 5 complaint types per borough

# View the result
print(top_5_complaints_by_borough)




library(dplyr)
library(ggplot2)

# Step 1: Calculate average resolution time for each complaint type by borough
average_resolution_by_complaint <- data %>%
  filter(Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 2: Sort by Borough and Average.Time (ascending) and get top 5 complaint types for each borough
top_5_complaints_by_borough <- average_resolution_by_complaint %>%
  arrange(Borough, Average.Time) %>%  # Sort by Borough and Average.Time (ascending)
  group_by(Borough) %>%  # Group by Borough to apply the top_n function
  slice_head(n = 5)  # Select top 5 complaint types per borough

# Step 3: Plotting the bar chart
ggplot(top_5_complaints_by_borough, aes(x = reorder(Borough, Average.Time), y = Average.Time, fill = Complaint.Type.Clean)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +  # Flip coordinates for a horizontal bar chart
  labs(title = "Top 5 Complaint Types with the Least Resolution Time by Borough",
       x = "Complaint Type",
       y = "Average Resolution Time (hours)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#####################################


# Step 1: Filter data for Queens and other boroughs, and calculate average resolution time
# Step 1: Count number of complaints by type in Queens
top_complaints_queens <- data %>%
  filter(!is.na(Complaint.Type.Clean), Borough != 'unspecified') %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Complaint.Count = n(), .groups = 'drop') %>%
  arrange(desc(Complaint.Count)) %>%
  head(5)  # Get the top 5 complaint types by count in Queens



# Step 2: Filter out top 5 complaints and add data for other boroughs
top_complaints_all_boroughs <- data %>%
  filter(Complaint.Type.Clean %in% top_complaints_queens$Complaint.Type.Clean, 
         !is.na(Resolution.Time), 
         Borough != 'unspecified') %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 3: Create the bar plot for top 5 complaints in Queens compared to other boroughs
ggplot(top_complaints_all_boroughs, aes(x = reorder(Complaint.Type.Clean, Average.Time), y = Average.Time, fill = Borough)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bars representing the average resolution time
  labs(title = "Average Resolution Time for Top 5 Complaints",
       x = "Complaint Type",
       y = "Average Resolution Time (hours)") +
  theme_minimal() +
  coord_flip() +  # Flip the coordinates to make the bar chart horizontal
  scale_fill_brewer(palette = "Set2")  # Use a color palette for the fill







#################################################################


library(dplyr)
library(ggplot2)
library(viridis)

# Step 1: Aggregate average resolution time by coordinates
complaint_data <- data %>%
  filter(Borough == "bronx", !is.na(Resolution.Time), !is.na(Latitude), !is.na(Longitude)) %>%
  group_by(Latitude, Longitude) %>%
  summarise(Average.Resolution.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

complaint_data <- complaint_data %>%
  mutate(Normalized_Avg_Resolution_Time = log1p(Average.Resolution.Time))
# Load Bronx boundary shapefile or boundary data
bronx_boundary <- counties(state = "NY", cb = TRUE, class = "sf") %>%
  filter(NAME == "Bronx")


# Step 2 Plot the heatmap using ggplot
ggplot(complaint_data, aes(x = Longitude, y = Latitude, color = Normalized_Avg_Resolution_Time)) +
  geom_sf(data = bronx_boundary, fill = NA, color = "black") +
  geom_point(size = 0.7, alpha = 0.6) +  # Plot points with transparency
  scale_color_viridis_c(option = "plasma", name = "Avg Resolution Time (hrs)") +  # Color scale
  labs(
    title = "Heatmap of Average Resolution Time by Complaint Location",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(legend.position = "right")  # Place the legend to the right




################################################


library(dplyr)
library(ggplot2)

# Step 1: Filter for Queens data
queens_data <- data %>%
  filter(Borough == "queens", !is.na(Resolution.Time), Borough != "unspecified")

# Step 2: Get the top 5 complaint types with the highest average resolution time in Queens
top_5_queens_complaints <- queens_data %>%
  group_by(Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop') %>%
  arrange(desc(Average.Time)) %>%
  head(6)

# Step 3: Calculate average resolution time for the same complaint types in other boroughs
other_boroughs_data <- data %>%
  filter(!is.na(Resolution.Time), !is.na(Borough),Borough != 'unspecified' ,Complaint.Type.Clean %in% top_5_queens_complaints$Complaint.Type.Clean) %>%
  group_by(Borough, Complaint.Type.Clean) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 4: Combine the data for plotting
combined_data <- bind_rows(
  queens_data %>%
    filter(Complaint.Type.Clean %in% top_5_queens_complaints$Complaint.Type.Clean) %>%
    mutate(Borough = "queens"),
  other_boroughs_data
)

# Step 5: Create a bar plot to compare average resolution time by borough and complaint type
ggplot(other_boroughs_data, aes(x = Complaint.Type.Clean, y = Average.Time, fill = Borough)) +
  geom_bar(stat = "identity", position = position_dodge(), color = "black") +
  labs(
    title = "Top 5 Complaints with the Longest Resolution Time in Queens",
    x = "Complaint Type",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

######################################################


library(dplyr)

# Step 1: Filter out Scaffold Safety and other complaint types
# Dataset for 'Scaffold Safety' complaint type

take_off <- c('quality of life')

what <- data %>%
  filter(Complaint.Type.Clean %in% take_off, Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Dataset for all other complaint types except 'Scaffold Safety'
other_complaints_data <- data %>%
  filter(Complaint.Type.Clean %notin% take_off, Borough != 'unspecified', !is.na(Resolution.Time)) %>%
  group_by(Borough) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

what <- what %>%
  mutate(Complaint.Type.Clean = 'Quality of Life')

other_complaints_data <- other_complaints_data %>%
  mutate(Complaint.Type.Clean = 'Other Complaints')

# Combine the two datasets
combined_data <- bind_rows(what, other_complaints_data)

combined_data <- combined_data %>% 
  inner_join(dem_borough, by = "Borough")

# Plot for Other Complaints and Scaffold Safety with top and bottom representation
ggplot(combined_data, aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +
  facet_wrap(~ Complaint.Type.Clean, scales = "free", ncol = 1) +  # Reset y-axis for each facet
  labs(title = "Average Resolution Time for Scaffold Safety and Other Complaints",
       x = "People per Square Mile",
       y = "Average Resolution Time (hours)") +
  theme_minimal() + 
  coord_cartesian(clip = "off")

#################################

# Step 1: Identify complaint types that are exclusive to the Bronx
bronx_exclusive_complaints <- data %>%
  filter(Borough == 'bronx') %>%
  distinct(Complaint.Type.Clean)

# Step 2: Identify complaints present in other boroughs
other_borough_complaints <- data %>%
  filter(Borough != 'bronx') %>%
  distinct(Complaint.Type.Clean)

exclusive <- bronx_exclusive_complaints %>%
  filter(Complaint.Type.Clean %notin% other_borough_complaints$Complaint.Type.Clean)

# Step 3: Keep only complaints that are found in at least one other borough
# (by excluding those exclusive to the Bronx)
shared_complaints <- other_borough_complaints %>%
  filter(Complaint.Type.Clean %in% exclusive$Complaint.Type.Clean)

# Step 4: Filter the data for complaints in `shared_complaints`
filtered_data <- data %>%
  filter(
    Complaint.Type.Clean %in% shared_complaints$Complaint.Type.Clean,
    Borough != 'unspecified', !is.na(Resolution.Time), !is.na(Borough)
  ) %>%
  group_by(Borough) %>%
  summarise(Average.Time = mean(Resolution.Time, na.rm = TRUE), .groups = 'drop')

# Step 5: Join demographic data
filtered_data <- filtered_data %>%
  inner_join(dem_borough, by = "Borough")

# Step 6: Plot for complaint types in all boroughs (excluding Bronx-only complaints)
ggplot(filtered_data, aes(x = People.Sqr.Mile, y = Average.Time, color = Borough)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", color = "green", se = TRUE) +
  geom_text(aes(label = Borough), vjust = -0.5, size = 3) +
  labs(
    title = "Average Resolution Time for Complaint Types (Excluding Bronx-Only Complaints)",
    x = "People per Square Mile",
    y = "Average Resolution Time (hours)"
  ) +
  theme_minimal() +
  coord_cartesian(clip = "off")  # Prevent clipping of labels


############################################################









