library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)

data <- fread("/home/joaomonteiro/Desktop/VD/NYC_311_Data_20241009.csv", sep=";", fill=TRUE)

clean_data <- subset(data, select = -c(`Unique Key`, `Agency`,
                                       `Descriptor`, `Incident Address`,
                                       `Cross Street 1`, `Cross Street 2`,
                                       `Intersection Street 1`, `Intersection Street 2`,
                                       `Address Type`, `Landmark`, 
                                       `Facility Type`, `Resolution Description`,
                                       `Community Board`, `Park Facility Name`,
                                       `Vehicle Type`, `Taxi Company Borough`,
                                       `Taxi Pick Up Location`, `Bridge Highway Name`,
                                       `Bridge Highway Direction`, `Road Ramp`,
                                       `Bridge Highway Segment`))

clean_data <- clean_data %>% 
  mutate(across(where(is.character), toupper))

most_common_complaints <- clean_data %>%
  group_by(City, `Complaint Type`) %>%
  summarize(count = n()) %>%
  slice_max(count, n = 1) 

ggplot(most_common_complaints, aes(x = City, y = count, fill = `Complaint Type`)) +
  geom_col() +
  labs(title = "Most Common Complaint by City",
       x = "City",
       y = "Count of Complaints") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability



complaints_by_city <- clean_data %>%
  filter(City != "" & !is.na(City) & grepl("\\S", City)) %>%
  group_by(City) %>%
  summarize(num_complaints = n()) %>%
  arrange(desc(num_complaints)) %>%
  slice_max(num_complaints, n = 10)

complaints_by_city$City <- factor(complaints_by_city$City, levels = complaints_by_city$City)

# Plot number of complaints by city
ggplot(complaints_by_city, aes(x = City, y = num_complaints)) +
  geom_col(fill = "skyblue") +
  labs(title = "Number of Complaints by City",
       x = "City",
       y = "Number of Complaints") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


names(clean_data)

sum(is.na(clean_data$`Created Date`))
sum(is.na(clean_data$`Closed Date`))


teste <- clean_data %>%
  mutate(
    `Created Date` = trimws(`Created Date`),
    `Closed Date` = trimws(`Closed Date`)
  )



teste <- teste %>%
  mutate(
    `Created Date` = parse_date_time(`Created Date`, orders = c("mdy HMS", "mdy HM", "mdy HMS", "mdy HM", "mdy", "ymd HMS", "ymd HM", "dmy HM", "dmy", "ymd")),
    `Closed Date` = parse_date_time(`Closed Date`, orders = c("mdy HMS", "mdy HM", "mdy HMS", "mdy HM", "mdy", "ymd HMS", "ymd HM", "dmy HM", "dmy", "ymd"))
  )
