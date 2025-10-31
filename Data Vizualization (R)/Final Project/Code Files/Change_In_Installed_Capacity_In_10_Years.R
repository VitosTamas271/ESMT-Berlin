# Libraries
library(dplyr)
library(readr)
library(tidyverse)
library(lubridate)

# Reading the dataset
data <- read.csv("C:/Users/Tamás/Desktop/ESMT/FirstSem/DataVisualization/GroupProject/PreprocessingEmberData/PreprocessedData/merged_installed_generation_capacity.csv")

# Filling the missing values with 0
data[is.na(data)] <- 0

# Convert dates into proper format
data <- data %>%
  mutate(
    Start.date = as.Date(Start.date, format = "%m/%d/%Y"),
    End.date = as.Date(End.date, format = "%m/%d/%Y"),
    Year = year(Start.date)
  )

summary(data)

# Calculating the sum of the capacity per year
capacity_in_10_years <- data %>%
  group_by(Year) %>%
  summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  # Adding a new column with the total installed capacity per year
  mutate(Total_Installed_Capacity = rowSums(across(-Year)))

# Plotting the result
ggplot(capacity_in_10_years, aes(x = Year, y = Total_Installed_Capacity)) +
  geom_line(color = "blue", size = 1) +
  geom_point(size = 2.5, color = "red") +
  labs(
    title = "Germany: Installed Generation Capacity (2015 - 2025)",
    x = "Year",
    y = "Total Installed Capacity"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13))
  