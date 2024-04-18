#### Preamble ####
# Purpose: Exploration of the dataset to gather trends and determine appropriate models  
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
# Load necessary libraries
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(here)

### Creating Summary Statistics and Visualizations ###

# Load the cleaned data
analysis_data <- read_csv(here("data", "analysis_data", "cleaned-data.csv"))

# Summary Statistics

# Generate and view summary statistics for traffic data
summary_stats <- analysis_data %>%
  summarise(
    Mean_Cars = mean(daily_cars, na.rm = TRUE),
    Median_Cars = median(daily_cars, na.rm = TRUE),
    Mean_Buses = mean(daily_bus, na.rm = TRUE),
    Median_Buses = median(daily_bus, na.rm = TRUE),
    Mean_Pedestrians = mean(daily_peds, na.rm = TRUE),
    Median_Pedestrians = median(daily_peds, na.rm = TRUE),
    Mean_Bicycles = mean(daily_bike, na.rm = TRUE),
    Median_Bicycles = median(daily_bike, na.rm = TRUE)
  ) 
# Display the summary statistics
View(summary_stats)

# Visualize Data

# Visualizing daily car traffic over time
line_graph_car <- analysis_data %>%
  ggplot(aes(x = count_date, y = daily_cars)) +
  geom_smooth(se = FALSE) +  
  labs(title = "Daily Cars Traffic Over Time",
       x = "Date",
       y = "Total Cars Traffic") +
  theme_minimal()

# Display car traffic plot
line_graph_car

# Save car traffic plot
ggsave("./other/outputs/car_traffic_over_time.png", plot = line_graph_car, width = 12, height = 6, units = "in")

# Visualizing daily bus traffic over time
line_graph_buses <- analysis_data %>%
  ggplot(aes(x = count_date, y = daily_bus)) +
  geom_smooth(se = FALSE) +
  labs(title = "Daily Bus Traffic Over Time",
       x = "Date",
       y = "Total Bus Traffic") +
  theme_minimal()

# Display bus traffic plot
line_graph_buses

# Save bus traffic plot
ggsave("./other/outputs/bus_traffic_over_time.png", plot = line_graph_buses, width = 12, height = 6, units = "in")

# Visualizing daily pedestrian traffic over time
line_graph_pedestrians <- analysis_data %>%
  ggplot(aes(x = count_date, y = daily_peds)) +
  geom_smooth(se = FALSE) +
  labs(title = "Daily Pedestrian Traffic Over Time",
       x = "Date",
       y = "Total Pedestrian Traffic") +
  theme_minimal()

# Display pedestrian traffic plot
line_graph_pedestrians

# Save pedestrian traffic plot
ggsave("./other/outputs/pedestrian_traffic_over_time.png", plot = line_graph_pedestrians, width = 12, height = 6, units = "in")

# Visualizing daily bike traffic over time
line_graph_cyclists <- analysis_data %>%
  ggplot(aes(x = count_date, y = daily_bike)) +
  geom_smooth(se = FALSE) +
  labs(title = "Daily Cyclist Traffic Over Time",
       x = "Date",
       y = "Total Cyclist Traffic") +
  theme_minimal()

# Display cyclist traffic plot
line_graph_cyclists

# Save cyclist traffic plot
ggsave("./other/outputs/cyclist_traffic_over_time.png", plot = line_graph_cyclists, width = 12, height = 6, units = "in")



# Visualizing car traffic with respect to the time trend
time_trend_graph_car <- analysis_data %>%
  ggplot(aes(x = time_trend, y = daily_cars)) +
  geom_point(aes(color = daily_cars), alpha = 0.5) + 
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  
  scale_color_gradient(low = "blue", high = "red") +
  labs(title = "Car Traffic Volume Over Time Trend",
       x = "Days Since Start",
       y = "Daily Car Traffic",
       color = "Daily Traffic") +
  theme_minimal()

# Display car traffic plot with time trend
time_trend_graph_car

# Save the car traffic plot with time trend
ggsave(here("other", "outputs", "car_traffic_time_trend.png"), plot = time_trend_graph_car, width = 12, height = 6, units = "in")





