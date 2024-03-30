#### Preamble ####
# Purpose: Exploration of the dataset to gather trends and determine appropriate models  
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(here)

#### Creating Summary Statistics and Visualizations ####

# Load the cleaned data
cleaned_data <- read_csv(here("data", "analysis_data", "cleaned-data-2010-2019.csv"))

## Summary Statistics ###

# Generate summary statistics 
summary_stats <- cleaned_data %>%
  summarise(
    mean_daily_sb_cars = mean(daily_sb_cars, na.rm = TRUE),
    median_daily_sb_cars = median(daily_sb_cars, na.rm = TRUE),
    mean_daily_nb_cars = mean(daily_nb_cars, na.rm = TRUE),
    median_daily_nb_cars = median(daily_nb_cars, na.rm = TRUE),
    mean_daily_wb_cars = mean(daily_wb_cars, na.rm = TRUE),
    median_daily_wb_cars = median(daily_wb_cars, na.rm = TRUE),
    mean_daily_eb_cars = mean(daily_eb_cars, na.rm = TRUE),
    median_daily_eb_cars = median(daily_eb_cars, na.rm = TRUE),
    # Buses
    mean_daily_sb_bus = mean(daily_sb_bus, na.rm = TRUE),
    median_daily_sb_bus = median(daily_sb_bus, na.rm = TRUE),
    mean_daily_nb_bus = mean(daily_nb_bus, na.rm = TRUE),
    median_daily_nb_bus = median(daily_nb_bus, na.rm = TRUE),
    mean_daily_wb_bus = mean(daily_wb_bus, na.rm = TRUE),
    median_daily_wb_bus = median(daily_wb_bus, na.rm = TRUE),
    mean_daily_eb_bus = mean(daily_eb_bus, na.rm = TRUE),
    median_daily_eb_bus = median(daily_eb_bus, na.rm = TRUE),
    # Pedestrians
    mean_daily_nx_peds = mean(daily_nx_peds, na.rm = TRUE),
    median_daily_nx_peds = median(daily_nx_peds, na.rm = TRUE),
    mean_daily_sx_peds = mean(daily_sx_peds, na.rm = TRUE),
    median_daily_sx_peds = median(daily_sx_peds, na.rm = TRUE),
    mean_daily_ex_peds = mean(daily_ex_peds, na.rm = TRUE),
    median_daily_ex_peds = median(daily_ex_peds, na.rm = TRUE),
    mean_daily_wx_peds = mean(daily_wx_peds, na.rm = TRUE),
    median_daily_wx_peds = median(daily_wx_peds, na.rm = TRUE),
    # Bike
    mean_daily_nx_bike = mean(daily_nx_bike, na.rm = TRUE),
    median_daily_nx_bike = median(daily_nx_bike, na.rm = TRUE),
    mean_daily_sx_bike = mean(daily_sx_bike, na.rm = TRUE),
    median_daily_sx_bike = median(daily_sx_bike, na.rm = TRUE),
    mean_daily_ex_bike = mean(daily_ex_bike, na.rm = TRUE),
    median_daily_ex_bike = median(daily_ex_bike, na.rm = TRUE),
    mean_daily_wx_bike = mean(daily_wx_bike, na.rm = TRUE),
    median_daily_wx_bike = median(daily_wx_bike, na.rm = TRUE)
  )

# Print summary statistics
View(summary_stats)


## VISUALIZE DATA ##

## Car 
# Aggregate total car traffic in each direction by date and generate plot
line_graph_car <- cleaned_data %>%
  group_by(count_date) %>%
  summarise(
    total_sb_cars = sum(daily_sb_cars, na.rm = TRUE),
    total_nb_cars = sum(daily_nb_cars, na.rm = TRUE),
    total_eb_cars = sum(daily_eb_cars, na.rm = TRUE),
    total_wb_cars = sum(daily_wb_cars, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("total_"), names_to = "direction", values_to = "total_cars") %>%
  ggplot(aes(x = count_date, y = total_cars)) +
  geom_smooth(aes(color = direction), se = FALSE) +  
  labs(title = "Daily Cars Traffic Over Time by Direction",
       x = "Date",
       y = "Total Cars Traffic") +
  theme_minimal()

line_graph_car

# Save plot 
ggsave("./other/outputs/car_traffic_over_time.png", plot = line_graph_car, width = 12, height = 6, units = "in")

## Bus 
# Aggregate total bus traffic in each direction by date and generate plot
line_graph_buses <- cleaned_data %>%
  group_by(count_date) %>%
  summarise(
    total_sb_bus = sum(daily_sb_bus, na.rm = TRUE),
    total_nb_bus = sum(daily_nb_bus, na.rm = TRUE),
    total_eb_bus = sum(daily_eb_bus, na.rm = TRUE),
    total_wb_bus = sum(daily_wb_bus, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("total_"), names_to = "direction", values_to = "total_bus") %>%
  ggplot(aes(x = count_date, y = total_bus)) +
  geom_smooth(aes(color = direction), se = FALSE) +
  labs(title = "Daily Bus Traffic Over Time by Direction",
       x = "Date",
       y = "Total Bus Traffic") +
  theme_minimal()

line_graph_buses

# Save plot 
ggsave("./other/outputs/bus_traffic_over_time.png", plot = line_graph_buses, width = 12, height = 6, units = "in")

## Pedestrian
# Aggregate total pedestrian traffic in each direction by date and generate plot
line_graph_pedestrians <- cleaned_data %>%
  group_by(count_date) %>%
  summarise(
    total_nx_peds = sum(daily_nx_peds, na.rm = TRUE),
    total_sx_peds = sum(daily_sx_peds, na.rm = TRUE),
    total_ex_peds = sum(daily_ex_peds, na.rm = TRUE),
    total_wx_peds = sum(daily_wx_peds, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("total_"), names_to = "direction", values_to = "total_peds") %>%
  ggplot(aes(x = count_date, y = total_peds)) +
  geom_smooth(aes(color = direction), se = FALSE) +
  labs(title = "Daily Pedestrian Traffic Over Time by Direction",
       x = "Date",
       y = "Total Pedestrian Traffic") +
  theme_minimal()

line_graph_pedestrians

# Save plot 
ggsave("./other/outputs/pedestrian_traffic_over_time.png", plot = line_graph_pedestrians, width = 12, height = 6, units = "in")

## Bike
# Aggregate total bike traffic in each direction by date and generate plot
line_graph_cyclists <- cleaned_data %>%
  group_by(count_date) %>%
  summarise(
    total_nx_bike = sum(daily_nx_bike, na.rm = TRUE),
    total_sx_bike = sum(daily_sx_bike, na.rm = TRUE),
    total_ex_bike = sum(daily_ex_bike, na.rm = TRUE),
    total_wx_bike = sum(daily_wx_bike, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = starts_with("total_"), names_to = "direction", values_to = "total_bike") %>%
  ggplot(aes(x = count_date, y = total_bike)) +
  geom_smooth(aes(color = direction), se = FALSE) +
  labs(title = "Daily Cyclist Traffic Over Time by Direction",
       x = "Date",
       y = "Total Cyclist Traffic") +
  theme_minimal()

line_graph_cyclists

# Save plot 
ggsave("./other/outputs/cyclist_traffic_over_time.png", plot = line_graph_cyclists, width = 12, height = 6, units = "in")

colnames(cleaned_data_2010_2019)
