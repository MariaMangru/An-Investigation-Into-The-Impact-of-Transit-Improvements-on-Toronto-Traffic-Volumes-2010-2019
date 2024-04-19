#### Preamble ####
# Purpose: Cleans the raw traffic counts recorded into a more manageable dataset 
# Author: Maria Mangru
# Date: 18th April, 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(here)
library(arrow)  

#### Clean data ####

# Reading the raw data
data_2010_2019 <- read.csv(here("data", "raw_data", "raw-data-2010-2019.csv"))
data_2020_2029 <- read.csv(here("data", "raw_data", "raw-data-2020-2029.csv"))

# Combine datasets 
combined_data <- rbind(data_2010_2019, data_2020_2029)


# Check if dataset was properly combined. Code commented out after check completed.
# head(combined_data)
# tail(combined_data)
# str(combined_data)


# Converting dates and filtering data
filtered_data <- combined_data %>%
  filter((count_date >= as.Date("2011-12-31") & count_date <= as.Date("2014-12-31")) |
           (count_date >= as.Date("2018-11-01") & count_date <= as.Date("2020-02-29")))

filtered_data$count_date <- as.Date(filtered_data$count_date)

# Creating time_trend variable

# Determine the base date for the time trend 
base_date <- min(filtered_data$count_date, na.rm = TRUE)

# Adding the time_trend variable which counts days since the base date 
filtered_data <- filtered_data %>%
  mutate(time_trend = as.numeric(count_date - base_date))


# Removing less relevant columns to focus on total traffic volumes
processed_data <- filtered_data[, c("count_id", "count_date", "location", 
                            "sb_cars_r","sb_cars_t","sb_cars_l","nb_cars_r", "nb_cars_t", "nb_cars_l", "wb_cars_r",
                            "wb_cars_t","wb_cars_l", "eb_cars_r", "eb_cars_t","eb_cars_l", "sb_bus_r", "sb_bus_t", 
                            "sb_bus_l", "nb_bus_r", "nb_bus_t", "nb_bus_l", "wb_bus_r", "wb_bus_t", "wb_bus_l", 
                            "eb_bus_r", "eb_bus_t", "eb_bus_l", "nx_peds", "sx_peds", "ex_peds", "wx_peds", 
                            "nx_bike", "sx_bike", "ex_bike", "wx_bike", "lng", "lat", "time_trend")]


# Aggregating data to calculate daily traffic volumes
daily_traffic <- processed_data %>%
  group_by(count_date, location, lng, lat, count_id, time_trend) %>%
  summarise(
    daily_cars = sum(c_across(starts_with("sb_cars")) + 
                       c_across(starts_with("nb_cars")) + 
                       c_across(starts_with("wb_cars")) + 
                       c_across(starts_with("eb_cars")), na.rm = TRUE),
    daily_bus = sum(c_across(starts_with("sb_bus")) + 
                      c_across(starts_with("nb_bus")) + 
                      c_across(starts_with("wb_bus")) + 
                      c_across(starts_with("eb_bus")), na.rm = TRUE),
    daily_peds = sum(nx_peds, sx_peds, ex_peds, wx_peds, na.rm = TRUE),
    daily_bike = sum(nx_bike, sx_bike, ex_bike, wx_bike, na.rm = TRUE)
  ) %>%
  ungroup()


# Defining streets to include and exclude

# Create a buffer around the streetcar 
buffer_size <- 0.05

# Define the boundary 
min_lat <- min(43.59223915574012, 43.60098812884999) - buffer_size
max_lat <- max(43.59223915574012, 43.60098812884999) + buffer_size
min_lng <- min(-79.54399824169857, -79.50535297505884) - buffer_size
max_lng <- max(-79.54399824169857, -79.50535297505884) + buffer_size


# Filter data based on this boundary 
daily_traffic <- daily_traffic %>%
  filter(lat >= min_lat & lat <= max_lat &
           lng >= min_lng & lng <= max_lng)


# Adding population column 
daily_traffic <- daily_traffic %>%
  mutate(population = case_when(
    year(count_date) >= 2012 & year(count_date) <= 2015 ~ 8244,
    year(count_date) >= 2016 & year(count_date) <= 2020 ~ 8781
  ))

#### Save data ####
write.csv(daily_traffic, here("data", "analysis_data", "cleaned-data.csv"), row.names = FALSE)

# Saving as Parquet
write_parquet(x = daily_traffic, sink = here("data", "analysis_data", "daily_traffic.parquet"))

