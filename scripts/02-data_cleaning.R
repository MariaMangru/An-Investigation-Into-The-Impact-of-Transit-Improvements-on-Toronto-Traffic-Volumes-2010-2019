#### Preamble ####
# Purpose: Cleans the raw traffic counts recorded into a more manageable dataset 
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

#### Clean data ####

# access raw data 
data <- read.csv("./data/raw_data/raw-data-2010-2019.csv")

# Covert dates and filter based on years 2015 to 2019
processed_data <- data %>%
  mutate(count_date = ymd(count_date)) %>%
  filter(year(count_date) >= 2015, year(count_date) <= 2019)


# Removing less relevant columns to focus on total traffic volumes, buses, pedestrians, and cyclists
processed_data <- processed_data[, c("count_id", "count_date", "location_id", "location", 
                            "sb_cars_r","sb_cars_t","sb_cars_l","nb_cars_r", "nb_cars_t", "nb_cars_l", "wb_cars_r",
                            "wb_cars_t","wb_cars_l", "eb_cars_r", "eb_cars_t","eb_cars_l", "sb_bus_r", "sb_bus_t", 
                            "sb_bus_l", "nb_bus_r", "nb_bus_t", "nb_bus_l", "wb_bus_r", "wb_bus_t", "wb_bus_l", "eb_bus_r", "eb_bus_t", 
                            "eb_bus_l", "nx_peds", "sx_peds", "ex_peds", "wx_peds", "nx_bike", "sx_bike", "ex_bike", "wx_bike", "centreline_type")]


# Aggregating data to daily traffic volumes 
daily_traffic <- processed_data %>%
  group_by(count_date, location_id, location) %>%
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


# Define streets to include and exclude 
# List of streets to keep
keep_streets <- c("SHEPPARD AVE W", "CHESSWOOD DR", "KEELE ST", "FINCH AVE W", 
                  "TANGIERS RD", "YORK BLVD", "IAN MACDONALD BLVD", "SENTINEL RD", 
                  "THE POND RD", "STEELES AVE W", "NORTHWEST GT", "FOUNDERS RD", "JANE ST")

# Filter data for relevant streets
daily_traffic <- daily_traffic %>%
  filter(str_detect(location, regex(paste(keep_streets, collapse = "|"), ignore_case = TRUE)))


# List of specific intersections to remove
remove_intersections <- c(
  "BATHURST", "BREMNER BLVD", "FORT YORK BLVD", "SPADINA AVE",
  "DUNDAS ST", "ANNETTE ST", "SIGNET DR", "CARPENTER RD", 
  "GIHON SPRING DR", "BLACK CREEK BLVD", "HARLOCK BLVD", 
  "NELSON RD", "ST CLAIR AVE W", "DUFFERIN", "BLOOR ST W", 
  "EGLINTON AVE", "HULLMAR DR", "MARTIN GROVE RD"
)

# Remove specific intersections
daily_traffic <- daily_traffic %>%
  filter(!str_detect(location, regex(paste(remove_intersections, collapse = "|"), ignore_case = TRUE)))



#### Save data ####
write.csv(daily_traffic, "./data/analysis_data/cleaned-data-2010-2019.csv", row.names = FALSE)



unique_locations <- unique(cleaned_data_2010_2019$location)
print(unique_locations)
  
  
  
