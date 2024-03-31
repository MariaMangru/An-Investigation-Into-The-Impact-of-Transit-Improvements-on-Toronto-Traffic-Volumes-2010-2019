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
library(here)

#### Clean data ####

# Reading the raw data
data <- read.csv(here("data", "raw_data", "raw-data-2010-2019.csv"))

# Converting dates and filtering data from 2014 to 2019
processed_data <- data %>%
  mutate(count_date = ymd(count_date)) %>%
  filter(year(count_date) >= 2014, year(count_date) <= 2019)


# Removing less relevant columns to focus on total traffic volumes
processed_data <- processed_data[, c("count_id", "count_date", "location_id", "location", 
                            "sb_cars_r","sb_cars_t","sb_cars_l","nb_cars_r", "nb_cars_t", "nb_cars_l", "wb_cars_r",
                            "wb_cars_t","wb_cars_l", "eb_cars_r", "eb_cars_t","eb_cars_l", "sb_bus_r", "sb_bus_t", 
                            "sb_bus_l", "nb_bus_r", "nb_bus_t", "nb_bus_l", "wb_bus_r", "wb_bus_t", "wb_bus_l", "eb_bus_r", "eb_bus_t", 
                            "eb_bus_l", "nx_peds", "sx_peds", "ex_peds", "wx_peds", "nx_bike", "sx_bike", "ex_bike", "wx_bike", "centreline_type")]


# Aggregating data to calculate daily traffic volumes
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


# Defining streets to include and exclude


# Streets to include
streets_to_include <- c("SHEPPARD AVE W", "CHESSWOOD DR", "KEELE ST", "FINCH AVE W", "TANGIERS RD",
                        "YORK BLVD", "IAN MACDONALD BLVD", "SENTINEL RD", "THE POND RD", "STEELES AVE W", 
                        "NORTHWEST GT", "FOUNDERS RD", "JANE ST")


# Creating a pattern for streets to exclude, case-insensitive
streets_to_exclude_pattern <- "(?i)" # makes the pattern case-insensitive
streets_to_exclude_pattern <- paste(streets_to_exclude_pattern, "BATHURST|BREMNER BLVD|FORT YORK BLVD|SPADINA AVE|DUNDAS ST|
                                    ANNETTE ST|SIGNET DR|CARPENTER RD|GIHON SPRING DR|BLACK CREEK BLVD|HARLOCK BLVD|
                                    NELSON RD|ST CLAIR AVE W AT KEELE ST & WESTON RD \\(PX 493\\)|DUFFERIN ST AT STEELES AVE W|
                                    BLOOR ST W AT JANE ST \\(PX 333\\)|BLOOR ST W AT KEELE ST & PARKSIDE DR \\(PX 328\\)|
                                    EGLINTON AVE AT JANE ST \\(PX 905\\)|BLACK CREEK DR S TCS AT JANE ST \\(PX 235\\)|
                                    BLACK CREEK DR N TCS AT JANE ST \\(PX 1418\\)|HULLMAR DR AT JANE ST \\(PX 2401\\)|
                                    MARTIN GROVE RD AT STEELES AVE W", collapse = "|")


# Filtering data based on streets to include and exclude
daily_traffic <- daily_traffic %>%
  filter(str_detect(string = location, pattern = paste0("(?i)", paste(streets_to_include, collapse = "|")))) %>%
  filter(!str_detect(string = location, pattern = streets_to_exclude_pattern))



#### Save data ####
write.csv(daily_traffic, here("data", "analysis_data", "cleaned-data-2010-2019.csv"), row.names = FALSE)

  
