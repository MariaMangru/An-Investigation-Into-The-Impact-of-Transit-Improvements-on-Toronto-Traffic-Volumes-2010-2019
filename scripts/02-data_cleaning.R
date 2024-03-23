#### Preamble ####
# Purpose: Cleans the raw traffic counts recorded into a more manageable dataset 
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)

#### Clean data ####

# access raw data 
data <- read.csv("./data/raw_data/raw-data-2010-2019.csv")

colnames(raw_data_2010_2019)

# Removing less relevant columns to focus on total traffic volumes, buses, pedestrians, and cyclists
data_simplified <- data[, c("count_id", "count_date", "location_id", "location", 
                            "sb_cars_t", "nb_cars_t", "wb_cars_t", "eb_cars_t",
                            "sb_bus_r", "nb_bus_r", "wb_bus_r", "eb_bus_r", 
                            "nx_peds", "sx_peds", "ex_peds", "wx_peds", 
                            "nx_bike", "sx_bike", "ex_bike", "wx_bike")]

# Correct data format 
data_simplified$count_date <- as.Date(data_simplified$count_date, format = "%m/%d/%Y")

# Aggregating data to daily traffic volumes 
daily_traffic_modes <- data_simplified %>%
  group_by(count_date, location_id, location) %>%
  summarise(daily_sb_cars_t = sum(sb_cars_t, na.rm = TRUE),
            daily_nb_cars_t = sum(nb_cars_t, na.rm = TRUE),
            daily_wb_cars_t = sum(wb_cars_t, na.rm = TRUE),
            daily_eb_cars_t = sum(eb_cars_t, na.rm = TRUE),
            daily_sb_bus = sum(sb_bus_r, na.rm = TRUE),
            daily_nb_bus = sum(nb_bus_r, na.rm = TRUE),
            daily_wb_bus = sum(wb_bus_r, na.rm = TRUE),
            daily_eb_bus = sum(eb_bus_r, na.rm = TRUE),
            daily_nx_peds = sum(nx_peds, na.rm = TRUE),
            daily_sx_peds = sum(sx_peds, na.rm = TRUE),
            daily_ex_peds = sum(ex_peds, na.rm = TRUE),
            daily_wx_peds = sum(wx_peds, na.rm = TRUE),
            daily_nx_bike = sum(nx_bike, na.rm = TRUE),
            daily_sx_bike = sum(sx_bike, na.rm = TRUE),
            daily_ex_bike = sum(ex_bike, na.rm = TRUE),
            daily_wx_bike = sum(wx_bike, na.rm = TRUE)) %>%
  ungroup()

# Removing outliers based on the interquartile range
remove_outliers <- function(data, column_name) {
  Q1 <- quantile(data[[column_name]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column_name]], 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  filtered_data <- data %>%
    filter(data[[column_name]] >= lower_bound & data[[column_name]] <= upper_bound)
  return(filtered_data)
}

# Applying the outlier removal function to each mode
modes_to_filter = c('daily_sb_cars_t', 'daily_nb_cars_t', 'daily_wb_cars_t', 'daily_eb_cars_t',
                    'daily_sb_bus', 'daily_nb_bus', 'daily_wb_bus', 'daily_eb_bus',
                    'daily_nx_peds', 'daily_sx_peds', 'daily_ex_peds', 'daily_wx_peds',
                    'daily_nx_bike', 'daily_sx_bike', 'daily_ex_bike', 'daily_wx_bike')

for(mode in modes_to_filter) {
  daily_traffic_modes <- remove_outliers(daily_traffic_modes, mode)
}



#### Save data ####
write.csv(daily_traffic_modes, "./data/analysis_data/cleaned-data-2010-2019.csv", row.names = FALSE)
