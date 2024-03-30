#### Preamble ####
# Purpose: Cleans the raw traffic counts recorded into a more manageable dataset 
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(dplyr)
library(stringr)

#### Clean data ####

# access raw data 
data <- read.csv("./data/raw_data/raw-data-2010-2019.csv")

# Removing less relevant columns to focus on total traffic volumes, buses, pedestrians, and cyclists
data_simplified <- data[, c("count_id", "count_date", "location_id", "location", 
                            "sb_cars_r","sb_cars_t","sb_cars_l","nb_cars_r", "nb_cars_t", "nb_cars_l", "wb_cars_r",
                            "wb_cars_t","wb_cars_l", "eb_cars_r", "eb_cars_t","eb_cars_l", "sb_bus_r", "sb_bus_t", 
                            "sb_bus_l", "nb_bus_r", "nb_bus_t", "nb_bus_l", "wb_bus_r", "wb_bus_t", "wb_bus_l", "eb_bus_r", "eb_bus_t", 
                            "eb_bus_l", "nx_peds", "sx_peds", "ex_peds", "wx_peds", "nx_bike", "sx_bike", "ex_bike", "wx_bike", "centreline_type")]


# Aggregating data to daily traffic volumes 
daily_traffic_modes <- data_simplified %>%
  group_by(count_date, location_id, location) %>%
  summarise(daily_sb_cars = sum(sb_cars_t + sb_cars_r + sb_cars_l, na.rm = TRUE),
            daily_nb_cars = sum(nb_cars_t + nb_cars_r + nb_cars_l, na.rm = TRUE),
            daily_wb_cars = sum(wb_cars_t + wb_cars_r + wb_cars_l, na.rm = TRUE),
            daily_eb_cars = sum(eb_cars_t + eb_cars_r + eb_cars_l, na.rm = TRUE),
            daily_sb_bus = sum(sb_bus_r + sb_bus_t + sb_bus_l, na.rm = TRUE),
            daily_nb_bus = sum(nb_bus_r + nb_bus_t + nb_bus_l, na.rm = TRUE),
            daily_wb_bus = sum(wb_bus_r + wb_bus_t + wb_bus_l, na.rm = TRUE),
            daily_eb_bus = sum(eb_bus_r + eb_bus_t + eb_bus_l, na.rm = TRUE),
            daily_nx_peds = sum(nx_peds, na.rm = TRUE),
            daily_sx_peds = sum(sx_peds, na.rm = TRUE),
            daily_ex_peds = sum(ex_peds, na.rm = TRUE),
            daily_wx_peds = sum(wx_peds, na.rm = TRUE),
            daily_nx_bike = sum(nx_bike, na.rm = TRUE),
            daily_sx_bike = sum(sx_bike, na.rm = TRUE),
            daily_ex_bike = sum(ex_bike, na.rm = TRUE),
            daily_wx_bike = sum(wx_bike, na.rm = TRUE)) %>%
  ungroup()

daily_traffic_modes <- daily_traffic_modes %>%
  group_by(count_date, location_id, location) %>%
  summarise(daily_cars = sum(daily_sb_cars + daily_nb_cars + daily_wb_cars + daily_eb_cars, na.rm = TRUE),
            daily_bus = sum(daily_sb_bus + daily_nb_bus + daily_wb_bus + daily_eb_bus, na.rm = TRUE),
            daily_peds = sum(daily_nx_peds + daily_sx_peds + daily_ex_peds + daily_wx_peds, na.rm = TRUE),
            daily_bike = sum(daily_nx_bike + daily_sx_bike + daily_ex_bike +  daily_wx_bike, na.rm = TRUE)) %>%
  ungroup()


# List of streets which are relevant 
streets <- c("SHEPPARD AVE W", "CHESSWOOD DR", "KEELE ST", "FINCH AVE W", "TANGIERS RD",
             "YORK BLVD", "IAN MACDONALD BLVD", "SENTINEL RD", "THE POND RD", "STEELES AVE W", 
             "NORTHWEST GT", "FOUNDERS RD", "JANE ST")


# Filter dataset for rows where location contains any of the streets
daily_traffic_modes <- daily_traffic_modes %>%
  filter(sapply(streets, function(street) grepl(street, location, ignore.case = TRUE)) %>% any())


daily_traffic_modes <- daily_traffic_modes %>%
  filter(between(as.Date(count_date), as.Date("2015-01-01"), as.Date("2019-12-31")))


daily_traffic_modes <- daily_traffic_modes %>%
  filter(
    !str_detect(location, "BATHURST|BREMNER BLVD|FORT YORK BLVD|SPADINA AVE") &
      !str_detect(location, "DUNDAS ST|ANNETTE ST") &
      !str_detect(location, "SIGNET DR|CARPENTER RD|GIHON SPRING DR") &
      !str_detect(location, "BLACK CREEK BLVD|HARLOCK BLVD|NELSON RD")
  )

daily_traffic_modes <- daily_traffic_modes[!grepl("ST CLAIR AVE W AT KEELE ST & WESTON RD (PX 493)|DUFFERIN ST AT STEELES AVE W|
                                                  BLOOR ST W AT JANE ST (PX 333)|BLOOR ST W AT KEELE ST & PARKSIDE DR (PX 328)|BLOOR ST W AT JANE ST (PX 333)|BLOOR ST W AT KEELE ST & PARKSIDE DR (PX 328)|
                                                  EGLINTON AVE AT JANE ST (PX 905)|BLACK CREEK DR S TCS AT JANE ST (PX 235)|BLACK CREEK DR N TCS AT JANE ST (PX 1418)|
                                                  HULLMAR DR AT JANE ST (PX 2401)|MARTIN GROVE RD AT STEELES AVE W", daily_traffic_modes$location, ignore.case = TRUE), ]

daily_traffic_modes <- daily_traffic_modes %>%
  mutate(count_date = as.Date(count_date, format = "%Y-%m-%d"))


#### Save data ####
write.csv(daily_traffic_modes, "./data/analysis_data/cleaned-data-2010-2019.csv", row.names = FALSE)



unique_locations <- unique(cleaned_data_2010_2019$location)
print(unique_locations)
  
  
  
