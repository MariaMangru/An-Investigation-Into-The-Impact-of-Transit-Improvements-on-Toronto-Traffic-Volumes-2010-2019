#### Preamble ####
# Purpose: Simulates the data set `Traffic Volumes at Intersections for All Modes`
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


### Workspace Setup ###
library(dplyr)
library(tidyr)
library(lubridate)

set.seed(123) # for reproducibility 

### Create simulated data set ###
simulate_traffic_volumes <- function(n = 1000) {
  data <- tibble(
    X_id = 1:n,
    count_id = sample(10000:99999, n, replace = TRUE),
    count_date = sample(seq(as.Date('2010-01-01'), as.Date('2019-12-31'), by="day"), n, replace = TRUE),
    location_id = sample(1000:9999, n, replace = TRUE),
    location = paste(sample(c("ELLESMERE RD", "YORK MILLS RD", "PARKSIDE DR", "QUEEN ST", "DUFFERIN ST"), n, replace = TRUE),
                     "AT",
                     sample(c("PARKINGTON", "DON MILLS", "SPRING RD", "WEST OF", "EAST OF"), n, replace = TRUE)),
    lng = runif(n, min=-79.63926, max=-79.12206),
    lat = runif(n, min=43.59052, max=43.85545), 
    centreline_type = sample(1:2, n, replace = TRUE),
    centreline_id = sample(10000000:99999999, n, replace = TRUE),
    px = sample(2000:2022, n, replace = TRUE),
    time_start = paste(sprintf("%02d:00:00", sample(0:23, n, replace = TRUE)), "UTC"),
    time_end = paste(sprintf("%02d:00:00", sample(0:23, n, replace = TRUE)), "UTC"),
    sb_cars_t = sample(0:500, n, replace = TRUE),
    nb_cars_t = sample(0:500, n, replace = TRUE),
    wb_cars_t = sample(0:500, n, replace = TRUE),
    eb_cars_t = sample(0:500, n, replace = TRUE),
    sb_truck_t = sample(0:100, n, replace = TRUE),
    nb_truck_t = sample(0:100, n, replace = TRUE),
    wb_truck_t = sample(0:100, n, replace = TRUE),
    eb_truck_t = sample(0:100, n, replace = TRUE),
    sb_bus_r = sample(0:50, n, replace = TRUE),
    nb_bus_r = sample(0:50, n, replace = TRUE),
    wb_bus_r = sample(0:50, n, replace = TRUE),
    eb_bus_r = sample(0:50, n, replace = TRUE),
    nx_peds = sample(0:300, n, replace = TRUE),
    sx_peds = sample(0:300, n, replace = TRUE),
    ex_peds = sample(0:300, n, replace = TRUE),
    wx_peds = sample(0:300, n, replace = TRUE),
    nx_bike = sample(0:200, n, replace = TRUE),
    sx_bike = sample(0:200, n, replace = TRUE),
    ex_bike = sample(0:200, n, replace = TRUE),
    wx_bike = sample(0:200, n, replace = TRUE),
    nx_other = sample(0:10, n, replace = TRUE),
    sx_other = sample(0:10, n, replace = TRUE),
    ex_other = sample(0:10, n, replace = TRUE),
    wx_other = sample(0:10, n, replace = TRUE)
  )
  
  ## Create time intervals ##
  data <- data %>%
    mutate(time_start = as.POSIXct(time_start, format="%H:%M:%S", tz="UTC"),
           time_end = time_start + minutes(15)) %>%
    mutate(time_start = format(time_start, "%Y-%m-%dT%H:%M:%S"),
           time_end = format(time_end, "%Y-%m-%dT%H:%M:%S"))
  
  return(data)
}

## Simulate the data set ##
simulated_traffic_volumes <- simulate_traffic_volumes(n = 1000)

## View the first few rows of the simulated data ##
head(simulated_traffic_volumes)





