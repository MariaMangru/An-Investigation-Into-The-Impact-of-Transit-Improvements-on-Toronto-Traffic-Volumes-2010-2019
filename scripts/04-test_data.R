#### Preamble ####
# Purpose: Tests simulate data, data cleaning and exploring cleaned data scripts
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
suppressPackageStartupMessages(library(stringr))
update.packages(stringr)


#### Test data ####

### Tests for simulated data set ###
source("00-simulate_dataset.R")

## 1: Check that simulated data set is not empty
test_that("Data frame is not empty", {
  expect_false(nrow(simulated_traffic_volumes) == 0)
})

## 2: Check for missing values in columns
test_that("There are no missing values in critical columns", {
  critical_cols <- c("count_date", "location_id", "location", "lng", "lat", "time_start", "time_end")
  for (col in critical_cols) {
    expect_true(all(!is.na(simulated_traffic_volumes[[col]])))
  }
})

## 3: Check longitude and latitude values to ensure they're within Toronto's bounds
test_that("Longitude and latitude are within expected range", {
  expect_true(all(simulated_traffic_volumes$lng >= -79.63926 & simulated_traffic_volumes$lng <= -79.12206))
  expect_true(all(simulated_traffic_volumes$lat >= 43.59052 & simulated_traffic_volumes$lat <= 43.85545))
})

## 4: Check data range to ensure they're reasonable 
test_that("Count dates are within the expected range", {
  expect_true(min(simulated_traffic_volumes$count_date) >= as.Date('2010-01-01'))
  expect_true(max(simulated_traffic_volumes$count_date) <= as.Date('2019-12-31'))
})

## 5: Check that traffic counts are reasonable
test_that("Traffic counts are within reasonable limits", {
  traffic_cols <- grep("sb_cars_t|nb_cars_t|wb_cars_t|eb_cars_t|sb_truck_t|nb_truck_t|wb_truck_t|eb_truck_t|sb_bus_r|nb_bus_r|wb_bus_r|eb_bus_r|nx_peds|sx_peds|ex_peds|wx_peds|nx_bike|sx_bike|ex_bike|wx_bike", names(simulated_traffic_volumes), value = TRUE)
  for (col in traffic_cols) {
    expect_true(all(simulated_traffic_volumes[[col]] >= 0))
  }
})


### Tests for cleaned data ###
source("02-data_cleaning.R")

## 1: Check that the data set contains the right columns
test_that("Dataset contains the correct columns after cleaning", {
  expected_columns <- c("count_date", "location_id", "location", 
                        "daily_sb_cars_t", "daily_nb_cars_t", "daily_wb_cars_t", "daily_eb_cars_t",
                        "daily_sb_bus", "daily_nb_bus", "daily_wb_bus", "daily_eb_bus",
                        "daily_nx_peds", "daily_sx_peds", "daily_ex_peds", "daily_wx_peds",
                        "daily_nx_bike", "daily_sx_bike", "daily_ex_bike", "daily_wx_bike")
  expect_equal(names(daily_traffic_modes), expected_columns)
})

## 2: Check that the date format is correct (YYYY-MM-DD)
test_that("Date format is correct", {
  expect_true(all(format(daily_traffic_modes$count_date, "%Y-%m-%d") == as.character(daily_traffic_modes$count_date)))
})

## 3: Check that no duplicate entries exist for a single date and location
test_that("Data is aggregated correctly without duplicates for a single date and location", {
  aggregated_check <- daily_traffic_modes %>%
    group_by(count_date, location_id) %>%
    summarise(entries = n()) %>%
    ungroup()
  
  expect_true(all(aggregated_check$entries == 1))
})


### Tests for traffic volumes ###
source("03-exploring_cleaned_data.R", local = TRUE)

## 1: Check if mean and median daily traffic are properly calculated for each mode 

test_that("Mean and median daily traffic calculations are correct for all modes", {
  modes <- c("sb_cars_t", "nb_cars_t", "wb_cars_t", "eb_cars_t",
             "sb_bus", "nb_bus", "wb_bus", "eb_bus",
             "nx_peds", "sx_peds", "ex_peds", "wx_peds",
             "nx_bike", "sx_bike", "ex_bike", "wx_bike")
  
  for (mode in modes) {
    daily_col <- paste0("daily_", mode)
    mean_val <- mean(cleaned_data[[daily_col]], na.rm = TRUE)
    median_val <- median(cleaned_data[[daily_col]], na.rm = TRUE)
    expect_true(mean_val > 0, info = paste0("Mean is non-positive for ", mode))
    expect_true(median_val > 0, info = paste0("Median is non-positive for ", mode))
  }
})

## 2: Check that line graphs are generated without error for each mode 

test_that("Graph generation does not produce errors", {
  expect_silent({
    modes <- c("car", "bus", "pedestrian", "cyclist")
    for (mode in modes) {
      graph_name <- paste0("line_graph_", mode, "s")
      expect_is(get(graph_name), "gg", info = paste0("Graph object for ", mode, " is not a ggplot"))
    }
  })
})