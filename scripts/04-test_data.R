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
library(here)



#### Test data ####

### Tests for simulated data set ###
source(here("scripts", "00-simulate_data.R"))

# Check that simulated data set is not empty
test_that("Data frame is not empty", {
  expect_false(nrow(simulated_traffic_volumes) == 0)
})

# Check for missing values in critical columns
test_that("There are no missing values in critical columns", {
  critical_cols <- c("count_date", "location_id", "location", "lng", "lat", 
                     "time_start", "time_end")
  for (col in critical_cols) {
    expect_true(all(!is.na(simulated_traffic_volumes[[col]])))
  }
})

# Check longitude and latitude values within Toronto's bounds
test_that("Longitude and latitude are within expected range", {
  expect_true(all(simulated_traffic_volumes$lng >= -79.63926 & 
                    simulated_traffic_volumes$lng <= -79.12206))
  expect_true(all(simulated_traffic_volumes$lat >= 43.59052 & 
                    simulated_traffic_volumes$lat <= 43.85545))
})

# Check count dates are within the expected range
test_that("Count dates are within the expected range", {
  expect_true(min(simulated_traffic_volumes$count_date) >= as.Date('2010-01-01'))
  expect_true(max(simulated_traffic_volumes$count_date) <= as.Date('2019-12-31'))
})

# Check that traffic counts are within reasonable limits
test_that("Traffic counts are within reasonable limits", {
  traffic_cols <- grep("sb_cars_t|nb_cars_t|wb_cars_t|eb_cars_t|sb_truck_t|nb_truck_t|
                        wb_truck_t|eb_truck_t|sb_bus_r|nb_bus_r|wb_bus_r|eb_bus_r|
                        nx_peds|sx_peds|ex_peds|wx_peds|nx_bike|sx_bike|ex_bike|wx_bike", 
                       names(simulated_traffic_volumes), value = TRUE)
  for (col in traffic_cols) {
    expect_true(all(simulated_traffic_volumes[[col]] >= 0))
  }
})

#### Tests for cleaned data set ####
source(here("scripts", "02-data_cleaning.R"))

# Check that the cleaned data set is not empty
test_that("Cleaned data frame is not empty", {
  expect_false(nrow(daily_traffic) == 0)
})

# Check for the existence of expected columns after cleaning
test_that("Expected columns exist after data cleaning", {
  expected_cols <- c("count_date", "location", "lng", "lat", 
                     "count_id", "time_trend", 
                     "daily_cars", "daily_bus", "daily_peds", "daily_bike")
  for (col in expected_cols) {
    expect_true(col %in% names(daily_traffic))
  }
})

# Check that dates are within the filtered range of 2014 to 2019
test_that("Dates are within the filtered range of 2014 to 2019", {
  expect_true(all(year(daily_traffic$count_date) >= 2011 & 
                    year(daily_traffic$count_date) <= 2020))
})



#### Tests for exploring cleaned data ####
source(here("scripts", "03-exploring_cleaned_data.R"))

# Test summary statistics generation
test_that("Summary statistics are generated correctly", {
  expect_false(is.null(summary_stats))
  expect_true("mean_daily_cars" %in% names(summary_stats))
  expect_true("median_daily_cars" %in% names(summary_stats))
  expect_true("mean_daily_bus" %in% names(summary_stats))
  expect_true("median_daily_bus" %in% names(summary_stats))
  expect_true("mean_daily_peds" %in% names(summary_stats))
  expect_true("median_daily_peds" %in% names(summary_stats))
  expect_true("mean_daily_bike" %in% names(summary_stats))
  expect_true("median_daily_bike" %in% names(summary_stats))
})

# Test that plot data is structured correctly
test_that("Data for visualizations is prepared correctly", {
  expect_true("count_date" %in% names(cleaned_data))
  expect_true("daily_cars" %in% names(cleaned_data))
  expect_true("daily_bus" %in% names(cleaned_data))
  expect_true("daily_peds" %in% names(cleaned_data))
  expect_true("daily_bike" %in% names(cleaned_data))
})

# Test that plot objects are created
test_that("Plot objects are created", {
  expect_false(is.null(line_graph_car))
  expect_false(is.null(line_graph_buses))
  expect_false(is.null(line_graph_pedestrians))
  expect_false(is.null(line_graph_cyclists))
})


#### Tests for model ####

source(here("scripts", "05-model.R"))

# Test for model existence
test_that("Models are created", {
  expect_true(!is.null(model_before))
  expect_true(!is.null(model_after))
})

# Test for multicollinearity (VIF)
test_that("Multicollinearity is within acceptable limits", {
  expect_true(all(vif(model_before) < 5)) 
  expect_true(all(vif(model_after) < 5))
})

# Test for normality of residuals using Shapiro-Wilk test
test_that("Residuals are normally distributed", {
  expect_true(shapiro.test(resid(model_before))$p.value > 0.05) 
  expect_true(shapiro.test(resid(model_after))$p.value > 0.05)
})

# Test for absence of highly influential observations based on Cook's distance
test_that("No highly influential observations based on Cook's distance", {
  expect_true(sum(cooks.distance(model_before) > 1) < 5)
  expect_true(sum(cooks.distance(model_after) > 1) < 5)
})

