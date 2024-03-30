#### Preamble ####
# Purpose: Creates negative binomial model 
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(MASS) 
library(dplyr)
library(readr)


## NEEDS A LOT OF WORK

#### Read data ####
analysis_data <- read_csv("data/analysis_data/cleaned-data-2010-2019.csv")

### Model data ####
traffic_model_nb <- glm.nb(
  formula = daily_sb_cars_t ~ daily_sb_bus + daily_wb_bus,
  data = analysis_data,
  link = log
)

summary(traffic_model_nb)

#### Save model ####
saveRDS(
  traffic_model_nb,
  file = "model/traffic_model_nb.rds"
)