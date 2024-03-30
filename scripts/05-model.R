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

## Scale the predictor variables ## 
analysis_data$daily_bus <- scale(analysis_data$daily_bus)
analysis_data$daily_peds <- scale(analysis_data$daily_peds)
analysis_data$daily_bike <- scale(analysis_data$daily_bike)


before_improvement <- subset(analysis_data, count_date < as.Date("2017-12-17"))
after_improvement <- subset(analysis_data, count_date >= as.Date("2017-12-17"))


# Check the number of observations in each subset to ensure they contain data.
cat("Observations before improvement:", nrow(before_improvement), "\n")
cat("Observations after improvement:", nrow(after_improvement), "\n")

### Model data ####


model_before <- glm.nb(daily_cars ~ daily_bus + daily_peds + daily_bike, 
                       data = before_improvement)
summary(model_before)


model_before <- glm.nb(daily_cars ~ daily_bus + daily_peds + daily_bike, 
                       data = before_improvement)
summary(model_before)

#### Save model ####
saveRDS(
  model,
  file = "model/model.rds"
)

### Checks for Model ###

library(car)
vif(model_before) 
vif(model_after)


# Plotting residuals to check for patterns
par(mfrow = c(2, 2))
plot(residuals(model_before), type = "p", main = "Residuals of Model Before Improvement")
plot(residuals(model_after), type = "p", main = "Residuals of Model After Improvement")


# Checking for normality of residuals
hist(resid(model_before), main = "Histogram of Residuals - Before")
hist(resid(model_after), main = "Histogram of Residuals - After")


# Checking for influential observations using Cook's distance
cooks.distance(model_before) # For the 'before improvement' model
cooks.distance(model_after)  # For the 'after improvement' model

# Plotting Cook's distance
plot(cooks.distance(model_before), type = "h", main = "Cook's Distance - Before")
plot(cooks.distance(model_after), type = "h", main = "Cook's Distance - After")





