#### Preamble ####
# Purpose: Creates negative binomial model 
# Author: Maria Mangru
# Date: 18th April, 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(MASS) 
library(dplyr)
library(readr)
library(car)
library(ggplot2)
library(here)



# Read and prepare data
analysis_data <- read.csv(here("data", "analysis_data", "cleaned-data.csv"))

# Scaling predictor variables for analysis
analysis_data <- analysis_data %>%
  mutate(
    daily_bus = scale(daily_bus),
    daily_peds = scale(daily_peds),
    daily_bike = scale(daily_bike)
  )


# Splitting the dataset into before and after a specific date for comparison
start_before <- as.Date("2011-12-31")
end_before <- as.Date("2014-12-31")
start_after <- as.Date("2018-11-01")
end_after <- as.Date("2020-02-29")

before_improvement <- subset(analysis_data, count_date >= start_before & count_date <= end_before)
after_improvement <- subset(analysis_data, count_date >= start_after & count_date <= end_after)


# Verifying the data split
cat("Observations before improvement:", nrow(before_improvement), "\n")
cat("Observations after improvement:", nrow(after_improvement), "\n")


### Model data ####

# Building negative binomial regression models for before and after improvement
model_before <- glm.nb(daily_cars ~ daily_bus + daily_peds + daily_bike + time_trend, data = before_improvement)
summary(model_before)

model_after <- glm.nb(daily_cars ~ daily_bus + daily_peds + daily_bike + time_trend, data = after_improvement)
summary(model_after)



# Graphical representation of the model results for the 'before' and `after` improvement model

# For model_before
before_fitted <- data.frame(observed = before_improvement$daily_cars, fitted = fitted(model_before))
ggplot(before_fitted, aes(x = fitted, y = observed)) +
  geom_point() +
  geom_line(aes(x = fitted, y = fitted), color = "red") +
  labs(title = "Observed vs. Fitted Values - Before Improvement",
       x = "Fitted Values",
       y = "Observed Values") +
  theme_minimal()
before_fitted

# For model_after
after_fitted <- data.frame(observed = after_improvement$daily_cars, fitted = fitted(model_after))
ggplot(after_fitted, aes(x = fitted, y = observed)) +
  geom_point() +
  geom_line(aes(x = fitted, y = fitted), color = "red") +
  labs(title = "Observed vs. Fitted Values - After Improvement",
       x = "Fitted Values",
       y = "Observed Values") +
  theme_minimal()
after_fitted


#### Save model ####
models_dir <- here("models")
saveRDS(model_before, file = file.path(models_dir, "model_before.rds"))
saveRDS(model_after, file = file.path(models_dir, "model_after.rds"))



### Checks for Model ###

# Checking for multicollinearity using VIF
vif(model_before) 
vif(model_after)

# Visualizing residuals to identify patterns
par(mfrow = c(2, 2))
plot(residuals(model_before), type = "p", main = "Residuals of Model Before Improvement")
plot(residuals(model_after), type = "p", main = "Residuals of Model After Improvement")

# Examining the normality of residuals
hist(resid(model_before), main = "Histogram of Residuals - Before")
hist(resid(model_after), main = "Histogram of Residuals - After")

# Investigating influential observations with Cook's distance
cooks.distance(model_before)
cooks.distance(model_after)  

# Plotting Cook's distance
plot(cooks.distance(model_before), type = "h", main = "Cook's Distance - Before")
plot(cooks.distance(model_after), type = "h", main = "Cook's Distance - After")


