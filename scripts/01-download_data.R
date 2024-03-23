#### Preamble ####
# Purpose: Downloads and saves the data from opendatatoronto
# Author: Maria Mangru
# Date: March 2024
# Contact:maria.mangru@mail.utoronto.ca
# License: MIT


#### Workspace setup ####
library(opendatatoronto)
library(dplyr)
library(readr)


#### Download data ####

# get package
package <- show_package("traffic-volumes-at-intersections-for-all-modes")
package

# get all resources for this package
resources <- list_package_resources("traffic-volumes-at-intersections-for-all-modes")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==7) %>% get_resource()
data


#### Save data ####
write_csv(data, "data/raw_data/raw-data-2010-2019.csv") 

         
