## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Identify Cities within PEPFAR Countries

# Libraries
library(tidyverse)
library(readxl)
library(sf)

# required data
source("./Scripts/01_CitiesServiceAreas.R")

# PEPFAR Facilities
sites <- read_csv("./Data/SBU_PEPFAR_USAID_Site_Coordinates_v2_SBU.csv")

sites %>% head()
sites %>% glimpse()

sites %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  distinct(countryname)

sites %>% 
  filter(is.na(latitude) | is.na(longitude))

sites <- sites %>% 
  filter(!is.na(latitude) & !is.na(longitude))

sites %>% 
  filter(countryname == 'South Sudan')
  

#write.csv(sites, file = "./Data/SBU_PEPFAR_USAID_Site_with_valid_Coordinates.csv", row.names = FALSE, na = "")

# cities %>% 
#   full_join(sites, by=c("country" = "countryname"))

CitiesAreas <- sf::st_read("./GIS/Cities_in_EPSG3857_5km_buffer.shp")

CitiesAreas

sitesGeo <- sf::st_read("./GIS/PEPFAR_sites.shp")

sitesGeo <- sitesGeo %>% 
  st_transform(crs = st_crs(CitiesAreas))


# Joint Cites attr to sites data
sitesData <- sitesGeo %>% 
  st_join(CitiesAreas, left = TRUE)

sitesData %>% str()
sitesData %>% glimpse()

sitesData2 <- sitesData %>% 
  st_set_geometry(NULL)

#sitesData2 %>% View()
sitesData2 %>% glimpse()

# Report
sitesData2 %>% 
  filter(!is.na(country) & !is.na(name)) %>% 
  group_by(country, name) %>% 
  tally()

# Export results
sitesData2 %>% 
  filter(!is.na(country) & !is.na(name)) %>% 
  group_by(country, name) %>% 
  tally() %>% 
  write.csv(file = "./Data/Cities_with_sites_in_5km_radius.csv", row.names = FALSE, na = "")

# Test results by country
sitesData2 %>% 
  filter(!is.na(country) & !is.na(name)) %>% 
  filter(country == 'Kenya') %>% 
  group_by(country) %>% 
  distinct(name) %>% tally()
  tally() 
