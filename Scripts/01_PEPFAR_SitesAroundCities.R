## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Identify PERFAR Sites within Cities Service Areas

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

# Explore content

# Countries servicing health facilities
sites %>% 
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  distinct(countryname)

# Invalid sites
sites %>% 
  filter(is.na(latitude) | is.na(longitude)) %>% 
  tally()

# Valid sites
sites <- sites %>% 
  filter(!is.na(latitude) & !is.na(longitude)) 

# Num of site by country
sites %>% 
  group_by(countryname) %>% 
  tally() %>% 
  arrange(desc(n))
  
# Export valid sites to csv file. 
#write.csv(sites, file = "./Data/SBU_PEPFAR_USAID_Site_with_valid_Coordinates.csv", row.names = FALSE, na = "")


# Read Service Areas shapefile
CitiesAreas <- sf::st_read("./GIS/Cities_in_EPSG3857_5km_buffer.shp")

st_crs(CitiesAreas)

# Read Site shapefile. Content is from the sites csv file above
sitesGeo <- sf::st_read("./GIS/PEPFAR_sites.shp")

st_crs(sitesGeo)

# Transform site to match Service Areas's dataset
sitesGeo <- sitesGeo %>% 
  st_transform(crs = st_crs(CitiesAreas))


# Joint Cites attr to sites data
sitesData <- sitesGeo %>% 
  st_join(CitiesAreas, left = TRUE)

sitesData %>% str()
sitesData %>% glimpse()

# Extract attributes only
sitesData2 <- sitesData %>% 
  st_set_geometry(NULL)

#sitesData2 %>% View()
sitesData2 %>% glimpse()

# Report > # of sites by country/city
sitesData2 %>% 
  filter(!is.na(country) & !is.na(name)) %>% 
  group_by(country, name) %>% 
  tally() %>% 
  arrange(country, name, desc(n))

# Export results
# sitesData2 %>% 
#   filter(!is.na(country) & !is.na(name)) %>% 
#   group_by(country, name, pop) %>% 
#   tally() %>% 
#   write.csv(file = "./Data/Cities_with_sites_in_5km_radius.csv", row.names = FALSE, na = "")

# Test results by country
sitesData2 %>% 
  filter(!is.na(country) & !is.na(name)) %>% 
  filter(country == 'Kenya') %>% 
  group_by(country, name, pop) %>% 
  tally() 
