## PROJECT:  Find your Beach
## AUTHOR:   T.Essam, A. Chaftez, B. Kagniniwa
## LICENSE:  MIT
## PURPOSE:  Buffer and extract cities / facility count
## DATE:     2020-04-09
## UPDATED:  


# LIBRARIES ----------------------------------------------------------------

library(maps) # extracting world cities from here
library(tidyverse)
library(sf)
library(vroom)
library(rnaturalearth)
library(mapview)

# GLOBALS -----------------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"
  gis <- "GIS"


# IMPORT, MUNGE AND BUFFER --------------------------------------------------------


  sites <- vroom(file.path(data_in, "SBU_PEPFAR_USAID_Site_Coordinates_v2_SBU.csv"))
  
  cities_buffered <- 
    st_read(file.path(gis, "Cities_in_EPSG3857_5km_buffer.shp"), stringsAsFactors = FALSE)
  
  st_crs(cities_buffered)
  
  # Load PEPFAR sites and transform CRS so we can perform a spatial join with buffered city data
  sites_geo <-  
    st_as_sf(sites %>% filter(!is.na(longitude)), coords = c("longitude", "latitude"), crs = 4326) %>% 
    st_transform(., crs = st_crs(3857))
  
  st_crs(sites_geo) 
  
  # Checking count of sites by country
  sites_geo %>% st_drop_geometry() %>% count(countryname, sort = TRUE) %>% print(n = Inf)
  

# INTERSECT, EXTRACT, and PLOT --------------------------------------------
  
  sites_to_cities <- st_join(sites_geo, cities_buffered, left = TRUE)
  check <- unique(sites_to_cities$name)
  
  
  # Return lat and long of each city
  final_list <- 
    sites_to_cities %>% 
    st_drop_geometry() %>% 
    group_by(lat, long, name, countryname, pop) %>% count() %>% 
    select(-n) %>% arrange(countryname, name) %>% 
    filter(!is.na(name)) %>% 
    ungroup() %>% 
    mutate(lrg_city = pop > 40000)
  
  write_csv(final_list, file.path(data_out, "USAID_PEPFAR_city_list.csv"))
  
  # Spot check a country
  cntry <- c("Zambia")
  ggplot(cities_buffered %>% filter(name %in% check & country == cntry)) + 
    geom_sf(size = 2.5) +
    geom_sf(data = sites_geo %>% filter(countryname == cntry), size = 2, colour = "#2c7fb8") +
    theme_minimal()


  
