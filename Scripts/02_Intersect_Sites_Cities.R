## PROJECT:  Find your Beach
## AUTHOR:   T.Essam, A. Chaftez, B. Kagniniwa
## LICENSE:  MIT
## PURPOSE:  Buffer and extract cities / facility count
## DATE:     2020-04-09
## UPDATED:  2020-04-10
## NOTES:    Streamlined processing into a single file that now does geoprocessing in single place


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

  crs <- 3857
  buffer_size <- 5000 #meter based buffer (5 Km)
  


# IMPORT, MUNGE AND BUFFER --------------------------------------------------------

  sites <- vroom(file.path(data_in, "SBU_PEPFAR_USAID_Site_Coordinates_v2_SBU.csv"))
  
  # Adding lat / lon duplicate columns so SF will keep a set with info when buffering
  # Buffer set above in globals and executed below. Renaming country variable
  # and aligning country names that are different between the two datasets.
  data(world.cities)
  cities <- 
    world.cities %>% 
    mutate(latitude = lat, 
           longitude = long) %>% 
    rename(country = country.etc) %>% 
    mutate(
         country = case_when(
         country == "Myanmar" ~ "Burma",
         country ==  "Ivory Coast" ~ "Cote d'Ivoire",
         country ==  "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
         country ==  "Swaziland" ~ "Eswatini",
         country ==  "Sudan" ~ "South Sudan", # No 'S. Sudan' in World Cities
         country ==  "Saint Kitts and Nevis" ~ "Saint Kitts & Nevis",
         country ==  "Antigua and Barbuda" ~ "Antigua & Barbuda",
         country ==  "Trinidad and Tobago" ~ "Trinidad & Tobago",
         country ==  "Saint Vincent and The Grenadines" ~ "Saint Vincent & the Grenadines",
      TRUE ~ country)
    )

    
  cities_buffered <- 
    st_as_sf(cities, coords = c("long", "lat"), crs = 4326) %>%
    st_transform(., crs = st_crs(crs)) %>% 
    st_buffer(buffer_size)


  st_crs(cities_buffered)$units

  # cities_buffered <- 
  #   st_read(file.path(gis, "Cities_in_EPSG3857_5km_buffer.shp"), stringsAsFactors = FALSE)
  # 
  # st_crs(cities_buffered)
  
  # Load PEPFAR sites and transform CRS so we can perform a spatial join with buffered city data
  sites_geo <-  
    st_as_sf(sites %>% filter(!is.na(longitude)), coords = c("longitude", "latitude"), crs = 4326) %>% 
    st_transform(., crs = st_crs(crs))
  
  st_crs(sites_geo) 
  
  # Checking count of sites by country
  sites_geo %>% st_drop_geometry() %>% count(countryname, sort = TRUE) %>% print(n = Inf)
  

# INTERSECT, EXTRACT, and PLOT TO VERRIFY  --------------------------------------------
  
  sites_to_cities <- st_join(sites_geo, cities_buffered, left = TRUE)
  
  
  # Not necessarily unique due to some city names repeated within a country
  check <- unique(sites_to_cities$name) 
  
  
  # Return lat and long of each city
  final_list <- 
    sites_to_cities %>% 
    st_drop_geometry() %>% 
    group_by(latitude, longitude, name, countryname, pop, capital) %>% 
    count() %>% 
    select(-n) %>% 
    arrange(countryname, name) %>% 
    filter(!is.na(name)) %>% 
    ungroup() %>% 
    mutate(lrg_city = pop > 40000)
  
  
  # Spot check a country with function
  verify_plot <- function(country) {
  
    cntry <- country
    check <- unique(sites_to_cities$name) #Not perfect, as some cities have mulitple entries (See Honduras)
    count <- sites_to_cities %>%
      filter(countryname == cntry) %>% 
      st_drop_geometry() %>% 
      distinct(name, pop) %>% 
      filter(!is.na(name)) %>% 
      add_tally() %>% 
      pull(n)
  
    # Pull in boundary from R natural earth, works for nearly every ou (Burma)
    boundary <- ne_countries(country = cntry, scale = "medium", returnclass = "sf") %>% 
      st_transform(st_crs(3857)) %>% 
      select(name, iso2 = iso_a2, iso3 = iso_a3, pop = pop_est)
    
    # Checking the original city data and matching to names (not perfect).
    # Want to see of the matches are overlapping w/ buffer. Some cities
    # are repeated due to multiple cities in a country with same name 
    ggplot(cities_buffered %>% filter(name %in% check & country == cntry)) + 
      geom_sf(data = boundary, colour = "#909090", alpha = 0.75) +
      geom_sf(colour = "#ffba77", fill = "#ffba77", size = 2.5) +
      geom_sf(data = sites_geo %>% filter(countryname == cntry), 
              size = 3, colour = "#2c7fb8", alpha = 0.6) +
      theme_minimal() +
      labs(title = paste(cntry, "has", count, "cities with PEPFAR facilities."))
  }

  # Listing of OUs to verify matches over with spot checks
  unique(sites_to_cities$countryname) 
 
 verify_plot(country = "Honduras")
 verify_plot(country = "Zambia")


# EXPORT ------------------------------------------------------------------

  write_csv(final_list, file.path(data_out, "USAID_PEPFAR_city_list.csv"))

