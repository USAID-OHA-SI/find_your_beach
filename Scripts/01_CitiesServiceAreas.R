## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Identify Cities within PEPFAR Countries and Generage their Service Areas

## Libraries
library(tidyverse)
library(readxl)
library(sf)
library(rnaturalearth)
library(RColorBrewer)

# required data
source("./Scripts/01_PEPFAR_OperatingUnits.R")

# Read Citis location data
cities_loc <- read_csv("./Data/world_cities.csv")

#cities_loc %>% str()
cities_loc %>% glimpse()

cities_loc %>% 
  distinct(country.etc) %>% 
  filter(country.etc %in% cntries) 

mCntries <- cntries[!cntries %in% c(cities_loc %>% distinct(country.etc) %>% pull())]
mCntries
# [1] "Burma"                            "Cote d'Ivoire"                   
# [3] "Democratic Republic of the Congo" "Eswatini"                        
# [5] "South Sudan"                      "Saint Kitts & Nevis"             
# [7] "Antigua & Barbuda"                "Trinidad & Tobago"               
# [9] "Saint Vincent & the Grenadines"  

#cities_loc %>% View()

# Non State Dept. Names
mCntries2 <- c("Myanmar",
               "Ivory Coast",
               "Congo Democratic Republic",
               "Swaziland",
               "Sudan",
               "Saint Kitts and Nevis",
               "Antigua and Barbuda",
               "Trinidad and Tobago",
               "Saint Vincent and The Grenadines")


# Update country name, and columns
cities <- cities_loc %>% 
  mutate(
    country.etc = case_when(
      country.etc == "Myanmar" ~ "Burma",
      country.etc ==  "Ivory Coast" ~ "Cote d'Ivoire",
      country.etc ==  "Congo Democratic Republic" ~ "Democratic Republic of the Congo",
      country.etc ==  "Swaziland" ~ "Eswatini",
      country.etc ==  "Sudan" ~ "South Sudan", # No 'S. Sudan' in World Cities
      country.etc ==  "Saint Kitts and Nevis" ~ "Saint Kitts & Nevis",
      country.etc ==  "Antigua and Barbuda" ~ "Antigua & Barbuda",
      country.etc ==  "Trinidad and Tobago" ~ "Trinidad & Tobago",
      country.etc ==  "Saint Vincent and The Grenadines" ~ "Saint Vincent & the Grenadines",
      TRUE ~ country.etc
    )
  ) 


cities <- cities %>% 
  filter(country.etc %in% cntries) %>% 
  select("country"=2, 1, 3:6)



# Verify differences
cities %>% distinct(country) %>% 
  pull() %>% 
  setdiff(cntries)


cities %>% 
  filter(is.na(lat) | is.null(lat) | is.na(long) | is.null(long))


# Export Target Cities
#cities %>% write.csv(file = "./Data/Cities_in_PEPFAR_Countries.csv", row.names = FALSE, na = "")


# Test: Create City Service Areas feature class for x country

# Country Boundaries
geoKenya <- ne_countries(country = 'Kenya', scale = "medium", returnclass = "sf") %>% 
  st_transform(st_crs(3857)) %>% 
  select(name, iso2 = iso_a2, iso3 = iso_a3, pop = pop_est)

glimpse(geoKenya)

# Create 5km Buffers
geoKenyaCities <- cities %>% 
  filter(country == 'Kenya') %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>% 
  st_transform(crs = st_crs(3857)) %>% 
  st_buffer(5000) 

glimpse(geoKenyaCities)

# Plot the Service Areas
geoKenya %>% 
  ggplot() +
  geom_sf(fill = NA) +
  geom_sf(data=geoKenyaCities, aes(fill = pop), color = 'white', lwd = .25) +
  scale_fill_viridis_b() +
  coord_sf() + 
  labs(title = "Kenya - 5km Service Area of main Cities") +
  theme_minimal() 

# Zoom into an area

box <- st_sfc(st_point(c(34, -2)), st_point(c(38, 2)), crs = 4326) %>% 
  st_transform(crs = st_crs(3857)) %>% 
  st_coordinates()

geoKenya %>% 
  ggplot() +
  geom_sf(fill = NA) +
  geom_sf(data = geoKenyaCities, aes(fill = pop), color = 'white', lwd = .25) +
  scale_fill_viridis_b() +
  coord_sf(xlim = box[, 'X'], ylim = box[, 'Y'], datum = st_crs(3857)) + 
  labs(title = "Kenya - 5km Service Area of main Cities",
       subtitle = "BBOX: [34, 38], [-2, 2]") +
  theme_minimal() 
  






