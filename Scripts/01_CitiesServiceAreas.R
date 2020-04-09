## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Identify Cities within PEPFAR Countries

## Libraries
library(tidyverse)
library(readxl)
library(sf)

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


# Update country name, and merge to Pepfar pnsu data
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

# cities <- cities %>% 
#   right_join(target_psnu, by=c("country.etc" = "countryname")) %>% 
#   filter(!is.na(operatingunit)) %>% 
#   select(7,'country'=2,8:9,1,3:6)

# Verify differences
cities %>% distinct(country) %>% 
  pull() %>% 
  setdiff(cntries)

cities %>% 
  filter(is.na(lat) | is.null(lat) | is.na(long) | is.null(long))

cities %>% 
  ggplot(aes(long, lat)) +
  geom_point() +
  coord_sf() +
  theme_minimal()

# Export Target Cities
#cities %>% write.csv(file = "./Data/Cities_in_PEPFAR_Countries.csv", row.names = FALSE, na = "")



cities %>% 
  filter(country == 'South Sudan')
# Create point feature class



