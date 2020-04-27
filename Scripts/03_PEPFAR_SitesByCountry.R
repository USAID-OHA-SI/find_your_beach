## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Summarize PERFAR Sites by USAID Bureau / Country


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)


# GLOBAL VARIABLES --------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"


# IMPORT DATA -------------------------------------------------------------

sites <- vroom(file.path(data_in, "SBU_PEPFAR_USAID_Site_Coordinates_v2_SBU.csv"))

sites %>% 
  glimpse()

# MUNGE -------------------------------------------------------------------

  # Summary: # of site per country
  sites %>% 
    group_by(countryname, iso) %>% 
    tally()

  # Summary: # of sites by service & country
  sites_in_country <- sites %>% 
    group_by(countryname, iso) %>% 
    summarise(
      site_all = n(),
      Site_HTS_TST = sum(HTS_TST == 'X', na.rm = T),
      Site_LAB_PTCQI = sum(LAB_PTCQI == 'X', na.rm = T),
      Site_TX_CURR = sum(TX_CURR == 'X', na.rm = T)
    )

  sites_in_country %>% 
    head()
  
  # Export
  sites_in_country %>% 
    write_csv(file.path(data_out, "USAID_PEPFAR_Sites_Summary.csv"), na = "")

  # Geo: Valid sites with available services
  sites_geo <- sites %>% 
    select(-3) %>% 
    filter(!is.na(latitude) | !is.na(longitude)) %>% 
    mutate(
      latitude = round(latitude, 1),
      longitude = round(longitude, 1),
      HTS_TST = ifelse(is.na(HTS_TST), 'No', 'Yes'),
      LAB_PTCQI = ifelse(is.na(LAB_PTCQI), 'No', 'Yes'),
      TX_CURR = ifelse(is.na(TX_CURR), 'No', 'Yes')
    ) 

    sites_geo %>% 
      head()
    
    # Export
    sites_geo %>% 
      write_csv(file.path(data_out, "USAID_PEPFAR_Sites_locations.csv"), na = "")

