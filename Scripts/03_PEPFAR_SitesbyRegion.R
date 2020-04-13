## PROJECT:  find your beach
## AUTHOR:   A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  Identify PERFAR Sites by USAID bureau


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(vroom)


# GLOBAL VARIABLES --------------------------------------------------------

  data_in <- "Data"
  data_out <- "Dataout"


# IMPORT DATA -------------------------------------------------------------

  sites <- vroom(file.path(data_in, "SBU_PEPFAR_USAID_Site_Coordinates_v2_SBU.csv"))
  regions <- vroom(file.path(data_in, "usaid_bureau_map.csv"))


# MUNGE -------------------------------------------------------------------

  #merge together
    site_w_bureaus <- sites %>% 
      left_join(regions, by = c("countryname" = "mission")) %>% 
      mutate(bureau = case_when(countryname == "Togo" ~ "Africa",
                                countryname == "Papua New Guinea" ~ "Asia",
                                TRUE ~ bureau)) 
    
  #export
    site_w_bureaus %>% 
      count(bureau, countryname, name = "site_count") %>% 
      arrange(bureau, countryname) %>% 
      write_csv(file.path(data_out, "USAID_PEPFAR_bureau_site_count.csv"), na = "")
    
    site_w_bureaus %>% 
      count(bureau, name = "site_count") %>% 
      arrange(bureau)
