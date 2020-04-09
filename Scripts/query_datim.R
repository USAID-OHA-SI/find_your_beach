## PROJECT:  find your beach
## AUTHOR:   A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  pull coordinates for USAID sites
## NOTE:     drawing heavily from USAID-OHA-SI/right_size/pull_datim
## DATE:     2020-04-09


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(Wavelength)
library(lubridate)
library(fs)


# GLOBAL VARIABLES --------------------------------------------------------

myuser <- ""


# DATIM API FUNCTION ------------------------------------------------------

  query_datim <- function(ou_uid, org_lvl, type, username, password, baseurl = "https://final.datim.org/"){
    
    print(paste("running ", ou_uid, " ... ", Sys.time()))
    
    core_url <-
      paste0(baseurl,"api/29/analytics?",
             "dimension=ou:LEVEL-", org_lvl, ";", ou_uid, "&", #level and ou
             "dimension=bw8KHXzxd9i:NLV6dy7BE2O&" #Funding Agency -> USAID
             )
    
    if(type == "TX"){
      type_url <- 
        paste0("dimension=pe:2019Oct&", #period
               "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets/Results - Targets W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:MvszPTQrUhy&", #technical area, TX_CURR
               "filter=RUkVjD3BsS1:PE5QVF0w4xj&" #Top Level  - Numerator
               )
    } else if (type == "HTS"){
      type_url <- 
        paste0("dimension=pe:2019Oct&", #period
               "dimension=IeMmjHyBUpi:W8imnja2Owd&", #Targets/Results - Targets W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:f5IPTM7mieH;wdoUps1qb3V;BTIqHnjeG7l;rI3JlpiuwEK;CUblPgOMGaT&&", #technical area,  HTS_TST, HTS_INDEX, PMTCT_STAT, TB_STAT, VMMC_CIRC
               "filter=bDWsPYyXgWP:mSBg9AZx1lV;viYXyEy7wKi;awSDzziN3Dn;EvyNJHbQ7ZE&", #HIV Test Status (Specific)
               "filter=Jm6OwL9IqEa:FVCb4OUYkiG;NcerbGnPdIc;oQmZ3urcBbP;xmYVJTekCtl;NwSG4Qshiu1;H6bOQCcwrBV;INrbTYREjOX;d7vYERohPfS;lRAOEGldPtj;sZWARAOoyvZ;qdFQQc5dCbH;OAwcN9zCmLM;t7WRCjHNaEU;K4CBGfivWfg;ML7FROoRi6r;v5AuS5Aw9Ks&" #HTS Modality (USE ONLY for FY19 Results/FY20 Targets)
        )
    } else if(type == "LAB"){
      type_url <- 
        paste0("dimension=pe:2018Oct&", #period
               "dimension=IeMmjHyBUpi:Jh0jDM5yQ2E&", #Targets/Results - Results W8imnja2Owd,Jh0jDM5yQ2E
               "dimension=LxhLO68FcXm:scxfIjoA6nt&" #technical area, LAB_PTCQI
        )
      
    }
    
    end_url <- "displayProperty=SHORTNAME&skipMeta=false&hierarchyMeta=true"
    
    full_url <- paste0(core_url, type_url, end_url)
    
    df <- get_datim_targets(full_url, username, password)
  
  return(df)
}




# IDENTIFY INPUTS FOR API -------------------------------------------------

  #identify site level in each ou
    df_lvls <- identify_levels(username = myuser, password = mypwd(myuser))
  
  #pull orgunit uids
    df_uids <- identify_ouuids(username = myuser, password = mypwd(myuser))
  
  #table for API use
    ctry_list <- left_join(df_lvls, df_uids, by = c("country_name" = "displayName")) %>% 
      select(operatingunit = name3, operatingunituid = id, countryname = country_name, 
             psnu_lvl = prioritization, site_lvl = facility)
    
    rm(df_lvls, df_uids)


# PULL DATA ---------------------------------------------------------------


  #run API across all countries
    df_tx <- map2_dfr(.x = ctry_list$operatingunituid, 
                      .y = ctry_list$site_lvl, 
                      .f = ~ query_datim(.x, .y, "TX", myuser, mypwd(myuser)))
      
    df_hts <- map2_dfr(.x = ctry_list$operatingunituid, 
                      .y = ctry_list$site_lvl, 
                      .f = ~ query_datim(.x, .y, "HTS", myuser, mypwd(myuser)))
    
      
    df_lab <- map2_dfr(.x = ctry_list$operatingunituid, 
                       .y = ctry_list$site_lvl, 
                       .f = ~ query_datim(.x, .y, "LAB", myuser, mypwd(myuser)))
    
  

# PULL COORDINATES --------------------------------------------------------

  #pull hierarchy
    df_orgs <- purrr::map_dfr(.x = ctry_list$operatingunituid,
                              .f = ~ pull_hierarchy(.x, myuser, mypwd(myuser))) 
    

# MUNGE AND APPEND DATA ---------------------------------------------------

  #combine all technical areas into HTS (modalities are derived from VMMC, TB, PMTCT, INDEX)
    df_hts <- df_hts %>% 
      mutate(`Technical Area` = "HTS_TST") %>% 
      group_by_if(is.character) %>% 
      summarise_if(is.double, sum, na.rm = TRUE) %>% 
      ungroup()
    
  #append data together
    df_full <- bind_rows(df_tx, df_hts, df_lab)

  #limit output variables
    df_sel <- df_full %>% 
      select(fundingagency = `Funding Agency`,
             resulttarget = `Targets / Results`,
             indicator = `Technical Area`,
             period = Period,
             orgunituid, 
             value = Value)
    
  #merge with hierarchy/coordinates
    df_sel <- left_join(df_sel, df_orgs)
  
  #reshape for mapping
    df_out <- df_sel %>% 
      select(indicator, countryname, orgunituid, latitude, longitude) %>% 
      mutate(exists = "X") %>% 
      spread(indicator, exists)


# EXPORT ------------------------------------------------------------------

  write_csv(df_out, "Dataout/SBU_PEPFAR_USAID_Site_Coordinates_SBU.csv", na = "")    
    