## PROJECT:  find your beach
## AUTHOR:   A.Chafetz | USAID
## LICENSE:  MIT
## PURPOSE:  summarize + visualize facility 
## NOTE:     setup visuals in R
## DATE:     2020-05-04
## UPDATED:  


# LIBRARIES ---------------------------------------------------------------

library(tidyverse)
library(here)
library(scales)
library(extrafont)
library(readxl)
library(glitr)
library(rnaturalearth)
library(sf)


# GLOBALS -----------------------------------------------------------------

  #folder locations
    data_in <- "Data"
    data_out <- "Dataout"
    images <- "Images"
  
  
  #scale prevance by a factor of 10k
    prev_fctr <- 10000

# IMPORT ------------------------------------------------------------------

  #PEPFAR sites
    df_sites <- read_csv(here(data_in, "SBU_PEPFAR_USAID_Site_Coordinates_v3_long_SBU.csv"))
  
  #COP20 PLHIV values
    df_plhiv <- read_csv(here(data_out, "COP20_PLHIV.csv"))
  
  #COVID data from JHU
    df_covid <- read_csv(here(data_out, "csse_covid_19_time_series_v2.csv"))
  

# FUNCTIONS ---------------------------------------------------------------

  #align/shorten names across datasets
    
    align_names <- function(df){
      
      if(!"countryname" %in% names(df))
        stop("variable for country must be named 'countryname'")
      
      df <- df %>% 
        mutate(countryname = case_when(countryname %in% c("Congo (Kinshasa)", "Democratic Republic of the Congo") ~ "DRC",
                                       countryname == "Dominican Republic" ~ "DR",
                                       countryname == "Papua New Guinea" ~ "PNG",
                                       TRUE ~ countryname))
      return(df)
    }
  


# SITE DATA ---------------------------------------------------------------
    
    
  #rename indicators for output & fix ctry names
    df_sites <- df_sites %>%
      mutate(service = recode(indicator,
                              "HTS_TST" = "HIV TESTING",
                              "TX_CURR" = "HIV TREATMENT",
                              "LAB_PTCQI" = "HIV LAB",
                              'SC_STOCK' = "STORAGE FACILITY")) %>% 
      align_names()

  #distinct # of sites by ctry 
    df_site_distinct_cnt <- df_sites %>% 
      group_by(countryname) %>% 
      tally(name = "pepfar_site_cnt")

  #distinct # of sites by ctry x service type
    df_site_ind_cnt <- df_sites %>% 
      count(countryname, service, name = "pepfar_site_cnt") %>% 
      complete(service, nesting(countryname), fill = list(pepfar_site_cnt = 0)) %>% 
      mutate(service = factor(service, c("HIV TESTING", "HIV TREATMENT", 
                                         "HIV LAB","STORAGE FACILITY")))

# PLHIV DATA --------------------------------------------------------------

    
  #clean up var & ctry names for merging
    df_plhiv <- df_plhiv %>% 
      rename(countryname = operatingunit,
             plhiv = PLHIV) %>% 
      align_names()

  

# COVID DATA --------------------------------------------------------------

    
  #clean up var + ctry names
    df_covid <- df_covid %>% 
      rename_all(tolower) %>% 
      rename(covid = cases) %>% 
      align_names() 
    
  #limit to just USAID countries with sites & only confirmed cases
    df_covid <- dc_covid %>% 
      filter(iso %in% unique(df_sites$iso),
             category == "Confirmed") %>% 
      select(-cumcases, -category)

  #add var for when cum case count is greater than 10
    df_covid <- df_covid %>% 
      mutate(reached_case_min = covid >=10)

  #identify start date (first ctry w/ 10 cases) for consistency across all ctry viz
    start_date <- df_covid %>% 
      filter(reached_case_min == TRUE) %>% 
      pull(ddate) %>% 
      min()

  #flag latest date for plotting a dot on line in viz
    df_covid_latest <- df_covid %>% 
      filter(ddate == max(ddate))

  #create prevalence vars
    df_plhiv_covid <- df_plhiv %>% 
      full_join(df_covid_latest) %>% 
      full_join(df_site_distinct_cnt) %>% 
      mutate(plhiv_per_10k = round(plhiv/population*prev_fctr, 0),
             covid_per_10k = round(covid/population*prev_fctr, 1))


# VIZ ---------------------------------------------------------------------

  
ctry_sel <- "Tanzania"

service_clrs <- c("HIV TESTING" = USAID_medblue,
                  "HIV TREATMENT" =  USAID_ltblue,
                  "HIV LAB" = USAID_dgrey,
                  "STORAGE FACILITY" = USAID_red)
    
# PLOT SITE TYPES ---------------------------------------------------------
    

    df_site_ind_cnt %>% 
      filter(countryname == ctry_sel) %>% 
      ggplot(aes(pepfar_site_cnt, fct_rev(service), fill = service)) +
      geom_blank(aes(pepfar_site_cnt * 1.05)) +
      geom_col() +
      geom_text(aes(label = comma(pepfar_site_cnt, accuracy = 1)),
                    family = "Source Sans Pro", color = "gray30",
                    hjust = -.5, na.rm = TRUE) +
      geom_vline(aes(xintercept = 0)) +
      scale_x_continuous(expand = c(0.005, 0.005)) +
      scale_fill_manual(values = service_clrs) +
      labs(x = NULL, y = NULL,
           title = "USAID Supported Facilities") +
      si_style_nolines() +
      theme(axis.text.x = element_blank(),
          legend.position = "none")


# PLOT COVID CASES --------------------------------------------------------


    df_covid %>% 
      filter(reached_case_min == TRUE,
             countryname == ctry_sel,
             ) %>% 
      arrange(ddate) %>% 
      mutate(covid = na_if(covid, 0),
             covid_latest = case_when(ddate == max(ddate, na.rm = TRUE) ~ covid)) %>% 
      ggplot(aes(ddate, covid)) +
      geom_area(color = USAID_blue, fill = USAID_blue, alpha = .2, size = 1.4, na.rm = TRUE) +
      geom_point(aes(y = covid_latest), size = 2.5, na.rm = TRUE) +
      expand_limits(x = start_date) +
      scale_y_log10() +
      labs(x = NULL, y = NULL,
           title = "Cumulative CONFIRMED COVID Cases") +
      si_style_ygrid()
      

# MAP SITES ---------------------------------------------------------------


    crosswalk <- tibble(countryname_ne = world$admin, iso = world$iso_a3)

    iso_sel <- df_plhiv_covid %>% 
      filter(countryname == ctry_sel) %>% 
      pull(iso)
    
    ctry_sel_ne <- crosswalk %>% 
      filter(iso == iso_sel) %>% 
      pull(countryname_ne)

    ctry_map <- ne_countries(country = ctry_sel_ne, scale = "medium", returnclass = "sf")
    
    
    ctry_map %>% 
      ggplot() +
      geom_sf() +
      geom_point(data = df_sites %>% filter(countryname == ctry_sel),
                 aes(longitude, latitude, fill = service),
                 size = 2, shape = 21, color = "white") +
      facet_grid(~ service) +
      scale_fill_manual(values = service_clrs) +
      labs(x = NULL, y = NULL) +
      theme_void() +
      theme(text = element_text(family = "Source Sans Pro"),
            legend.position = "none")
    

# PLHIV + COVID STATS -----------------------------------------------------

    
  # Distinct Supported Facilities
    df_plhiv_covid %>% 
      filter(countryname == ctry_sel) %>% 
      pull(pepfar_site_cnt) %>% 
      paste("Distinct USAID Supported Facilitites:", .)
    
  #PLHIV
    df_plhiv_covid %>% 
      filter(countryname == ctry_sel) %>% 
      pull(plhiv) %>% 
      comma(1) %>% 
      paste("PLHIV:", .)
    
  #PLHIV per 10k
    df_plhiv_covid %>% 
      filter(countryname == ctry_sel) %>% 
      pull(plhiv_per_10k) %>% 
      comma(1) %>%
      paste("PLHIV per 10,000:", .)
    
  #COVID
    df_plhiv_covid %>% 
      filter(countryname == ctry_sel) %>% 
      pull(covid) %>% 
      comma(1) %>%
      paste("COVID-19:", .)
    
  #COVID per 10k
    df_plhiv_covid %>% 
      filter(countryname == ctry_sel) %>% 
      pull(covid_per_10k) %>%
      comma(1) %>%
      paste("COVID-19 per 10,000:", .)
  
  
