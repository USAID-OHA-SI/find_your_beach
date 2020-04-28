## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  ETL JHU COVID-19 Data for PEPFAR Situational Report


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(lubridate)
library(ISOcodes)


# GLOBAL VARIABLES --------------------------------------------------------

data_in <- "Data"
data_out <- "Dataout"

# GITHUB Resources


# JHU Raw data path

  # Source: https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_time_series
  
  jhuRepo <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/"

  # JHU - Daily cumulative report
  dailyReport <- "csse_covid_19_data/csse_covid_19_daily_reports/04-26-2020.csv"
  
  # JHU - Time Series Daily Confirmed Cases
  tsCases <- "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
  tsRecov <- "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv"
  tsDeaths <- "csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"


# READ Data -------------------------------------------------
  
  # Confirmed
  tsCData <- vroom(file.path(paste0(jhuRepo, tsCases)))
  
  # Recovered
  tsRData <- vroom(file.path(paste0(jhuRepo, tsRecov)))
  
  # Deaths
  tsDData <- vroom(file.path(paste0(jhuRepo, tsDeaths)))


# Munge function ----------------------------------------------------------
  # Reshape, coerce date col to dates for sorting, and create daily cases
  
  reshapify <- function(df) {
   df %>% 
      pivot_longer(-c(1:4),
        names_to = "date",
        values_to = "cases") %>% 
      rename(countryname = `Country/Region`) %>% 
      mutate(date = as.Date(date, "%m/%d/%y")) %>% 
      group_by(countryname, date) %>% 
      summarise(cases = sum(cases, na.rm = TRUE)) %>% 
      ungroup() %>% 
      arrange(countryname, date) %>% 
      group_by(countryname) %>% 
      mutate(daily_cases = cases - lag(cases)) %>% 
      ungroup()
  }
  
# Data Cleaning ------------------------------------------------

  # Confirmed
  tsCData <- tsCData %>% 
    reshapify() %>% 
    mutate(category = 'Confirmed') 

  # Recovered
  tsRData <- tsRData %>% 
    reshapify() %>% 
    mutate(category = 'Recovered') 
  
  # Deaths
  tsDData <- tsDData %>% 
    reshapify %>% 
    mutate(category = 'Deaths') 

  # Merge all tables
  tsData <- 
    tsCData %>% 
    bind_rows(list(tsDData, tsRData))

  # Validate countries (No admin2)
  tsData %>% 
    distinct(countryname) %>% 
    pull()
  
  tsData <- ISO_3166_1 %>% 
    select(Name, iso = Alpha_3) %>%
    mutate(Name = recode(Name, 
                         "Congo, The Democratic Republic of the" = "Congo (Kinshasa)",
                         "Myanmar" = "Burma",
                         "Côte d'Ivoire" = "Cote d'Ivoire",
                         "Lao People's Democratic Republic" = "Laos",
                         "Tanzania, United Republic of" = "Tanzania",
                         "Viet Nam" = "Vietnam"
                         )) %>% 
    left_join(tsData, ., by = c("countryname" = "Name")) %>% 
    arrange(countryname, date, category) %>% 
    group_by(countryname, category) %>% 
    mutate(ten_mark = if_else(cases >= 10, 1, 0)) %>% 
    arrange(countryname, date, category) %>% 
    group_by(countryname, category) %>% 
    mutate(days_since_ten = cumsum(ten_mark)) %>% 
    ungroup()
  
# TODOs ---------------------------------------------------------------
  
  #1. Validate results with JHU Dashboards
  
  #2. Update country/region names to match State Department's list

# EXPORT --------------------------------------------------------------
tsData %>% 
  write_csv(file.path(data_out, "csse_covid_19_time_series.csv"), na = "")
  