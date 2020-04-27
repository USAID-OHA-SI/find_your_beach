## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  ETL JHU COVID-19 Data for PEPFAR Situational Report


# DEPENDENCIES ------------------------------------------------------------

library(tidyverse)
library(vroom)
library(lubridate)


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


# Data Cleaning ------------------------------------------------

  # Confirmed
  tsCData <- tsCData %>% 
    gather(ddate, cases, -c(1:4)) %>% 
    select(-1) %>% 
    group_by(`Country/Region`, Lat, Long, ddate) %>% 
    summarise(
      cases = sum(cases, na.rm = F)
    ) %>% 
    mutate(
      cumcases = cumsum(cases)
    ) %>% 
    ungroup() %>% 
    mutate(
      category = 'Confirmed'
    ) 

  # Recovered
  tsRData <- tsRData %>% 
    gather(ddate, cases, -c(1:4)) %>% 
    select(-1) %>% 
    group_by(`Country/Region`, Lat, Long, ddate) %>% 
    summarise(
      cases = sum(cases, na.rm = F)
    ) %>% 
    mutate(
      cumcases = cumsum(cases)
    ) %>%  
    ungroup() %>% 
    mutate(
      category = 'Recovered'
    ) 
  
  # Deaths
  tsDData <- tsDData %>% 
    gather(ddate, cases, -c(1:4)) %>% 
    select(-1) %>% 
    group_by(`Country/Region`, Lat, Long, ddate) %>% 
    summarise(
      cases = sum(cases, na.rm = F)
    ) %>% 
    mutate(
      cumcases = cumsum(cases)
    ) %>% 
    ungroup() %>% 
    mutate(
      category = 'Deaths'
    ) 

  # Merge all tables
  tsData <- tsCData %>% 
    rbind(tsDData) %>% 
    rbind(tsRData) %>%
    mutate(
      ddate = mdy(ddate)
    ) %>%
    rename(Country_Region = `Country/Region`) 

  # Validate countries (No admin2)
  tsData %>% 
    distinct(Country_Region) %>% 
    pull()
  
  
# TODOs ---------------------------------------------------------------
  
  #1. Validate results with JHU Dashboards
  
  #2. Update country/region names to match State Department's list

# EXPORT --------------------------------------------------------------
tsData %>% 
  write_csv(file.path(data_out, "csse_covid_19_time_series.csv"), na = "")
  