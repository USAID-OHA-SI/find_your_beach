## PROJECT:  find your beach
## AUTHOR:   B.Kagniniwa | USAID
## LICENSE:  MIT
## PURPOSE:  Extract PEPFAR OU/Country/SNU1/PSNU

## Libraries
library(tidyverse)
library(readxl)
library(sf)


# Read Citis location data
global_org <- read_csv("./Data/HFR_FY20_GLOBAL_orghierarchy_20200306.csv")

global_org %>% names()

# Extract OUs
ous <- global_org %>% 
  distinct(operatingunit) %>% 
  pull()

# Extract Countries
cntries <- global_org %>% 
  distinct(countryname) %>% 
  pull()

# Extract OU/Countries
target_cntries <- global_org %>% 
  distinct(operatingunit, countryname) 

# Extract OU/Countries/SNU1/PSNUs
target_psnu <- global_org %>% 
  distinct(operatingunit, countryname, snu1, psnu)

target_psnu 




