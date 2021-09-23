
# load libraries ----------------------------------------------------------

library(gsheet)
library(tidyverse)
library(lubridate)


# read in raw data files --------------------------------------------------

frassData <- read_csv('data/raw_data/frass_2021-08-30.csv')

ccData <- read_csv('data/raw_data/CC_2021-08-10.csv')

# download frass reliability data - not necessary unless outdated

#gsheet2tbl('https://docs.google.com/spreadsheets/d/1RwXzwhHUbP0m5gKSOVhnKZbS1C_NrbdfHLglIVCzyFc/edit#gid=1611171427') %>% 
#  write.csv(paste('data/raw_data/frass_reliability_',
#                  Sys.Date(),
#                  '.csv',
#                  sep = ''))


# read in frass reliability data

frassReliability <- read_csv('data/raw_data/frass_reliability_2021-09-03.csv')


# convert raw data to useful form -----------------------------------------

# make frass reliability useable

usefulReliability <- 
  frassReliability %>% 
  # remove any incorrect site IDs
  filter(site %in% c(117, 8892356)) %>%
  # add site IDs to dataset
  mutate(
    date = mdy(date),
    siteID = case_when(
      site == 117 ~ 'Prairie Ridge Ecostation',
      site == 8892356 ~ 'NC Botanical Garden')) %>% 
  # subset to relevant columns
  select(
    siteID,
    date,
    reliability)

# make frass data useable

usefulFrass <- 
  frassData %>% 
  # add a year column and working date column
  mutate(
    date = mdy(Date.Collected),
    year = year(date)) %>%
  # add a julian week column
  mutate(
    julianDay = yday(date),
    julianWeek = 7*floor(julianDay/7)+4) %>% 
  # remove rows where mass or number of frass is NA
  drop_na(
    Frass.mass..mg.,
    Frass.number) %>% 
  # remove any incorrect site IDs
  filter(Site != 'Prairie Ridge' | Site != 'Botanical Garden') %>% 
  # add site IDs to dataset
  mutate(siteID = case_when(
    Site == 'Prairie Ridge' ~ 'Prairie Ridge Ecostation',
    Site == 'Botanical Garden' ~ 'NC Botanical Garden')) %>% 
  # subset to relevant columns
  select(
    siteID,
    date,
    year,
    julianDay,
    julianWeek,
    trapID = Trap,
    frassMassmg = Frass.mass..mg.,
    frassNumber = Frass.number,
    frassMethod = Method,
    notes = Notes) %>% 
  # attach reliability information
  left_join(
    usefulReliability,
    by = c('siteID', 'date'))

# extract CC data and make useable

usefulCC <- 
  ccData %>% 
  # extract caterpillar observations from PR and NCBG
  filter(Name %in% c('Prairie Ridge Ecostation', 'NC Botanical Garden')) %>% 
  # add site IDs to dataset
  mutate(siteID = Name)

