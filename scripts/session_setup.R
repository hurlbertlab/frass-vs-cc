

# load libraries ----------------------------------------------------------

library(gsheet)
library(lubridate)
library(tidyverse)


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
  filter(site == 117 | site == 8892356,
         !is.na(reliability)) %>%
  # add site IDs to dataset
  mutate(siteID = ifelse(site == 117,
                         'Prairie Ridge Ecostation',
                         ifelse(site == 8892356,
                                'NC Botanical Garden',
                                NA))) %>% 
  # subset to relevant columns
  select(siteID,
         date,
         reliability)

# make frass data useable

usefulFrass <- 
  frassData %>% 
  # add a year column
  mutate(
    year = year(
      as.Date(frassData$Date.Collected,
              format = '%m/%d/%Y'))) %>%  
  # add a julian week column
  # try using yday()
  mutate(julianDay = yday(as.Date(frassData$Date.Collected,
                                  format = '%m/%d/%Y')),
         julianWeek = 7*floor(julianDay/7)+4) %>% 
  # remove rows where mass or number of frass is NA
  drop_na(Frass.mass..mg.,
          Frass.number) %>% 
  # remove any incorrect site IDs
  filter(Site != 'Prairie Ridge' | Site != 'Botanical Garden') %>% 
  # add site IDs to dataset
  mutate(siteID = ifelse(Site == 'Prairie Ridge',
                         'Prairie Ridge Ecostation',
                         ifelse(Site == 'Botanical Garden',
                                'NC Botanical Garden',
                                NA))) %>% 
  # subset to relevant columns
  select(siteID,
         date = Date.Collected,
         year,
         julianDay,
         julianWeek,
         trapID = Trap,
         frassMassmg = Frass.mass..mg.,
         frassNumber = Frass.number,
         frassMethod = Method,
         notes = Notes) %>% 
  # attach reliability information
  left_join(usefulReliability,
            by = c('siteID', 'date'))

# extract CC data and make useable

usefulCC <- 
  ccData %>% 
  # extract caterpillar observations from PR and NCBG
  filter(Name %in% c('Prairie Ridge Ecostation', 'NC Botanical Garden')) %>% 
  # add site IDs to dataset
  mutate(siteID = Name)


# necessary custom functions ----------------------------------------------

# Function for calculating the mode of a series of values
# --in this particular use case, if there multiple modes, we want the largest value
Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}


# Function for substituting values based on a condition using dplyr::mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

