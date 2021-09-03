
# load libraries

library(gsheet)
library(lubridate)
library(tidyverse)

# read in primary frass (frassData) and Caterpillars Count (ccData) files

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
