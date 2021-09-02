
# load libraries

library(gsheet)
library(lubridate)
library(tidyverse)

# read in primary frass (frassData) and Caterpillars Count (ccData) files

frassData <- read_csv('data/raw_data/frass_2021-08-30.csv')

ccData <- read_csv('data/raw_data/CC_2021-08-10.csv')
