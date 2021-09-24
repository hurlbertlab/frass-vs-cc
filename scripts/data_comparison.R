
# setup -------------------------------------------------------------------

# load libraries

library(tidyverse)

library(ggplot)

library(lubridate)

# read in data

comp_data_r1 <- 
  read_csv(
    'data/processed_data/weekly_stats_2018-2021_rel1.csv')

comp_data_r2 <- 
  read_csv(
    'data/processed_data/weekly_stats_2018-2021_rel2.csv')

comp_data_r3 <- 
  read_csv(
    'data/processed_data/weekly_stats_2018-2021_rel3.csv')


# next steps ------------------------------------

# check for correlation between frass measurements with different reliabilities - must account for differences between years - for both mass and number
# generate overlayed plots for frass and CC
# raw correlation between frass and cc data - which variables?
