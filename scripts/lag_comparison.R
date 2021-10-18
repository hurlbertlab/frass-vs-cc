
# setup -------------------------------------------------------------------

library(ggplot2)

source('scripts/functions.R')

comp_data <- read_csv('data/processed_data/weekly_stats_2015-2021_rel2.csv')

comp_data %>% 
  cbind(
    map_dfc(
      1:3,
      ~ lead(comp_data$mean_number, n = .)) %>% set_names(c('nlag1', 'nlag2', 'nlag3'))) %>% 
  cbind(
    map_dfc(
      1:3,
      ~ lag(comp_data$mean_number, n = .)) %>% set_names(c('nlag1', 'nlag2', 'nlag3'))) %>% 
  cbind(
    map_dfc(
      1:3,
      ~ lead(comp_data$mean_mass, n = .)) %>% set_names(c('mlead1', 'mlead2', 'mlead3'))) %>% 
  cbind(
    map_dfc(
      1:3,
      ~ lag(comp_data$mean_mass, n = .)) %>% set_names(c('mlag1', 'mlag2', 'mlag3')))

# get R2 values for all - must group by year and site going into lm
