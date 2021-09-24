
# setup -------------------------------------------------------------------

# source scripts

source('scripts/functions.R')

# load packages

library(ggplot2)

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


# plot data together ------------------------------------------------------

ggplot(
  data = comp_data_r3 %>% 
    filter(
      year == 2021,
      site == 'NC Botanical Garden') %>% 
    drop_na(mean_mass),
  mapping = aes(
    x = julianweek,
    y = mean_mass)) +
  geom_point(color = 'forestgreen') +
  geom_line(color = 'forestgreen') +
  scale_y_continuous(
    name = 'Mean Frass Mass',
    sec.axis = sec_axis(~./5, name = 'Mean Caterpillar Biomass')) +
  geom_point(
    data = comp_data_r3%>% 
      filter(
        year == 2021,
        site == 'NC Botanical Garden'),
    mapping = aes(
      x = julianweek,
      y = meanBiomass*5),
    color = 'blue') +
  geom_line(
    data = comp_data_r3%>% 
      filter(
        year == 2021,
        site == 'NC Botanical Garden'),
    mapping = aes(
      x = julianweek,
      y = meanBiomass*5),
    color = 'blue')

# next steps ------------------------------------

# check for correlation between frass measurements with different reliabilities - must account for differences between years - for both mass and number
# generate overlayed plots for frass and CC
# raw correlation between frass and cc data - which variables?