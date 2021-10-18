
# setup -------------------------------------------------------------------

library(ggplot2)

source('scripts/functions.R')

comp_data <- read_csv('data/processed_data/weekly_stats_2015-2021_rel2.csv')

temp <- 
  comp_data %>% 
  group_by(site, year) %>% 
  mutate(
    nlead1 = lead(mean_number, 1),
    nlead2 = lead(mean_number, 2),
    nlead3 = lead(mean_number, 3),
    nlag1 = lag(mean_number, 1),
    nlag2 = lag(mean_number, 2),
    nlag3 = lag(mean_number, 3),
    mlead1 = lead(mean_mass, 1),
    mlead2 = lead(mean_mass, 2),
    mlead3 = lead(mean_mass, 3),
    mlag1 = lag(mean_mass, 1),
    mlag2 = lag(mean_mass, 2),
    mlag3 = lag(mean_mass, 3)) %>% 
  filter(year == 2021, site == 'NC Botanical Garden') %>% 
  ungroup()

temp %>% 
  select(mean_mass, starts_with('ml')) %>% 
  map(~lm(temp$meanBiomass ~ .x, data =)) %>% 
  map(summary) %>% 
  map('r.squared') %>% 
  bind_cols()

# may need two separate map functions, one for each site, to account for missing years in PR

map(
  c('Prairie Ridge Ecostation', 'NC Botanical Garden'),
  function(x){
    temp <- 
    comp_data %>% 
    group_by(site, year) %>% 
    mutate(
      nlead1 = lead(mean_number, 1),
      nlead2 = lead(mean_number, 2),
      nlead3 = lead(mean_number, 3),
      nlag1 = lag(mean_number, 1),
      nlag2 = lag(mean_number, 2),
      nlag3 = lag(mean_number, 3),
      mlead1 = lead(mean_mass, 1),
      mlead2 = lead(mean_mass, 2),
      mlead3 = lead(mean_mass, 3),
      mlag1 = lag(mean_mass, 1),
      mlag2 = lag(mean_mass, 2),
      mlag3 = lag(mean_mass, 3)) %>% 
    filter(year == 2021, site == x) %>% 
    ungroup()
  
  temp %>% 
    select(mean_mass, starts_with('ml')) %>% 
    map(~lm(temp$meanBiomass ~ .x, data =)) %>% 
    map(summary) %>% 
    map('r.squared') %>% 
    bind_cols() %>% 
    mutate(site = x, .before = 'mean_mass')
}) %>% bind_rows()
  
comp_data %>% 
  group_by(site, year) %>% 
  mutate(
    nlead1 = lead(mean_number, 1),
    nlead2 = lead(mean_number, 2),
    nlead3 = lead(mean_number, 3),
    nlag1 = lag(mean_number, 1),
    nlag2 = lag(mean_number, 2),
    nlag3 = lag(mean_number, 3),
    mlead1 = lead(mean_mass, 1),
    mlead2 = lead(mean_mass, 2),
    mlead3 = lead(mean_mass, 3),
    mlag1 = lag(mean_mass, 1),
    mlag2 = lag(mean_mass, 2),
    mlag3 = lag(mean_mass, 3)) %>%  
  group_by(site, year) %>% 
  summarize(
    biomass_mass = tryCatch(
      summary(lm(meanBiomass ~ mean_mass))$r.squared %>% round(4),
      error = function(c){return(0)})
  ) %>% View()
