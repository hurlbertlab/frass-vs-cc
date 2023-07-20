
# setup -------------------------------------------------------------------

library(ggplot2)

source('scripts/functions.R')

comp_data <- read_csv('data/processed_data/weekly_stats_2015-2021_rel2.csv')


# lag comparison ----------------------------------------------------------

comp_data %>% 
  group_by(site, year) %>% 
  summarize(
    frass = sum(mean_mass, na.rm = T),
    cc = sum(meanBiomass, na.rm = T))

map(
  c('meanBiomass', 'fracSurveys', 'meanDensity'),
  function(v){
    map(
      c(2015:2019,2021),
      function(y){
        temp <- 
          comp_data %>% 
          group_by(site, year) %>% 
          mutate(
            '0' = mean_mass,
            '1' = lead(mean_mass, 1),
            '2' = lead(mean_mass, 2),
            '3' = lead(mean_mass, 3),
            '-1' = lag(mean_mass, 1),
            '-2' = lag(mean_mass, 2),
            '-3' = lag(mean_mass, 3)) %>% 
          filter(year == y, site == 'NC Botanical Garden') %>% 
          ungroup()
        
        temp %>% 
          select('0','1','2','3','-1','-2','-3') %>% 
          map(~lm(pull(temp, v) ~ .x, data = temp)) %>% 
          map(summary) %>% 
          map('r.squared') %>% 
          bind_cols() %>% 
          pivot_longer(
            cols = '0':'-3',
            names_to = 'lag',
            values_to = 'R2') %>% 
          mutate(
            site = 'NC Botanical Garden',
            year = y,
            var = v,
            lag = as.double(lag),
            .before = 'R2')
      }) %>% bind_rows()
  }) %>% bind_rows() %>% 
  ggplot(
    aes(
      x = lag,
      y = R2,
      color = var)) +
  geom_point() +
  facet_wrap(~year)


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
  bind_cols() %>% 
  pivot_longer(
    cols = mean_mass:mlag3,
    names_to = 'lag',
    values_to = 'R2') %>% 
  mutate(site = 'NC Botanical Garden', year = 2021, .before = 'lag')

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

