
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


# big boy plotting function -----------------------------------------------

comp_plot = function(
  Data,
  Site,
  Year,
  frass_var, # 'mean_mass' or 'mean_number' 
  cc_var # 'meanBiomass', 'meanDensity', or 'fracSurveys'
){
  
  temp <- extract_stats(data = Data, Site = Site, Year = Year)
  
  coeff <- 
    temp %>% 
    drop_na(frass_var) %>% 
    pull(frass_var) %>% 
    max() /
    Data %>% 
    drop_na(cc_var) %>% 
    pull(cc_var) %>% 
    max()

  model <-
    lm(
      temp %>% pull(cc_var) ~ temp %>% pull(frass_var))

  summary(model)
  
  typeof(temp[[frass_var]])
  
  ggplot(
    data = temp %>%
      drop_na(frass_var),
    mapping = aes(
      x = julianweek,
      y = drop_na(temp, frass_var)[[frass_var]])) + # you gotta do something about this - maybe rewrite data frame at opening of function?
    geom_point(color = 'forestgreen') +
    geom_line(color = 'forestgreen') #+
#     scale_y_continuous(
#       name = case_when(
#         frass_var == 'mean_mass' ~ 'Mean Mass of Frass',
#         frass_var == 'mean_number' ~ 'Mean Number of Frass Pieces'),
#       sec.axis = sec_axis(
#         trans = ~./coeff,
#         name = case_when(
#           cc_var == 'meanBiomass' ~ 'Mean Caterpillar Biomass',
#           cc_var == 'meanDensity' ~ 'Mean Density of Caterpillars',
#           cc_var == 'fracSurveys' ~ 'Fraction of Surveys with Caterpillars'))) +
    # geom_point(
    #   data = temp %>%
    #     drop_na(cc_var),
    #   mapping = aes(
    #     x = julianweek,
    #     y = cc_var*coeff),
    #   color = 'blue') +
    # geom_line(
    #   data = temp %>%
    #     drop_na(cc_var),
    #   mapping = aes(
    #     x = julianweek,
    #     y = cc_var*coeff),
    #   color = 'blue') +
    # theme(
    #   axis.title.y.left = element_text(color = 'forestgreen'),
    #   axis.text.y.left = element_text(color = 'forestgreen'),
    #   axis.title.y.right = element_text(color = 'blue'),
    #   axis.text.y.right = element_text(color = 'blue'))
}

# raw correlation ---------------------------------------------------------

lm(
  meanBiomass ~ mean_mass,
  data = extract_stats(comp_data_r3, 'NC Botanical Garden', 2021)) %>% summary()

ggplot(
  data = extract_stats(comp_data_r3, 'NC Botanical Garden', 2021),
  mapping = aes(
    x = mean_mass,
    y = meanBiomass)) +
  geom_point() +
  geom_smooth(method = 'lm')

# plot data together ------------------------------------------------------

ggplot(
  data = extract_stats(comp_data_r3, 'NC Botanical Garden', 2021) %>% 
    drop_na(mean_mass),
  mapping = aes(
    x = julianweek,
    y = mean_mass)) +
  geom_point(color = 'forestgreen') +
  geom_line(color = 'forestgreen') +
  scale_y_continuous(
    name = 'Mean Frass Mass',
    sec.axis = sec_axis(
      ~./(max(
        extract_stats(comp_data_r3, 'NC Botanical Garden', 2021) %>% 
          drop_na(mean_mass) %>% 
          pull(mean_mass)
        /max(
          extract_stats(comp_data_r3, 'NC Botanical Garden', 2021) %>% 
            pull(meanBiomass)))), 
      name = 'Mean Caterpillar Biomass')) +
  geom_point(
    data = extract_stats(comp_data_r3, 'NC Botanical Garden', 2021),
    mapping = aes(
      x = julianweek,
      y = meanBiomass*(max(mean_mass[!is.na(mean_mass)]/max(meanBiomass)))), # when you make a custom plotting function, assign a name to this garbage monster
    color = 'blue') +
  geom_line(
    data = extract_stats(comp_data_r3, 'NC Botanical Garden', 2021),
    mapping = aes(
      x = julianweek,
      y = meanBiomass*(max(mean_mass[!is.na(mean_mass)]/max(meanBiomass)))),
    color = 'blue') +
  theme(
    axis.title.y.left = element_text(color = 'forestgreen'),
    axis.text.y.left = element_text(color = 'forestgreen'),
    axis.title.y.right = element_text(color = 'blue'),
    axis.text.y.right = element_text(color = 'blue'))

# next steps ------------------------------------

# raw correlation between frass and cc data
# check for correlation between frass measurements with different reliabilities - must account for differences between years - for both mass and number
# generate overlayed plots for frass and CC
