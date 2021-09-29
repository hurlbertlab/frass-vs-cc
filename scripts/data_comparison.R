
# setup -------------------------------------------------------------------

# source scripts

source('scripts/functions.R')

# load packages

library(ggplot2)

library(ggpubr)

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
  
  temp <- 
    Data %>% 
    filter(
      site == Site,
      year == Year)
  
  temp_frass <- drop_na(temp, frass_var)
  
  temp_cc <- drop_na(temp, cc_var)
  
  coeff <- 
    temp_frass %>% 
    pull(frass_var) %>% 
    max() /
    temp_cc %>% 
    pull(cc_var) %>% 
    max()
  
  model <- lm(temp %>% pull(cc_var) ~ temp %>% pull(frass_var))
  
  cc_var_name <- case_when(
    cc_var == 'meanBiomass' ~ 'Mean Caterpillar Biomass',
    cc_var == 'meanDensity' ~ 'Mean Density of Caterpillars',
    cc_var == 'fracSurveys' ~ 'Fraction of Surveys with Caterpillars')
  
  frass_var_name <- case_when(
    frass_var == 'mean_mass' ~ 'Mean Mass of Frass',
    frass_var == 'mean_number' ~ 'Mean Number of Frass Pieces')
  
  byweekplot <-
    ggplot(
      data = temp_frass,
      mapping = aes_string(
        x = 'julianweek',
        y = frass_var)) +
    geom_point(color = 'forestgreen') +
    geom_line(color = 'forestgreen') +
    scale_y_continuous(
      name = frass_var_name,
      sec.axis = sec_axis(
        trans = ~./coeff,
        name = cc_var_name)) +
    geom_point(
      data = temp_cc,
      mapping = aes(
        x = julianweek,
        y = .data[[cc_var]]*coeff),
      color = 'blue') +
    geom_line(
      data = temp_cc,
      mapping = aes(
        x = julianweek,
        y = .data[[cc_var]]*coeff),
      color = 'blue') +
    theme(
      axis.title.y.left = element_text(color = 'forestgreen'),
      axis.text.y.left = element_text(color = 'forestgreen'),
      axis.title.y.right = element_text(color = 'blue'),
      axis.text.y.right = element_text(color = 'blue'))

  correlationplot <-
    ggplot(
      data = temp,
      mapping = aes_string(
        x = frass_var,
        y = cc_var)) +
    geom_point() +
    geom_smooth(method = 'lm')

  ggarrange(
      byweekplot, correlationplot,
      nrow = 2) %>%
    annotate_figure(
      top = text_grob(
        paste(
          paste(
            frass_var_name,
            'and',
            cc_var_name,
            sep = ' '),
          paste(
            'for',
            Site,
            'in',
            as.character(Year),
            sep = ' '),
          sep = '\n'),
        face = 'bold',
        size = 14),
      bottom = paste(
        'R^2 =',
        as.character(round(summary(model)$r.squared, 4)),
        sep = ' '))
}


# next steps ------------------------------------

# do we want to do outlier exclusion for individual giant caterpillars?