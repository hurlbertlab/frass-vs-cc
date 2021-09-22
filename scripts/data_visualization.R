
# setup -------------------------------------------------------------------

source('scripts/data_manipulation.R')

# let's get things in order -----------------------------------------------

plotComp = function(
  year, # 2017-2021
  site, # 'NC Botanical Garden' or 'Prairie Ridge Ecostation'
  jdRange = c(1,365), # range of Julian days to analyze
  frass_data, # data frame suitable for frass by week analysis
  frass_reliability = 3, # minimum reliability of frass for inclusion
  frass_var, # 'mean_mass' or 'mean_number'
  cc_data, # data frame suitable for CC by week analysis
  cc_var, # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
  cc_min_length = 0 # minimum caterpiller length for inclusion
  )  {
  
  frass_stats <- 
    meanFrassByWeek(
      surveyData = frass_data,
      for_year = year,
      min_reliability = frass_reliability,
      site_id = site,
      jdRange = jdRange)
  
  cc_stats <- 
    meanDensityByWeek(
      surveyData = cc_data,
      year = year,
      site_id = site,
      ordersToInclude = 'caterpillar',
      minLength = cc_min_length,
      jdRange = jdRange)
  
 # wombo_combo <- 
   cc_stats %>% 
     select(
       julianweek,
       cc_var) %>% 
     inner_join(
       frass_stats %>% 
         select(
           julianWeek,
           frass_var),
       by = c('julianweek' = 'julianWeek')) %>% 
     pivot_longer(
       cols = cc_var:frass_var,
       names_to = 'variable',
       values_to = 'value')
}
