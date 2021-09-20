
# setup -------------------------------------------------------------------

source('scripts/session_setup.R')

source('scripts/data_manipulation.R')

# let's get things in order -----------------------------------------------

plotComp = function(
  year, # 2017-2021
  site, # 'NC Botanical Garden' or 'Prairie Ridge Ecostation'
  jdRange = c(1,365), # range of Julian days to analyze
  frass_data, # data frame suitable for frass by week analysis
  frass_reliability = 3, # minimum reliability of frass for inclusion
  frass_var, # 'mass' or 'number'
  cc_data, # data frame suitable for CC by week analysis
  cc_var, # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
  cc_min_length = 0 # minimum caterpiller length for inclusion
  )  {
  
  frassStats <- 
    meanFrassByWeek(
      surveyData = frass_data,
      for_year = year,
      min_reliability = frass_reliability,
      site_id = site,
      jdRange = jdRange)
  
  ccStats <- 
    meanDensityByWeek(
      surveyData = cc_data,
      year = year,
      site_id = site,
      ordersToInclude = 'caterpillar',
      minLength = cc_min_length,
      jdRange = jdRange)
  
}
