
# setup -------------------------------------------------------------------

source('scripts/data_manipulation.R')

# let's get things in order -----------------------------------------------

# combine frass and cc by week stats for a year and site

combinedByWeek = function(
  year, # 2017-2021
  site, # 'NC Botanical Garden' or 'Prairie Ridge Ecostation'
  jdRange = c(1,365), # range of Julian days to analyze
  frass_data, # data frame suitable for frass by week analysis
  frass_reliability = 3, # minimum reliability of frass for inclusion
  cc_data, # data frame suitable for CC by week analysis
  cc_min_length = 0 # minimum caterpillar length for inclusion
  )  {
  
  frass_stats <- 
    meanFrassByWeek(
      surveyData = frass_data,
      for_year = year,
      min_reliability = frass_reliability,
      site_id = site,
      jdRange = jdRange) %>% 
    mutate(
      site = site,
      year = year)
  
  cc_stats <- 
    meanDensityByWeek(
      surveyData = cc_data,
      year = year,
      site_id = site,
      ordersToInclude = 'caterpillar',
      minLength = cc_min_length,
      jdRange = jdRange) %>% 
    mutate(
      site = site,
      year = year)
  
 cc_stats %>% 
     left_join(
       frass_stats,
       by = c(
         'julianweek' = 'julianWeek',
         'site',
         'year'))
}

# generate a data frame with by-week stats from each year and site - 3 versions for comparison by reliability

compiled_cc_frass_r1 <- 
  map_dfr(
  unique(usefulCC$Year),
  function(x) {
  map_dfr(
    c('NC Botanical Garden', 'Prairie Ridge Ecostation'),
    function(y) {
    combinedByWeek(
    year = x,
    site = y,
    frass_data = usefulFrass,
    frass_reliability = 1,
    cc_data = usefulCC)
    })
}) %>% 
  arrange(
    year,
    site,
    julianweek) %>% 
  relocate(
    site,
    year,
    .before = julianweek)

# save compiled summary stats as a .csv

write_csv(
  compiled_cc_frass_r1,
  paste(
    'data/processed_data/compiled_cc_frass_r1',
    today(),
    '.csv',
    sep = '_'))

# reliability 2

compiled_cc_frass_r2 <- 
  map_dfr(
    unique(usefulCC$Year),
    function(x) {
      map_dfr(
        c('NC Botanical Garden', 'Prairie Ridge Ecostation'),
        function(y) {
          combinedByWeek(
            year = x,
            site = y,
            frass_data = usefulFrass,
            frass_reliability = 2,
            cc_data = usefulCC)
        })
    }) %>% 
  arrange(
    year,
    site,
    julianweek) %>% 
  relocate(
    site,
    year,
    .before = julianweek)

# save compiled summary stats as a .csv

write_csv(
  compiled_cc_frass_r2,
  paste(
    'data/processed_data/compiled_cc_frass_r2',
    today(),
    '.csv',
    sep = '_'))

# reliability 3

compiled_cc_frass_r3 <- 
  map_dfr(
    unique(usefulCC$Year),
    function(x) {
      map_dfr(
        c('NC Botanical Garden', 'Prairie Ridge Ecostation'),
        function(y) {
          combinedByWeek(
            year = x,
            site = y,
            frass_data = usefulFrass,
            frass_reliability = 3,
            cc_data = usefulCC)
        })
    }) %>% 
  arrange(
    year,
    site,
    julianweek) %>% 
  relocate(
    site,
    year,
    .before = julianweek)

# save compiled summary stats as a .csv

write_csv(
  compiled_cc_frass_r3,
  paste(
    'data/processed_data/compiled_cc_frass_r3',
    today(),
    '.csv',
    sep = '_'))

