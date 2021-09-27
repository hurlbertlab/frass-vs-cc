
source('scripts/session_setup.R')

# component custom functions ----------------------------------------------

# Function for calculating the mode of a series of values
# --in this particular use case, if there multiple modes, we want the largest value
Mode = function(x){ 
  if (!is.numeric(x)) {
    stop("values must be numeric for mode calculation")
  }
  ta = table(x)
  tam = max(ta)
  mod = as.numeric(names(ta)[ta == tam])
  return(max(mod))
}


# Function for substituting values based on a condition using dplyr::mutate
# Modification of dplyr's mutate function that only acts on the rows meeting a condition
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}


# data extraction functions -----------------------------------------------

# function for extracting by-week summary statistics from a single site and year

extract_stats = function(data, Site, Year){
  data %>% 
    filter(
      year == Year,
      site == Site)
  }

# frass analysis -------------------------------------------------------------------

meanFrassByWeek = function(
  surveyData,
  for_year, # 2015-2021
  min_reliability = 3, # 1-3, with 3 being the highest reliability
  site_id, # 'NC Botanical Garden' or 'Prairie Ridge Ecostation'
  jdRange = c(1,365),
  plot = F,
  plotVar = 'mean_mass' # 'mean_mass' or 'mean_number',
) {
  
  # filter data to site, year, and reliability; remove tilted traps from dataset
  
  filter1 <-
    surveyData %>% 
    filter(
      siteID == site_id,
      year == for_year,
      reliability >= min_reliability,
      !grepl('tilt', notes),
      julianDay >= jdRange[1],
      julianDay <= jdRange[2])
  
  # calculate mean mass and number of frass by week
  week_means <- 
  filter1 %>% 
    group_by(julianWeek) %>% 
    summarize(
      n_traps = n(),
      mean_mass = mean(frassMassmg),
      mean_number = mean(frassNumber))
  
  if(plot == F){
    return(week_means)
  }
  
  if(plot == T) {
    return(ggplot(
      week_means,
      aes(
        x = julianWeek,
        y = case_when(
          plotVar == 'mean_mass' ~ mean_mass,
          plotVar == 'mean_number' ~ mean_number))) +
        geom_point() +
        geom_line() +
        labs(
          x = 'Julian Week',
          y = case_when(
            plotVar == 'mean_mass' ~ 'Mean Mass of Frass (mg)',
            plotVar == 'mean_number' ~ 'Mean Number of Frass'),
          title = case_when(
            plotVar == 'mean_mass' ~ 'Mean Frass Mass by Julian Week',
            plotVar == 'mean_number' ~ 'Mean Frass Number by Julian Week'),
          subtitle = for_year))
    }
}

# CC! ---------------------------------------------------------------------

#function for calculating and displaying mean density by week for CC data

meanDensityByWeek = function(
  surveyData,
  year,
  site_id,
  ordersToInclude = 'All',
  # which arthropod orders to calculate density for (codes)
  minLength = 0,
  # minimum arthropod size to include 
  jdRange = c(1,365),
  outlierCount = 10000,
  plot = FALSE,
  plotVar = 'fracSurveys', 
  # 'meanDensity' or 'fracSurveys' or 'meanBiomass'
  minSurveyCoverage = 0.8,
  # minimum proportion of unique survey branches examined per week in order to include the week as a data point
  allDates = TRUE,
  new = TRUE,
  color = 'black',
  allCats = TRUE,
  ...) 
{
  surveyData <- 
    filter(
      surveyData,
      Year == year,
      siteID == site_id)
  
  if(length(ordersToInclude)==1 & ordersToInclude[1]=='All') {
    ordersToInclude = unique(surveyData$Group)
  }
  
  numUniqueBranches = length(unique(surveyData$PlantFK))
  
  firstFilter = surveyData %>%
    filter(julianday >= jdRange[1], julianday <= jdRange[2]) %>%
    mutate(julianweek = 7*floor(julianday/7) + 4)
  
  effortByWeek = firstFilter %>%
    group_by(julianweek) %>%
    summarize(nSurveyBranches = n_distinct(PlantFK),
              nSurveys = n_distinct(ID)) %>%
    mutate(modalBranchesSurveyed = Mode(5*ceiling(nSurveyBranches/5)),
           nSurveySets = nSurveys/modalBranchesSurveyed,
           modalSurveySets = Mode(round(nSurveySets)),
           okWeek = ifelse(nSurveySets/modalSurveySets >= minSurveyCoverage, 1, 0))
  
  if (allDates) {
    effortByWeek$okWeek = 1
  }
  
  if (!allCats) {
    secondFilter = firstFilter %>%
      filter(Hairy != 1, Tented != 1, Rolled != 1)
  } else {
    secondFilter = firstFilter
  }
  
  arthCount = secondFilter %>%
    filter(Length >= minLength, 
           Group %in% ordersToInclude) %>%
    mutate(Quantity2 = ifelse(Quantity > outlierCount, 1, Quantity)) %>% #outlier counts replaced with 1
    group_by(julianweek) %>%
    summarize(totalCount = sum(Quantity2, na.rm = TRUE),
              numSurveysGTzero = length(unique(ID[Quantity > 0])),
              totalBiomass = sum(Biomass_mg, na.rm = TRUE)) %>% 
    right_join(effortByWeek, by = 'julianweek') %>%
    filter(okWeek == 1) %>%
    #next line replaces 3 fields with 0 if the totalCount is NA
    mutate_cond(is.na(totalCount), totalCount = 0, numSurveysGTzero = 0, totalBiomass = 0) %>%
    mutate(meanDensity = totalCount/nSurveys,
           fracSurveys = 100*numSurveysGTzero/nSurveys,
           meanBiomass = totalBiomass/nSurveys) %>%
    arrange(julianweek) %>%
    data.frame()
  
  if (plot & new) {
    plot(arthCount$julianweek, arthCount[, plotVar], type = 'l', 
         col = color, las = 1, ...)
    points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  } else if (plot & new==F) {
    points(arthCount$julianweek, arthCount[, plotVar], type = 'l', col = color, ...)
    points(arthCount$julianweek, arthCount[, plotVar], pch = 16, col = color, ...)
  }
  return(arthCount)
}


# combining data ----------------------------------------------------------

# combine frass and cc by week stats for a year and site

combinedByWeek = function(
  year, # 2017-2021
  site, # 'NC Botanical Garden' or 'Prairie Ridge Ecostation'
  jdRange = c(1,365), # range of Julian days to analyze
  frass_data, # data frame suitable for frass by week analysis
  frass_reliability = 2, # minimum reliability of frass for inclusion
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
    full_join(
      frass_stats,
      by = c(
        'julianweek' = 'julianWeek',
        'site',
        'year'))
}

# generate a data frame with by-week stats from all years and sites

summary_stats = function(
  years = 2018:year(today()),
  sites = c('NC Botanical Garden', 'Prairie Ridge Ecostation'),
  frass_rel = 2,
  write = F,
  view = T) {
  
  temp <- 
    map_dfr(
      years,
      function(x) {
        map_dfr(
          sites,
          function(y) {
            combinedByWeek(
              year = x,
              site = y,
              frass_data = usefulFrass,
              frass_reliability = frass_rel,
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
  
  if(write == T) {
    write_csv(
      temp,
      file = paste(
        'data/processed_data/',
        'weekly_stats_',
        paste(
          min(temp$year),
          max(temp$year),
          sep = '-'),
        paste(
          '_rel',
          frass_rel,
          sep = ''),
        '.csv',
        sep = ''))
  }
  
  if(view == T) {
    temp
  }
}
