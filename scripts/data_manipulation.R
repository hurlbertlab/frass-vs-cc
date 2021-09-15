
source('scripts/session_setup.R')


## next steps:
# do not include if '45 degree ' or 90 degree' in the notes column - indicates potential false 0 - and do we want to include accuracy sorting for 'tilt' in that column?
# generate mass and number by week function for frass
# check density by week for CC and make sure it is compatible for comparison with frass



# frass -------------------------------------------------------------------

meanFrassByWeek = function(
  surveyData,
  for_year, # 2018-2021
  min_reliability = 3, # 1-3, with 3 being the highest reliability
  site_id, # 'NC Botanical Garden' or 'Prairie Ridge Ecostation'
  jdRange = c(1,365),
  plot = F,
  plotVar = 'mass' # 'mass' or 'number',
) {
  
  # filter data to site, year, and reliability; remove tilted traps from dataset
  
  # filter1 <-
    surveyData %>% 
    filter(
      siteID == site_id,
      year == for_year,
      reliability >= min_reliability,
      !grepl('tilt', notes))
  
  # calculate mean mass and number of frass by 
  
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


# data comparison ---------------------------------------------------------

# join data by julian week for plotting
