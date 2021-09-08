
source('scripts/session_setup.R')


# frass -------------------------------------------------------------------

# make frass reliability useable

usefulReliability <- 
  frassReliability %>% 
  # remove any incorrect site IDs
  filter(site != 117 | site != 8892356,
         !is.na(reliability)) %>%
  # add site IDs to dataset
  mutate(siteID = ifelse(site == 117,
                'PR',
                ifelse(site == 8892356,
                            'NCBG',
                            NA))) %>% 
  # subset to relevant columns
  select(siteID,
         date,
         reliability)

# make frass data useable

usefulFrass <- 
  frassData %>% 
  # add a year column
  mutate(
    year = year(
      as.Date(frassData$Date.Collected,
              format = '%m/%d/%Y'))) %>%  
  # add a julian week column
  mutate(
    julianWeek = floor(
      julian(
        as.Date(
          frassData$Date.Collected,
          format = '%m/%d/%Y'),
        # keep getting origin must of length 1 error
        origin = paste(.$year, '-01-01'))) / 7) %>% 
    # remove rows where mass or number of frass is NA
  filter(
    !is.na(Frass.mass..mg.),
    !is.na(Frass.number)) %>% 
  # remove any incorrect site IDs
  filter(Site != 'Prairie Ridge' | Site != 'Botanical Garden') %>% 
  # add site IDs to dataset
  mutate(siteID = ifelse(Site == 'Prairie Ridge',
                         'PR',
                         ifelse(Site == 'Botanical Garden',
                                'NCBG',
                                NA))) %>% 
  # subset to relevant columns
  select(siteID,
         date = Date.Collected,
         year,
         julianWeek,
         trapID = Trap,
         frassMassmg = Frass.mass..mg.,
         frassNumber = Frass.number,
         frassMethod = Method,
         notes = Notes) %>% 
  # attach reliability information
  left_join(usefulReliability,
            by = c('siteID', 'date'))
  
# extract CC data and make useable

usefulCC <- 
  ccData %>% 
  # extract caterpillar observations from PR and NCBG
  filter(Name != 'Prairie Ridge Ecostation' | Name != 'NC Botanical Garden',
         Group == 'caterpillar') %>% 
  # add site IDs to dataset
  mutate(siteID = ifelse(Name == 'Prairie Ridge',
                         'PR',
                         ifelse(Name == 'Botanical Garden',
                                'NCBG',
                                NA)))

## need to modify data for:
# do not include if '90 degree' in the notes column - indicates potential false 0 - and do we want to include accuracy sorting for 'tilt' in that column?
# join reliability to main file - include option in density by week function for sorting by reliability

## next steps
# generate density by week function for frass
# check density by week for CC and make sure it is compatibly for comparison with frass


# CC! ---------------------------------------------------------------------


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


#function for calculating and displaying mean density by week for CC data

meanDensityByWeek = function(surveyData,
                             # merged dataframe of Survey and arthropodSighting tables for a single site
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
