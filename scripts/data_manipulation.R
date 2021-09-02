
source('scripts/session_setup.R')

# add Julian week to frassData

frassDataJulian <- 
  frassData %>% 
  mutate(
    Julian.Week = floor(
      julian(
        as.Date(
          frassData$Date.Set,
          format = '%m/%d/%Y'),
        origin = as.Date('2021-01-01')) / 7))