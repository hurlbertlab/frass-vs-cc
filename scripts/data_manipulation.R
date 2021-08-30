
# setup -------------------------------------------------------------------

library(gsheet)
library(lubridate)
library(tidyverse)


# downloading and opening data --------------------------------------------------------

# function for downloading (write) or displaying (open) up-to-date frass records from Google Drive

frassData = function(open = F, write = F, makeObject = F) {
  require(gsheet)
  url = "https://docs.google.com/spreadsheets/d/1RwXzwhHUbP0m5gKSOVhnKZbS1C_NrbdfHLglIVCzyFc/edit#gid=1479231778"
  data = gsheet2tbl(url)
  if (write) {
    write.csv(data,
              paste('data/raw_data/frass_',
                    Sys.Date(),
                    '.csv',
                    sep = ''),
              row.names = F)
  }
  if (open) {return(data)}
  if (makeObject) {frass_data <<- gsheet2tbl(url)}
}


# editing data ------------------------------------------------------------

# function that takes date field (%m/%d/%Y) and time field (hh:mm, 24h time), converts date to julian day, adds fractional day represented by hrs and mins

julianDayTime = function(date, hour_min) {
  require(lubridate)
  jday = yday(date)
  temp = sapply(strsplit(hour_min, ":"),
                function(x) {
                  x = as.numeric(x)
                  x[1] + x[2]/60
  })
  output = jday + temp/24
  return(output)
}
