# frass-vs-cc
Comparing phenology patterns between frass observation and Caterpillars Count! observations.

This repo contains data and code for the purpose of analyzing patterns in caterpillar phenology.

This project will focus on comparing observed caterpillar phenology patterns based on the contents of frass traps and observed phenology based on observations of caterpillars from the Caterpillars Count! citizen science project.

Most informative output from this work is found in the Rmd and corresponding pdf files in the output folder.

scripts/

data_comparison.R: code for comparing frass mass pattern across different reliability thresholds.

functions.R: code including several functions frequently used to analyze Caterpillars Count! or frass data.

lag_comparison.R: code to compare frass and caterpillar abundance accounting for potential lag between the two.

session_setup.R: code with useful bits and bobs to work on data.


data/

processed_data/

weekly_stats_2015-2021_rel[1-3].csv: weekly summary statistics from Caterpillars Count! and frass where only data above reliability threshold of 1, 2, or 3 is included.
  site: either NC Botanical Garden or Prairie Ridge Ecostation.
  julianweek: the ordinal day of the first day of week summarized.
  numSurveysGTzero: the number of survey conducted in the week summarized where caterpillar mass was greater than zero.
  totalBiomass: the total biomass of caterpillars for the week summarized based on the length values recorded in Caterpillars Count!
  nSurveyBranches: the number of branches surveyed in the summarized week.
  nSurveys: the number of surveys conducted in the summarized week.
  meanDensity: the mean number of caterpillars on surveys in the summarized week.
  fracSurveys: the percent of surveys were caterpillars were observed in the summarized week.
  meanBiomass: the mean biomass, in milligrams, of caterpillars observed per survey in the summarized week.
  n_traps: the number of frass traps used to collect frass in the summarized week.
  mean_mass: the mean mass, in milligrams, of frass collected per trap in the summarized week.
  mean_number: the mean number of frass pieces collected per trap int he summarized week.
  reliability: the recorded reliability of frass data in the summarized week, with 3 being the greatest reliability.
  
raw_data/

CC_2021-08-10.csv: the full Caterpillars Count! dataset downloaded on the given data. See the README in arth-trees for metadata.

frass_2021-08-30.csv: data on frass collected in traps at Prairie Ridge Ecostation and the NC Botanical Garden from 2015-2021.
  Date.Set and Time.Set: when the frass trap was emptied and set out to capture frass.
  Date.Collected and Time.Collected: when frass was collected from the trap for the given iteration of the trap.
  Trap: the identifier for the trap.
  Frass.mass..mg: the total mass of frass collected during the given time.
  Frass.number: the number of frass pieces colelcted during the given time.
  Method: the type of frass trap used for collection.
  
frass_reliability_2021-09-03: data on the reliability of frass collected based on the weather conditions.
  site: code corresponding to either Prairie Ridge Ecostation or NC Botanical Garden.