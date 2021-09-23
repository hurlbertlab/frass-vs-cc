
# setup -------------------------------------------------------------------

source('scripts/session_setup.R')

source('scripts/functions.R')

# writing data for analysis -----------------------------------------

# generate three files for three different reliability requirements

map(
  1:3,
  ~summary_stats(
    frass_rel = .,
    write = T))

