# GPP data download script.
'%ni%' = Negate('%in%')
# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(DBI)
# source("./code/update_data_products.R")
# Load stream names
streams = readRDS(file = "./data/derived_data/streams.rds")
latlong = read_csv(file = "./data/site_latlong.csv")


for(i in seq_along(streams)){
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_30min_airPressure.rds")
  # load, stack, munge, and save depth files
  neonstore::neon_read(product = "DP1.00004.001",
                       table = "BP_30min-basic",
                       site = streams[i],
                       altrep = FALSE
  ) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  gc()
}