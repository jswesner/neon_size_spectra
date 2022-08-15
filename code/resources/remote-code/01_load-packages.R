# load the packages for running remote metabolism

# Install latest version of streamMetabolizer.
remotes::install_github('appling/unitted')
# remotes::install_github("appling/streamMetabolizer")
remotes::install_github("USGS-R/streamMetabolizer")

# Load packages.
# library(StreamPULSE)
library(streamMetabolizer)
library(tidyverse)

source("./code/resources/metabFunctions.R")
latlong = read_csv(file = "./data/site_latlong.csv")
