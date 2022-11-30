# load the packages for running remote metabolism

# Install latest version of streamMetabolizer.
# remotes::install_github('appling/unitted')
# remotes::install_github("appling/streamMetabolizer")
# remotes::install_github("USGS-R/streamMetabolizer")
# remotes::install_github('streampulse/StreamPULSE')
rstan::rstan_options(auto_write = TRUE)

# Load packages.
library(unitted)
library(StreamPULSE)
library(streamMetabolizer)
library(tidyverse)
library(brms)
library(mgcv)
library(rstan)
# library(cmdstanr)
'%ni%' <- Negate('%in%')


source("./code/resources/metabFunctions.R")
latlong = read_csv(file = "./data/site_latlong.csv")
streams = readRDS(file = "./data/derived_data/streams.rds")

theme_set(theme_minimal())
