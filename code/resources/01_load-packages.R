# Load packages.
library(here)
here::i_am("./code/resources/01_load-packages.R")
library(tidyverse)
library(unitted)
library(StreamPULSE)
library(streamMetabolizer)
library(brms)
library(mgcv)
library(dygraphs)
library(xts)
library(rstan)
# library(cmdstanr)
library(reaRate)
'%ni%' <- Negate('%in%')

# Install latest version of streamMetabolizer.
# remotes::install_github('appling/unitted')
# remotes::install_github("appling/streamMetabolizer")
# remotes::install_github("USGS-R/streamMetabolizer")
# remotes::install_github('streampulse/StreamPULSE')
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source(here::here("code/resources/metabFunctions.R"))
latlong = read_csv(file = here::here("data/site_latlong.csv"))
streams = readRDS(file = here::here("data/derived_data/streams.rds"))

theme_set(theme_minimal())
