# Data for aqua sync project

# This file organizes the full data set to be in the template
# format for the aquasync project on assessing changes to size 
# spectra across landuse gradients, led by Dr. Dan Perkins. 

# written by Justin Pomeranz, October 2023

# this script is for the site metadata

library(tidyverse)

# read in site-date collection information
site_date <- read_csv("data/derived_data/aqua-sync_data/site_date.csv")
# read in field metadata
field <- read_csv("data/field_data.csv")
View(field)
names(field)

field_selected <- field %>%
  rename(
    siteID = `Site ID`,
    geographical_position_specification = State,
    geographical_latitude = Latitude,
    geographical_longitude = Longitude,
    geographical_altitude = Elevation,
    landuse_type = `Dominant NLCD Classes`) %>%
  select(siteID,
         geographical_position_specification,
         geographical_latitude,
         geographical_longitude,
         geographical_altitude,
         landuse_type)

field_date <- site_date %>%
  select(site, siteID, sampling_year, sampling_month) %>%
  left_join(field_selected) %>%
  select(-siteID)

names(field_date)

site_meta <- field_date %>%
  mutate(data_owner = "Pomeranz",
         data_contributors = "NEON: National Ecological Observation Netwok",
         geographical_position = "United States of America",
         multiple_sampling = ">1",
         organism_groups = "Invertebrates + Fish",
         trophic_levels = ">2",
         sampling_methodology = "Hess, Surbers, Kicknets, Electrofishing") %>%
  select(
    data_owner,
    data_contributors,
    site,
    geographical_position,
    geographical_position_specification,
    geographical_latitude,
    geographical_longitude,
    geographical_altitude,
    sampling_year,
    sampling_month,
    multiple_sampling,
    landuse_type,
    organism_groups,
    trophic_levels,
    sampling_methodology)

write_csv(site_meta, 
          "data/derived_data/aqua-sync_data/site_metadata.csv")
