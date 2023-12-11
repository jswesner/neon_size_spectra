# Data for aqua sync project

# This file organizes the full data set to be in the template
# format for the aquasync project on assessing changes to size 
# spectra across landuse gradients, led by Dr. Dan Perkins. 

# written by Jeff Wesner, December 2023

library(tidyverse)
library(lubridate)

# Full NEON fish data
fish <- readRDS("data/raw_data/fish/fish_stacked.rds")
fish_taxa = read_csv("data/raw_data/fish/neon_fish-taxa.csv") %>% clean_names()

invertebrate_size_data = read_csv("data/derived_data/aqua-sync_data/invertebrate-size-data.csv")

write_csv(fish_taxa, file = "data/derived_data/aqua-sync_data/fish-taxonomy.csv")

# Get sampling_method
sampling_method = fish$fsh_perPass %>% 
  distinct(reachID, samplerType, eventID) %>% 
  clean_names() %>% 
  rename(sampling_method = sampler_type)

# Wrangled fish size/taxa information
fish_dw_taxa <- readRDS("data/derived_data/fish_dw_taxa.rds") %>% 
  ungroup 

fish_size_data_all = fish_dw_taxa %>% 
  mutate(date = as.Date(julian)) %>% 
  mutate(site = paste0(site_id, "_", date),
         sampling_method = "electrofisher") %>% 
  mutate(sample = paste0(reach_id, ".electrofisher"),
         organism_group = "fish",
         body_weight_units = "mg") %>%
  rename(sampling_area = area_m2,
         body_mass = dw,
         count = no_m2) %>% 
  mutate(multiplier = 1/sampling_area) %>% 
  left_join(fish_taxa %>% 
              distinct(taxon_id, scientific_name)) %>% 
  select(site, sampling_method, sample, sampling_area,
         organism_group, scientific_name, body_mass, body_weight_units, count, multiplier) %>% 
  rename(taxon = scientific_name) %>% 
  glimpse()

# fix dates so they match with corresponding invert samples. When multiple fish dates are within 30 days of a corresponding
# invert date, merge them so that only the most recent fish sample is included

invert_dates = invertebrate_size_data %>% 
  distinct(site) %>% 
  separate(site, into = c("site_id", "date"), sep = "_") %>% 
  mutate(date = as.Date(date),
         julian = julian(date))

fish_dates = fish_size_data %>% 
  distinct(site, sample) %>% 
  separate(site, into = c("site_id", "date"), sep = "_") %>% 
  mutate(date = as.Date(date),
         julian = julian(date)) 

# fish samples to keep
fish_samples_to_keep = fish_dates %>% rename(date_fish = date,
                      julian_fish = julian) %>% 
  full_join(invert_dates %>% rename(date_invert = date,
                                    julian_invert = julian)) %>% 
  mutate(date_diff = abs(julian_fish - julian_invert)) %>% 
  filter(date_diff <= 30) %>% 
  group_by(site_id, date_fish) %>% 
  add_tally() %>%         
  filter(date_diff == min(date_diff)) %>% # if more than one sample is within 30 days of the other, keep the closest sampls
  ungroup %>% 
  select(-n) %>% 
  distinct(date_invert, .keep_all = T) %>% 
  mutate(site_invert = paste0(site_id, "_", date_invert)) %>% 
  distinct(site_invert, sample, date_invert)

# remove fish smaller than this cutoff
mean_cutoff = readRDS("data/derived_data/fish_mean_cutoff.rds")

fish_size_data = fish_size_data_all %>% 
  right_join(fish_samples_to_keep) %>% 
  select(-site) %>% 
  rename(site = site_invert) %>% 
  filter(body_mass >= mean_cutoff)

write_csv(fish_size_data, file = "data/derived_data/aqua-sync_data/fish_size_data.csv")


# sanity checks
# do dates match?

check_fish_dates = fish_size_data %>% 
  separate(site, into = c("site_id", "date"), sep = "_") %>% 
  distinct(date) %>% 
  pull()

check_invert_dates = invertebrate_size_data %>%
  distinct(site) %>% 
  separate(site, into = c("site_id", "date"), sep = "_") %>% 
  distinct(date) %>% 
  pull()

# this should be 0, which indicates that all of the fish dates are also included in the invert dates
setdiff(check_fish_dates, check_invert_dates)

check_fish_sites = fish_size_data %>% 
  distinct(site) %>% 
  pull()

check_invert_sites = invertebrate_size_data %>% 
  distinct(site) %>% 
  pull()

# this should be 0, which indicates that all of the fish sites are also included in the invert sites
setdiff(check_fish_sites, check_invert_sites)
