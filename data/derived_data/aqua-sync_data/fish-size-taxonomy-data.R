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

write_csv(fish_taxa, file = "data/derived_data/aqua-sync_data/fish-taxonomy.csv")

# Get sampling_method
sampling_method = fish$fsh_perPass %>% 
  distinct(reachID, samplerType, eventID) %>% 
  clean_names() %>% 
  rename(sampling_method = sampler_type)

# Wrangled fish size/taxa information
fish_dw_taxa <- readRDS("data/derived_data/fish_dw_taxa.rds") %>% 
  ungroup 

fish_size_data = fish_dw_taxa %>% 
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

write_csv(fish_size_data, file = "data/derived_data/aqua-sync_data/fish_size_data.csv")
