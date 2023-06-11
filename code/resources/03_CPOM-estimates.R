rm(list = ls())
library(tidyverse)
library(here)
# source("./code/resources/01_load-packages.R")
here::i_am("code/resources/03_CPOM-estimates.R")

# load macro data
macro = readRDS(file = "data/raw_data/inverts/inverts_stacked.rds")
# get sample areas
sample_areas = macro$inv_fieldData %>% as_tibble %>% distinct(samplerType, benthicArea) 


## read in and summarise data for saving to derived_data folder as cpom_means.rds
read.csv(file = here::here("data/raw_data/resources/allochthonous/NEON-stream-CPOM - Sheet1.csv")) %>% 
  # rename split sample repIDs
  dplyr::mutate(repID = gsub("R\\d{1}(\\.\\d{1})","", repID)) %>% 
  mutate(samplerType = case_when(grepl("icknet", label.notes) ~ "modifiedKicknet",
                                 grepl("SURBER", label.notes) ~ "surber",
                                 grepl("core", label.notes) ~ "core",
                                 grepl("Hess", label.notes) ~ "hess")) %>%
  left_join(sample_areas) %>% 
  mutate(om_mgm2 = OMmass_mg_corr/benthicArea) %>%
  as_tibble() %>%
  # group by site and repID
  group_by(siteCode, repID) %>%
  # sum split samples
  dplyr::summarise(OMmass_g_corr = sum(om_mgm2/1000, na.rm = TRUE)) %>% 
  # regroup by just site to get mean and sd
  group_by(siteCode) %>% 
  dplyr::summarise(mean = mean(OMmass_g_corr, na.rm = TRUE),
                   sd = sd(OMmass_g_corr, na.rm = TRUE)) %>%
  mutate(om_units = "gAFDM_m2") %>% 
  # add in MAYF which we have no samples for
  bind_rows(., data.frame(siteCode = "MAYF",
                          mean = NA,
                          sd = NA, 
                          units = NA)) %>% 
  # remove the sample that has no known site ID for now
  dplyr::filter(!is.na(siteCode)) %>% 
  # rename to be same as gpp_means.rds
  dplyr::rename(siteID = "siteCode") %>% 
  # save file to derived_data
  saveRDS(., file = here::here("./data/derived_data/cpom_means.rds"))


read.csv(file = here::here("data/raw_data/resources/allochthonous/NEON-stream-CPOM - Sheet1.csv")) %>% 
  # rename split sample repIDs
  dplyr::mutate(repID = gsub("R\\d{1}(\\.\\d{1})","", repID)) %>% 
  mutate(samplerType = case_when(grepl("icknet", label.notes) ~ "modifiedKicknet",
                                 grepl("SURBER", label.notes) ~ "surber",
                                 grepl("core", label.notes) ~ "core",
                                 grepl("Hess", label.notes) ~ "hess")) %>%
  left_join(sample_areas) %>% 
  mutate(om_mgm2 = OMmass_mg_corr/benthicArea) %>%
  as_tibble() %>%
  # group by site and repID
  group_by(siteCode, repID) %>%
  # sum split samples
  dplyr::summarise(OMmass_g_corr = sum(om_mgm2/1000, na.rm = TRUE)) %>% 
  # regroup by just site to get mean and sd
  group_by(siteCode) %>% 
  dplyr::summarise(mean = mean(OMmass_g_corr, na.rm = TRUE),
                   sd = sd(OMmass_g_corr, na.rm = TRUE)) %>%
  mutate(om_units = "gAFDM_m2") %>% 
  # add in MAYF which we have no samples for
  bind_rows(., data.frame(siteCode = "MAYF",
                          mean = NA,
                          sd = NA, 
                          units = NA)) %>% 
  # remove the sample that has no known site ID for now
  dplyr::filter(!is.na(siteCode)) %>% 
  # rename to be same as gpp_means.rds
  dplyr::rename(siteID = "siteCode") %>% 
  write_csv(file = "data/derived_data/cpom_means.csv")
