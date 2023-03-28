rm(list = ls())
source("./code/resources/01_load-packages.R")
here::i_am("code/resources/03_CPOM-estimates.R")

## read in and summarise data for saving to derived_data folder as cpom_means.rds
read.csv(file = here::here("data/raw_data/resources/allochthonous/NEON-stream-CPOM - Sheet1.csv")) %>% 
  # rename split sample repIDs
  dplyr::mutate(repID = gsub("R\\d{1}(\\.\\d{1})","", repID)) %>%
  # group by site and repID
  group_by(siteCode, repID) %>%
  # sum split samples
  dplyr::summarise(OMmass_mg_corr = sum(OMmass_mg_corr, na.rm = TRUE)) %>% 
  # regroup by just site to get mean and sd
  group_by(siteCode) %>% 
  dplyr::summarise(mean = mean(OMmass_mg_corr, na.rm = TRUE),
                   sd = sd(OMmass_mg_corr, na.rm = TRUE)) %>% 
  # add in MAYF which we have no samples for
  bind_rows(., data.frame(siteCode = "MAYF",
                          mean = NA,
                          sd = NA)) %>% 
  # remove the sample that has no known site ID for now
  dplyr::filter(!is.na(siteCode)) %>% 
  # rename to be same as gpp_means.rds
  dplyr::rename(siteID = "siteCode") %>% 
  # save file to derived_data
  saveRDS(., file = here::here("./data/derived_data/cpom_means.rds"))
