# Download Macroinvertebrate data and estimate dry weights

library(neonUtilities)
library(tidyverse)
library(lubridate)

source("code/inverts/estimate_macro_dw_functions.R")

# load Length Weight coefficient table (used in part C below)
coeff <- read.csv("data/raw_data/macro_lw_coeffs.csv")
neon_token <- source("C:/Users/jfpom/Documents/Wesner/NEON documents/neon_token_source.R")
stream_sites <- readRDS("data/derived_data/streams.rds")



# 1) Download data ---------------------------------------------------------

# these are large files, and may take a while to fully download dependent on web traffic and connection speed. 

# download all macroinvertebrate colllection data from January 2016 to December 2021
macro <- loadByProduct(dpID = "DP1.20120.001",
                       site = stream_sites, 
                       startdate = "2016-01",
                       enddate = "2021-12",
                       check.size = FALSE,
                       token = neon_token$value,
                       nCores = 4)


# 2) Add LW coefficients, estimate dry weights  ------------------------------------

# add length weight coefficients by taxon
MN.lw <- LW_coef(x = macro$inv_taxonomyProcessed,
                 lw_coef = coeff,
                 percent = TRUE)

# questionable measurements ####
# filter out individuals that were "damaged" and measurement was affected
# this is a flag which is added by NEON
MN.no.damage <- MN.lw %>%
  filter(!str_detect(sampleCondition,
                     "measurement")) %>%
  est_dw(fieldData = macro$inv_fieldData)



# filter out NA values in dw
MN.no.damage <- MN.no.damage %>%
  filter(!is.na(dw), !is.na(no_m2)) 

nrow(MN.no.damage) / nrow(MN.lw)

########################################################
########################################################
# double check this!!!! ---------------------------------------------------
########################################################
########################################################


# when the data was downloaded in January 2022, some of the collections were conducted across 2 days, or at two different times on the same day. 
# For example: 
MN.no.damage %>%
  mutate(year = year(collectDate),
         month = month(collectDate)) %>%
  filter(siteID == "HOPB",
         year == 2017,
         month == 4) %>%
  select(collectDate) %>% 
  unique()
# and MAYF 2017-07-18
MN.no.damage %>%
  mutate(year = year(collectDate),
         month = month(collectDate)) %>%
  filter(siteID == "MAYF",
         year == 2017,
         month == 7) %>%
  select(collectDate) %>% 
  unique()
# We deal with this in a later script by pulling year and month from "collectDate" and using unique siteID, year, month, combinations. 


# make unique group index for siteID:collectDate combo
MN.no.damage <- MN.no.damage %>%  
  mutate(year = year(collectDate),
         month = month(collectDate)) %>%
  mutate(ID = 
           group_indices(
             ., siteID, year, month)) %>%
  arrange(ID) %>%
  select(-year, -month)


# make a key for which ID goes with whoch siteID:collectDate combo
ID_key <- MN.no.damage %>%
  select(ID, siteID, collectDate) %>%
  distinct()

# add sample year, month, to ID_key
ID_key <- ID_key %>%
  mutate(year = year(collectDate),
         month = month(collectDate))



# save objects for analysis in script 2 and 3
saveRDS(MN.no.damage, "data/derived_data/macro_dw.RDS")
saveRDS(ID_key, "data/derived_data/sample_ID_key.RDS")


# Data summary ------------------------------------------------------------


# what percent of data is damaged?
message(paste(
  round(nrow(MN.no.damage) / nrow(MN.lw), 4)*100,
  "percent of individuals were not damaged"))

# 90% not damaged


# total number of measured individuals
length(na.omit(MN.no.damage$sizeClass))
# > 82 k individual measurements

# clean up MN.no.damage object
MN.no.damage <- MN.no.damage %>%
  # calculate total count / per m2 for each size class
  group_by(siteID, collectDate, dw) %>%
  summarise(count = sum(no_m2)) %>%
  ungroup() %>%
  # remove counts that are NA
  na.omit() %>%
  # duplicate number of rows based on count
  #i.e. if count = 10, duplicate that row 10 times
  uncount(count)

nrow(MN.no.damage)
# 18 million rows


