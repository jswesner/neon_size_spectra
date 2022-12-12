# Download Macroinvertebrate data and estimate dry weights

library(neonUtilities)
library(tidyverse)
library(lubridate)

source("code/custom-functions/est_dw.R")

source("code/custom-functions/LW_coef.R")

# load Length Weight coefficient table (used in part C below)
coeff <- read.csv("data/raw_data/macro_lw_coeffs.csv")

# need to set this as a local variable
# emailed Junker on 12/12/22
neon_token <- source("C:/Users/jfpom/Documents/Wesner/NEON documents/neon_token_source.R")

stream_sites <- readRDS("data/raw_data/streams.rds")


# 1) Download data ---------------------------------------------------------

# these are large files, and may take a while to fully download dependent on web traffic and connection speed. 

# 2022-12-12: not sure when the last time this download script was ran (maybe sometime in 2021?)
# When I ran it again today (2022-12-12) there were more samples. My best guess is that not all of the collections from 2021 were on NEON data portal yet, but they are now. There were only 1-3 more samples per site, which would make sense if the 2021 data has been updated since the last download.  

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
# 96.9% had equations (2022-12-12)

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

# 92.39% of data was not damaged (2022-12-12)

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

# Note ####
# I re ran script on Dec 12, 2022
# old number of samples = 279
# now = 324 samples
# 
# old data had > 114 thousand rows of data
# Now >134 thousand rows of data
#
# uncount() old data = 27.3 million
# uncount() new data 31.8 MILLION 

# getting warning messages for group_by() 
# maybe something has changed in dplyr?


# save objects for analysis in future scripts 2 and 3
saveRDS(MN.no.damage, "data/raw_data/macro_dw.RDS")
saveRDS(ID_key, "data/raw_data/sample_ID_key.RDS")


# Data summary ------------------------------------------------------------


# what percent of data is damaged?
message(paste(
  round(nrow(MN.no.damage) / nrow(MN.lw), 4)*100,
  "percent of individuals were not damaged"))

# 92.39% not damaged (2022-12-12)


# total number of measured individuals
length(na.omit(MN.no.damage$sizeClass))
# > 134 k individual measurements (2022-12-12)

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
# 31 million rows (2022-12-12)


