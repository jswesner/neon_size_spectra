# Data for aqua sync project

# This file organizes the full data set to be in the template
# format for the aquasync project on assessing changes to size 
# spectra across landuse gradients, led by Dr. Dan Perkins. 

# written by Justin Pomeranz, October 2023

library(tidyverse)

#stream sites
streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU", 
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

# macroinvertebrate raw format
dat <- readRDS("data/derived_data/MN.no.damage.taxa.rds")

# filter out just stream data
dat <- dat %>%
  filter(siteID %in% streamsites)

names(dat)

# make a table of taxonomic information
dat %>%
  select(scientificName, taxonRank:specificEpithet) %>%
  unique() %>%
  write_csv("data/derived_data/aqua-sync_data/invertebrate-taxonomy.csv")

# template column names:
# site = site name, code
# sampling_method = Surber, kick, etc
# sample = unique sample for that sampling method
# sampling_area = area of each sample in m2
# organism_group = invertebrates or fish
# taxon = taxonomic ID, lowest rank possible
# body_mass = 
# body_length = 
# body_weight_units = 
# body_length_units = 
# count = number of individuals
# multiplier = 1/sample area, multiplier needed for count per m2 for that sample

# copied from 00_make_macros_data.R script
size_dat <- dat %>% 
  group_by(dw, siteID, sampleID, collectDate,
           scientificName, sizeClass, benthicArea) %>% 
  summarize(
    # total number of individuals from sample
    # this is based on the % subsample in the original processing
    n = sum(estimatedTotalCount, na.rm = TRUE))%>%
  filter(!is.na(dw)) %>%
  mutate(organism_group = "macroinvertebrate") 

# select columns necessary for formatting
size_dat <- size_dat %>%
  ungroup() %>%
  select(siteID, sampleID, collectDate, benthicArea,
         organism_group, scientificName, dw, sizeClass, n)

# separate sampleID column to get sample number and 
# sample method
# sampleID column does not have uniform number of characters
# below separate() commands are a bit convoluted, but they work for this data set
# double check if doing this again on future data
sample_method <- size_dat %>%
  separate(sampleID, into = c("sampleID", "sample"), -1 ) %>%
  separate(sampleID, into = c("beginning", "end"), 9) %>%
  separate(end, into = c("num", "sampling_method")) %>%
  select(-beginning, -num)

names(sample_method)

# rename existing columns and add new ones to match aquaSync format
short_dat <- sample_method %>%
  mutate(date = as_date(collectDate),
         body_weight_units = "mg",
         body_length_units = "mm") %>%
  tidyr::unite("site", c(siteID, date), remove = FALSE) %>%
  rename(sampling_area = benthicArea,
         taxon = scientificName,
         body_mass = dw,
         body_length = sizeClass) %>%
  select(-collectDate)

# extract info on site-date combo
# use this to construct site metadate
site_date <- short_dat %>%
  select(site, siteID, date) %>%
  unique() %>%
  mutate(sampling_year = year(date),
         sampling_month = month(date))
# save as csv
write_csv(site_date, "data/derived_data/aqua-sync_data/site_date.csv")


# extend data set based on count of body suze classes in "n" column
# this makes each row of data be for 1 individual
size_dat_formatted <- short_dat %>%
  uncount(n, ) %>%
  mutate(count = 1,
         multiplier = 1 / sampling_area)

View(size_dat_formatted)

write_csv(size_dat_formatted, "data/derived_data/aqua-sync_data/invertebrate-size-data.csv")
