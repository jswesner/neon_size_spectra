# Data for aqua sync project

# This script organizes data and creates the "site_data" csv to be put into the excel data template 
# format for the aquasync led by Dr. Dan Perkins. 

# written by Justin Pomeranz, March 2024

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
# `scientificName` = `taxon` in size data
# dat %>%
#   select(scientificName, taxonRank:specificEpithet) %>%
#   unique() %>%
#   rename(taxon = scientificName,) %>%
#   write_csv("data/derived_data/aqua-sync_data/invertebrate-taxonomy.csv")


# From Wesner ####
macro <- readRDS("data/raw_data/macro.rds") # to get collection info

# get sampler type for each sampleID

sampler = macro$inv_fieldData %>%
  distinct(sampleID, samplerType, habitatType, substratumSizeClass) %>%
  rename(method = samplerType)


# join sample type to dat

sampler_dat <- dat %>%
  left_join(sampler) %>%
  filter(!is.na(method)) %>%
  mutate(date = ymd(as.Date(collectDate)))

# copied from 00_make_macros_data.R script
size_dat <- sampler_dat %>% 
  group_by(dw, siteID, sampleID, collectDate,
           scientificName, sizeClass, benthicArea,
           method, habitatType, substratumSizeClass) %>% 
  summarize(
    # total number of individuals from sample
    # this is based on the % subsample in the original processing
    n = sum(estimatedTotalCount, na.rm = TRUE))%>%
  filter(!is.na(dw)) %>%
  mutate(organism_group = "Invertebrate") 

# select columns necessary for formatting
size_dat <- size_dat %>%
  ungroup() %>%
  select(siteID, sampleID, method,
         collectDate, benthicArea,
         organism_group, scientificName,
         dw, sizeClass, n)
names(size_dat)


# rename existing columns and add new ones to match aquaSync format
short_dat <- size_dat %>%
  mutate(date = as_date(collectDate),
         year = year(date),
         month = month(date),
         body_weight_units = "mg",
         body_length_units = "mm") %>%
  tidyr::unite("site_date", c(siteID, date), remove = FALSE) %>%
  rename(sampling_area = benthicArea,
         taxon = scientificName, 
         body_mass = dw,
         body_length = sizeClass,
         site = siteID) %>%
  select(-collectDate)

# extract info on site-date combo
# use this to construct site metadata
site_date <- short_dat %>%
  select(site, site_date, year, month, method) %>%
  unique() %>%
  rename(sampling_methodology = method)

# make a vector of site_date which has fish collections
fish <- read_csv("data/derived_data/aqua-sync_data/fish_size_data.csv")

fish_dates <- fish %>% pull(site) %>% unique()

site_date <- site_date %>%
  filter(site_date %in% fish_dates)

# combine different years, months, sampling methods by site
# end up with one table with one row per site
(site_year <- site_date %>%
    select(site, year) %>%
    distinct() %>%
    group_by(site) %>%
    mutate(year = paste(
      sort(year), collapse =  ", ")) %>%
    distinct())

(site_month <- site_date %>%
    select(site, month) %>%
    distinct() %>%
    group_by(site) %>%
    mutate(month = paste(
      sort(month), collapse =  ", "))%>%
    distinct())

(site_method <-  site_date %>%
    select(site, sampling_methodology)%>%
    distinct() %>%
    group_by(site) %>%
    mutate(
      sampling_methodology = paste(
        sort(sampling_methodology),
        collapse = ", "))%>%
    distinct() %>%
    mutate(sampling_methodology = paste(sampling_methodology,  "electrofisher", sep = ", ")))

(site_date_method <- left_join(site_year, site_month) %>%
    left_join(site_method))

# site lat long info
site_meta <- read.csv("data/field_data.csv")
site_date <- left_join(site_date_method, site_meta, by = join_by(site == Site.ID)) %>%
  rename(geographical_position_specification = State,
         geographical_latitude = Latitude,
         geographical_longitude = Longitude,
         geographical_altitude = Elevation,
         sampling_year = year,
         sampling_month = month,
         landuse_type = Dominant.NLCD.Classes) %>%
  mutate(geographical_position = "USA",
         organism_groups = "Invertebrates + Fish",
         sampling_season = "spring, summer",
         trophic_levels = ">2",
         data_owner = "NEON: NAtional Ecological Observatory Network",
         data_contributors = "Pomeranz, Wesner, Junker, Gjoni",
         multiple_sampling = "2 per year",
         region = "North America") %>%
  select(data_owner, data_contributors,
         site, geographical_position,
         geographical_position_specification,
         geographical_latitude,
         geographical_longitude,
         geographical_altitude, 
         sampling_year, sampling_month,
         sampling_season, 
         multiple_sampling,
         region, landuse_type,
         organism_groups, 
         trophic_levels, sampling_methodology)



# save as csv
write_csv(site_date, "data/derived_data/aqua-sync_data/site_data.csv")
