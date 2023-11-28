# script for Valente and Saito collaboration

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

dat_select <- dat %>%
  select(siteID, collectDate, uid, sizeClass,
         class, order, suborder, family,
         genus, invertebrateLifeStage,
         formula, a, b, dw,
         estimatedTotalCount, benthicArea) %>%
  filter(!is.na(estimatedTotalCount))

dat_select %>%
  mutate(collectDate = as_date(collectDate)) %>%
  unite("sitio_area_ID", c(siteID, collectDate), remove = TRUE) %>%
  rename(individuo_ID = uid,
         Larvae_or_adult = invertebrateLifeStage,
         BL = sizeClass,
         Equation = formula,
         DM_mg = dw,
         Class = class, 
         Order = order, 
         Suborder = suborder, 
         Family = family,
         Genus = genus) %>%
  mutate(Subamostra_ID = NA,
         HW = NA, 
         BW = NA, 
         TBL = NA, 
         percent_ash = NA,
         AFDM = NA, 
         Source = NA) %>%
  select(sitio_area_ID, Subamostra_ID,
         individuo_ID, HW, BL, BW, TBL,
         Class, Order, Suborder, Family,
         Genus, Larvae_or_adult, Equation,
         Source, a, b, percent_ash, DM_mg,
         estimatedTotalCount, benthicArea) %>%
  write_csv("data/derived_data/Valente-Saito/biological-data.csv")

# column names: biological data
# sitio_area_ID - site id?
# Subamostra_ID - Not sure
# individuo_ID - ID code for each individual
# HW - head width, NA
# BL - body length
# BW - body width, NA
# TBL - total body length, NA
# Class - taxonomic...
# Order
# Suborder
# Family
# Genus
# Larvae_or_adult - not recorded
# Equation - 
# Source - 
# a
# b
# % ash - NA
# AFDM - NA
# DM_mg

site_data <- read_csv("data/derived_data/aqua-sync_data/site_data.csv")
names(site_data)

site_meta <- read.csv("data/field_data.csv")

site_data %>%
  rename(sitio_area_ID = site,
         Lat = geographical_latitude,
         Long = geographical_longitude) %>%
  mutate(subsample_ID = NA) %>%
  select(sitio_area_ID, 
         subsample_ID,
         Lat, 
         Long) %>%
  write_csv("data/derived_data/Valente-Saito/ecological-data.csv")

# Ecological meta data
# site_area_ID
# subsample_ID
# Lat
# Long
# Include below other ecological variables present in the dataset
# Var1
# Var2
# Var3
# Var4