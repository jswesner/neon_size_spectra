# Download Macroinvertebrate data and estimate dry weights
library(neonUtilities)
library(tidyverse)
library(janitor)
library(lubridate)
library(tidybayes)
library(brms)
library(neonstore)
library(neonDivData)
# source("code/sandbox/estimate_macro_dw_functions.R")
source("code/custom-functions/LW_coef.R")

# load Length Weight coefficient table (used below)
coeff <- read.csv("data/raw_data/inverts/macro_lw_coeffs.csv")

# directory
Sys.setenv(NEONSTORE_HOME = paste(getwd(), 
                                  "/data",
                                  sep=""))

# download data (takes ~15 minutes) --------------------------------
#stream sites
streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU", 
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

# neon_download(product="DP1.20120.001",
#               start_date=NA,
#               end_date=NA,
#               type="basic",
#               site= NA)

# stack data
# inverts_stacked = stackFromStore(filepaths=neon_dir(),
#                                   dpID="DP1.20120.001",
#                                   package="basic",
#                                   site = streamsites)

# saveRDS(inverts_stacked, file = "data/raw_data/inverts/inverts_stacked.rds")

macro = readRDS(file = "data/raw_data/inverts/inverts_stacked.rds")

# 2) Add LW coefficients, estimate dry weights  ------------------------------------

# add length weight coefficients by taxon
MN.lw <- LW_coef(x = macro$inv_taxonomyProcessed,
                 lw_coef = coeff,
                 percent = TRUE)

# questionable measurements ####
# filter out individuals that were "damaged" and measurement was affected
# this is a flag which is added by NEON
# MN.no.damage <- MN.lw %>%
#   filter(!str_detect(sampleCondition,
#                      "measurement")) %>%
#   est_dw(fieldData = macro$inv_fieldData)


# length_weight estimates -------------------------------------------------



# est_dw <- function(x, fieldData){
# x = inv_taxonomyProcessed table from NEON data product "DP1.20120.001" with LW coefficients added using the LW_coeff() function
# fieldData = inv_fieldData table from NEON data product "DP1.20120.001"

MN.lw.x = MN.lw %>%
  filter(!str_detect(sampleCondition,
                     "measurement"))
fieldData = macro$inv_fieldData

# simplify fieldData to three columns
field = fieldData %>%
  select(siteID, sampleID, benthicArea) %>%
  distinct()
# add benthicArea column from fieldData to x. This is necessary to calculate number per m2 below

# join by siteID and sampleID
MN.no.damage.taxa <- left_join(MN.lw.x, field, by = c("siteID", "sampleID")) %>% 
  # filter(invertebrateLifeStage %in% c("larva", "pupa")) %>%
  filter(sizeClass >= 3) %>%
  mutate(dw = case_when(formula_type == 1 ~ a * sizeClass^b,
                        formula_type == 2 ~ exp(a + b * log(sizeClass)))) 

saveRDS(MN.no.damage.taxa, file = "data/derived_data/MN.no.damage.taxa.rds")

MN.no.damage <- MN.no.damage.taxa %>% 
  mutate(date = ymd(as.Date(collectDate))) %>% 
  group_by(dw, siteID, date) %>% 
  summarize(n = sum(estimatedTotalCount),
            benthicArea = sum(benthicArea),
            no_m2 = n/benthicArea)

# clean and save ---------------------------------
# filter out NA values in dw
MN.no.damage.nona <- MN.no.damage %>%
  filter(!is.na(dw), !is.na(no_m2)) %>% 
  as_tibble()

############ Data before 2020 contains duplicates according to NEON :
#https://www.neonscience.org/impact/observatory-blog/duplicate-macroinvertebrate-collection-dp120120001-records:
# This is supposed to be fixed in the 2022 release. The code below deletes duplicates anyway

# make unique group index for siteID:collectDate combo
MN.no.damage.unique <- MN.no.damage.nona %>%  
  mutate(year = year(date),
         month = month(date)) %>%
  mutate(event_id = 
           paste(siteID, year, month, sep = "_"),
         animal_type = "macroinvertebrates") %>%
  arrange(event_id) %>%
  select(-year, -month) %>% 
  clean_names()


macro_dw = MN.no.damage.unique  %>%
  group_by(dw, event_id, site_id, date, animal_type) %>% 
  summarize(n = sum(n),
            no_m2 = sum(no_m2)) %>% 
  mutate(julian = julian(date)) %>% 
  filter(site_id %in% streamsites)

saveRDS(macro_dw, file = "data/inverts_dw-allyears.rds")


macro %>% glimpse()

# taxa
macro$inv_taxonomyProcessed %>% as_tibble() %>% 
  mutate(year = year(collectDate)) %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  distinct(scientificName) %>% 
  nrow()

# number of body size measures
macro$inv_taxonomyProcessed %>% as_tibble() %>% 
  mutate(year = year(collectDate)) %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  summarize(total = sum(estimatedTotalCount, na.rm = T))

2403743 + 64940

MN.no.damage.taxa %>%
  distinct(dw, order, siteID) %>% 
  ggplot(aes(y = reorder(order, dw), x = dw)) + 
  geom_point() + 
  # scale_x_log10() +
  NULL
