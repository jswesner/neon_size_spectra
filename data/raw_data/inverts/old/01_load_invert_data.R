# Not sure what this is
# Jim - is this the auto-updater for the neon data you were talking about??


# INvertebrate data download script.

# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(DBI)

# function that wrap neonstore functions to update data products as needed
source("./code/update_data_products.R")

### Download invertebrate data
update_data_products(products = 'inverts')
# Load stream names
streams = readRDS(file = "./data/derived_data/streams.rds")



### Check out field data
invert_fieldData = neonstore::neon_table(table = "inv_fieldData")

### Get annual density estimates
invert_tab = neonstore::neon_table(table = "inv_taxonomyProcessed") %>%
  dplyr::filter(siteID %in% streams) %>%
  dplyr::select(siteID, collectDate, acceptedTaxonID, scientificName, estimatedTotalCount) %>%
  dplyr::mutate(date = as.Date(collectDate, format = "%y-%m-%d HH:MM:SS"),
                year = as.character(lubridate::year(date))) %>%
  group_by(siteID, year, acceptedTaxonID, scientificName) %>%
  dplyr::summarise(estimatedTotalCount = sum(estimatedTotalCount, na.rm = TRUE)) %>%
  ungroup %>% group_by(siteID, year) %>%
  dplyr::mutate(total = sum(estimatedTotalCount, na.rm = TRUE)) %>%
  ungroup %>% group_by(siteID, year, acceptedTaxonID, scientificName) %>%
  dplyr::mutate(relAbun = estimatedTotalCount/total) %>%
  dplyr::select(-total) %>%
  ungroup %>% group_by(siteID, year) %>%
  dplyr::mutate(rank = dense_rank(desc(relAbun)))

### code below is preliminary. Muting and keeping here until a better home is found ----


# invert_tab %>%
#   group_by(siteID, year) %>%
#   dplyr::summarise(total = sum(relAbun, na.rm = TRUE)) %>%
#   ungroup %>% dplyr::select(total) %>% unname %>% unlist
# 
# invert_tab %>%
#   ggplot() +
#   geom_line(aes(x = rank, y = log(relAbun), group = year, color = year)) +
#   geom_point(aes(x = rank, y = log(relAbun), group = year, color = year)) +
#   theme_minimal()+
#   facet_wrap(~siteID)
# 
# invert_yearList = invert_tab %>%
#   dplyr::mutate(year = as.numeric(year)) %>%
#   dplyr::filter(relAbun > 0) %>%
#   na.omit %>%
#   junkR::named_group_split(siteID) 
# 
# library(codyn)
# debugonce(codyn::rank_shift)
# codyn::rank_shift(invert_yearList[[22]], time.var = 'year', species.var = 'acceptedTaxonID', abundance.var = 'estimatedTotalCount')
# 
# invert_rankShift = invert_yearList[c(1:21,23:24)] %>%
#   purrr::map(~codyn::rank_shift(.x, time.var = 'year', species.var = 'acceptedTaxonID', abundance.var = 'estimatedTotalCount'))
# 
# invert_rankSumm = invert_rankShift %>%
#   purrr::map(~mean(.x$MRS)) %>% bind_rows(.id = siteID) %>%
#   pivot_longer(everything(),names_to = 'siteID', values_to = 'MRS_mean')
