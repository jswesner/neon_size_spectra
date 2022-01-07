# INverterate data download script.

# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(DBI)

# Load stream names
streams = readRDS(file = "./data/derived_data/streams.rds")

products <- neonstore::neon_products()

### Download invertebrate data
library(codyn)
neonstore::neon_download("DP1.20120.001")
neonstore::neon_store("inv_fieldData")
neonstore::neon_store("inv_taxonomyProcessed")

### Check out field data
invert_fieldData = neonstore::neon_table(table = "inv_fieldData")

### Get annual density estimates
invert_tab = neonstore::neon_table(table = "inv_taxonomyProcessed") %>%
  dplyr::filter(siteID %in% streams) #%>%
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

invert_tab %>%
  group_by(siteID, year) %>%
  dplyr::summarise(total = sum(relAbun, na.rm = TRUE)) %>%
  ungroup %>% dplyr::select(total) %>% unname %>% unlist

invert_tab %>%
  ggplot() +
  geom_line(aes(x = rank, y = log(relAbun), group = year, color = year)) +
  geom_point(aes(x = rank, y = log(relAbun), group = year, color = year)) +
  theme_minimal()+
  facet_wrap(~siteID)

invert_rankShift = invert_tab %>%
  group_by(siteID, year) %>%
  junkR::named_group_split(siteID, year) %>%
  purrr::map(~codyn::mean_rank_shift)
