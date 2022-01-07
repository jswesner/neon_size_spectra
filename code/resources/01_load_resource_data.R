# GPP data download script.

# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(DBI)

# Load stream names
streams = readRDS(file = "./data/derived_data/streams.rds")

# GPP products ----
## download the products

products <- neonstore::neon_products()
### Download water quality data
neonstore::neon_download("DP1.20288.001")
### Download discharge data
neonstore::neon_download("DP1.20288.001")
### Download depth data
neonstore::neon_download("DP4.00130.001")



# download the water quality data
neon_store(product = "DP1.20288.001",
  table = "waq_instantaneous-basic")

discharge <- neonstore::neon_index("DP4.00130.001")

# download the depth data
neon_download("DP4.00130.001",
  site = "BLDE", start_date = "2018-01-01",
  type = "basic", unique = TRUE
)

# Allochthonous products -----
## organic matter data
# download the canopy cover data

neonstore::neon_download("DP1.20191.001")
neon_canopy_tables <- neonstore::neon_index(product = "DP1.20191.001")

# view the currently downloaded files

neon_sed_tables <- neonstore::neon_index(product = "DP1.20194.001")
neonstore::neon_store(table = "rip_percentComposition-basic")
can_tbl <- neonstore::neon_table(table = "rip_percentComposition-basic") %>%
  dplyr::select(siteID, startDate, canopyCoverPercent) %>%
  group_by(siteID) %>%
  dplyr::summarise(cover_perc = mean(canopyCoverPercent, na.rm = TRUE))

# download any new tables
neonstore::neon_download("DP1.20194.001")

# import Sediment lab data to local database
neonstore::neon_store(table = "asc_externalLabData-basic")

# get just the organic Carbon of
sed_TOC <- neonstore::neon_table(table = "asc_externalLabData-basic") %>%
  dplyr::filter(analyte == "TOC")
sed_TOC %>%
  dplyr::select(siteID, analyteConcentration) %>%
  group_by(siteID) %>%
  dplyr::summarise(
    meanConc = mean(analyteConcentration, na.rm = TRUE),
    quant75Conc = quantile(analyteConcentration, 0.75, na.rm = TRUE),
    quant25Conc = quantile(analyteConcentration, 0.25, na.rm = TRUE)
  ) %>%
  left_join(can_tbl, by = "siteID") %>%
  na.omit() %>%
  dplyr::arrange(sort(cover_perc)) %>%
  ggplot() +
  geom_segment(aes(x = cover_perc, xend = cover_perc, y = quant25Conc, yend = quant75Conc, color = siteID)) +
  geom_point(aes(x = cover_perc, y = meanConc, color = siteID), size = 2) +
  scale_y_continuous(name = "TOC", limits = c(0, 10)) +
  scale_x_continuous(name = "Percent Canopy Cover (%)") +
  viridis::scale_color_viridis(discrete = TRUE, option = "D") +
  theme_minimal() +
  theme(
    legend.position = c(0, 1),
    legend.justification = c(0, 1),
    legend.title = element_blank()
  )
# facet_wrap(~siteID, scales = 'free_x')

# # read the external lab data in
debugonce(neonstore::neon_read)
neonstore::neon_read(
  table = "asc_externalLabData-basic", product = "DP1.20194.001",
  ext = "csv", sho
)

# # connect to sediment db
# con = neon_db()
# sed = tbl(con, "asc_externalLabData-basic-DP1.20194.001")
# dbDisconnect(con, shutdown = TRUE)

# EPILITHON ----
## Download the epilithon files
neonstore::neon_download("DP1.20166.001")
epilithon_tables = neonstore::neon_index("DP1.20166.001")

## read in Field data if needed
neonstore::neon_store(table = "alg_fieldData-basic")
epi_field_tab = neonstore::neon_table(table = "alg_fieldData-basic")

## read
epi_bio_tab = neonstore::neon_table(table = "alg_biomass-basic") %>%
  dplyr::filter(analysisType == "AFDM" & siteID %in% streams) %>%
  dplyr::select(siteID, collectDate, AFDM_g = "adjAshFreeDryMass") %>%
  dplyr::filter(AFDM_g < 1) %>%
  dplyr::mutate(Date = as.Date(collectDate, format = "%y-%m-%d HH:MM:SS"))

epi_bio_summ = epi_bio_tab %>%
  group_by(siteID, Date) %>%
  dplyr::summarise(across(AFDM_g, list(mean = ~mean(.x, na.rm = TRUE),
                                       quant2.5 = ~quantile(.x, 0.025, na.rm= TRUE),
                                       quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))

epi_bio_summ %>%
  ggplot()+
  # geom_ribbon(aes(x = Date, ymin= AFDM_g_quant2.5, ymax = AFDM_g_quant97.5))+
  geom_line(aes(x = Date, y = log(AFDM_g_mean)))+
  geom_point(aes(x = Date, y = log(AFDM_g_mean)), size = 3)+
  facet_wrap(~siteID)

# - DOM flux
#
