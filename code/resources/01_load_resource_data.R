# GPP data download script.
'%ni%' = Negate('%in%')
# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(neonUtilities)
library(neonstore)
library(tidyverse)
library(DBI)
source("./code/update_data_products.R")
# Load stream names
streams = readRDS(file = "./data/derived_data/streams.rds")
latlong = read_csv(file = "./data/site_latlong.csv")
# Resource code for diferent resource types ----
# GPP products ----
## download the products
update_data_products(products = "resources")
# the neonstore db has been really slow and 
# taking a lot of memory
streams_mod = streams[streams %ni% c("POSE")]
for(i in seq_along(streams_mod)){
  
  fileName = paste0("./ignore/site-gpp-data/",streams_mod[i],"_30min_DO.rds")
  # load, stack, munge, and save DO files
  waq_df = neonstore::neon_read(table = 'waq_instantaneous-basic',
                                product = 'DP1.20288.001',
                                site = streams_mod[i],
                                altrep = FALSE
  ) %>%
    dplyr::select(siteID, startDateTime, matches("dissolvedOxygen.*|.*DOSat")) %>%
    dplyr::mutate(timePeriod = cut(startDateTime, breaks = "15 min")) %>%
    group_by(timePeriod) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  rm(waq_df);gc()
  
}

for(i in seq_along(streams)){
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_30min_depthZ.rds")
  # load, stack, munge, and save depth files
  neonstore::neon_read(product = "DP1.20016.001",
                                table = "EOS_30_min-basic",
                                site = streams[i],
                                altrep = FALSE
  ) %>%
    dplyr::select(siteID, startDateTime, surfacewaterElevMean, sWatElevFinalQF) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  gc()
}

for(i in seq_along(streams)){
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_1hr_dischargeQ.rds")
  # load, stack, munge, and save depth files
  neonstore::neon_read(product = "DP4.00130.001",
                       table = "csd_continuousDischarge-basic",
                       site = streams[i],
                       altrep = FALSE
  ) %>%
    dplyr::select(siteID, endDate, calibratedPressure, equivalentStage, maxpostDischarge) %>%
    dplyr::mutate(timePeriod = cut(endDate, breaks = "1 hour")) %>%
    group_by(timePeriod, endDate) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  gc()
}

streams_mod = streams[streams %ni% c("POSE")]

for(i in seq_along(streams_mod[4:length(streams_mod)])){
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_30min_airPressure.rds")
  # load, stack, munge, and save depth files
  neonstore::neon_read(product = "DP1.00004.001",
                       table = "BP_30min-basic",
                       site = streams_mod[i],
                       altrep = FALSE
  ) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  gc()
}


products <- neonstore::neon_products()
neonstore::neon_download("DP1.00024.001",
                         site = streams)

neonstore::neon_index("DP1.00024.001") %>% View()

x = neonstore::neon_read(product = "DP1.00004.001",
                     table = "BP_30min-basic",
                     site = "BLDE",
                     altrep = FALSE
)
### Check index of  water quality data
 ## These data are too large to subset locally, must subset on Larry
x = neonstore::neon_index("DP1.00004.001")
### Index discharge data
 ## These data are too large to subset locally must subset on Larry
neonstore::neon_index("DP4.00130.001") -> Q
### Index depth data
neonstore::neon_index(product = "DP1.20016.001",
                      table = "EOS_30_min-basic",
                      )
### Index Temp data
 ## Data have been subset to each stream
 neonstore::neon_index("DP1.20053.001")
### Index rearation data
 ## currently need to assess if this is needed
View(neonstore::neon_index("DP1.20190.001"))

# download the water quality data
neon_store(table = 'waq_instantaneous-basic', site = "BLDE")
for(i in 1:length(streams)){
  x = neonstore::neon_table(table = 'waq_instantaneous-basic', site = streams[i])
  saveRDS(x, file = paste0("./ignore/site-gpp-data/",streams[i],"_waq.rds"))
}

for(i in 1:length(streams)){
  x = neonstore::neon_table(table = "TSW_30min-basic", site = streams[i])
  saveRDS(x, file = paste0("./ignore/site-gpp-data/",streams[i],"_30min_temp.rds"))
}

for(i in 1:length(streams)){
  x = neonstore::neon_table(table = "", site = streams[i])
  saveRDS(x, file = paste0("./ignore/site-gpp-data/",streams[i],"_continuousZ.rds"))
}



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
## Check for updates if needed, Don't need to do this 
neonstore::neon_download("DP1.20166.001")
## Check the tables already downloaded
epilithon_tables = neonstore::neon_index("DP1.20166.001")

## read in Field data if needed
neonstore::neon_store(table = "alg_fieldData-basic")
epi_field_tab = neonstore::neon_table(table = "alg_fieldData-basic")

## read in and clean epilithon AFDM
epi_bio_tab = neonstore::neon_table(table = "alg_biomass-basic") %>%
  dplyr::filter(analysisType == "AFDM" & siteID %in% streams) %>%
  dplyr::select(siteID, collectDate, AFDM_g = "adjAshFreeDryMass") %>%
  dplyr::filter(AFDM_g < 1) %>%
  dplyr::mutate(AFDM_mg = AFDM_g*1000,
                Date = as.Date(collectDate, format = "%y-%m-%d HH:MM:SS"))

epi_bio_summ = epi_bio_tab %>%
  group_by(siteID, Date) %>%
  dplyr::summarise(across(AFDM_mg, list(mean = ~mean(.x, na.rm = TRUE),
                                       quant2.5 = ~quantile(.x, 0.025, na.rm= TRUE),
                                       quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))

epi_bio_summ %>%
  ggplot()+
  # geom_ribbon(aes(x = Date, ymin= AFDM_mg_quant2.5, ymax = AFDM_mg_quant97.5))+
  geom_line(aes(x = Date, y = log(AFDM_mg_mean)))+
  geom_point(aes(x = Date, y = log(AFDM_mg_mean)), size = 3)+
  theme_minimal()+
  facet_wrap(~siteID)

# - DOM flux
#
