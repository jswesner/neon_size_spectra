# GPP data download script.
'%ni%' = Negate('%in%')
# Load packages and helper functions --------
# devtools::install_github("cboettig/neonstore")
library(tidyverse)
# library(magrittr)
# library(plyr)
# library(dplyr)
library(neonUtilities)
library(neonstore)
library(DBI)
source("./code/update_data_products.R")
# Load stream names
streams = readRDS(file = "./data/derived_data/streams.rds")
latlong = read_csv(file = "./data/site_latlong.csv")

# Resource code for different resource types ----
# GPP products ----
## download the products
update_data_products(products = "resources")
# the neonstore db has been really slow and 
# taking a lot of memory
# streams_mod = streams[streams %ni% c("POSE")]
for(i in seq_along(streams)){
  
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_DO.rds")
  # load, stack, munge, and save DO files
  tictoc::tic();neonstore::neon_read(table = 'waq_instantaneous-basic',
                                product = 'DP1.20288.001',
                                site = streams[i],
                                altrep = FALSE
  ) %>%
    dplyr::select(siteID, startDateTime, matches("dissolvedOxygen.*|.*DOSat"), horizontalPosition) %>%
    junkR::named_group_split(horizontalPosition) %>%
    # dplyr::mutate(timePeriod = cut(startDateTime, breaks = "15 min")) %>%
    furrr::future_map(~.x %>% 
                 group_by(startDateTime) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))) %>%
    saveRDS(., file = fileName);tictoc::toc()
  # clean up the mess to clear memory
  # rm(waq_df);
  gc()
  
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
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_dischargeQ.rds")
  # load, stack, munge, and save depth files
  neonstore::neon_read(product = "DP4.00130.001",
                       table = "csd_continuousDischarge-basic",
                       site = streams[i],
                       altrep = FALSE
  ) %>%
    dplyr::select(siteID, startDateTime = "endDate", calibratedPressure, equivalentStage, maxpostDischarge) %>%
    # dplyr::mutate(timePeriod = cut(endDate, breaks = "1 hour")) %>%
    group_by(startDateTime) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, "%Y-%m-%d %H:%M:%S")) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  gc()
}

for(i in seq_along(streams)){
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_30min_airPressure.rds")
  # load, stack, munge, and save depth files
  neonstore::neon_read(product = "DP1.00004.001",
                       table = "BP_30min-basic",
                       site = streams[i],
                       altrep = FALSE
  ) %>%
    saveRDS(., file = fileName)
  # clean up the mess to clear memory
  gc()
}

for(i in seq_along(streams)){
  
  fileName = paste0("./ignore/site-gpp-data/",streams[i],"_30min_temp.rds")
  # load, stack, munge, and save DO files
  tictoc::tic();neonstore::neon_read(table = 'TSW_30min-basic',
                                     product = 'DP1.20053.001',
                                     site = streams[i],
                                     altrep = FALSE
  ) %>%
    dplyr::select(siteID, startDateTime, surfWaterTempMean, horizontalPosition) %>%
    junkR::named_group_split(horizontalPosition) %>%
    # dplyr::mutate(timePeriod = cut(startDateTime, breaks = "15 min")) %>%
    furrr::future_map(~.x %>% 
                        group_by(startDateTime) %>%
                        dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))) %>%
    saveRDS(., file = fileName);tictoc::toc()
  # clean up the mess to clear memory
  # rm(waq_df);
  gc()
  
}

for(i in seq_along(streams)){
  
  tictoc::tic();reafileName = paste0('./ignore/site-gpp-data/',streams[i],'_reaeration.rds')
  reaInputList <- neonUtilities::loadByProduct(dpID = 'DP1.20190.001', 
                                               site = streams[i],
                                               check.size = FALSE)
  
  rea_backgroundFieldCondDataIn <- reaInputList$rea_backgroundFieldCondData
  rea_backgroundFieldSaltDataIn <- reaInputList$rea_backgroundFieldSaltData
  rea_fieldDataIn <- reaInputList$rea_fieldData
  rea_plateauMeasurementFieldDataIn <- reaInputList$rea_plateauMeasurementFieldData
  rea_plateauSampleFieldDataIn <- reaInputList$rea_plateauSampleFieldData
  rea_externalLabDataSaltIn <- reaInputList$rea_externalLabDataSalt
  rea_externalLabDataGasIn <- reaInputList$rea_externalLabDataGas
  rea_widthFieldDataIn <- reaInputList$rea_widthFieldData
  
  Sys.sleep(time = 100)
  qInputList <- neonUtilities::loadByProduct(dpID = 'DP1.20048.001', 
                                             site = streams[i], 
                                             check.size = FALSE)
  
  dsc_fieldDataIn <- qInputList$dsc_fieldData
  dsc_individualFieldDataIn <- qInputList$dsc_individualFieldData
  dsc_fieldDataADCPIn <- qInputList$dsc_fieldDataADCP
  
  Sys.sleep(time = 100)
  # Download Sensor Data
  sensorData <- neonUtilities::loadByProduct(dpID = 'DP1.20288.001', 
                                             site = streams[i],
                                             check.size = FALSE)
  
  waq_instantaneousIn <- sensorData$waq_instantaneous
  
  reaFormatted <- reaRate::def.format.reaeration(
    rea_backgroundFieldCondData = rea_backgroundFieldCondDataIn,
    rea_backgroundFieldSaltData = rea_backgroundFieldSaltDataIn,
    rea_fieldData = rea_fieldDataIn,
    rea_plateauMeasurementFieldData = rea_plateauMeasurementFieldDataIn,
    rea_plateauSampleFieldData = rea_plateauSampleFieldDataIn,
    rea_externalLabDataSalt = rea_externalLabDataSaltIn,
    rea_externalLabDataGas = rea_externalLabDataGasIn,
    rea_widthFieldData = rea_widthFieldDataIn,
    dsc_fieldData = dsc_fieldDataIn,
    dsc_individualFieldData = dsc_individualFieldDataIn,
    dsc_fieldDataADCP = dsc_fieldDataADCPIn,
    waq_instantaneous = waq_instantaneousIn)
  
  saveRDS(list("reaFormatted" = reaFormatted,
               "qInputList" = qInputList,
               "reaInputList" = reaInputList), reafileName)
  rm("reaFormatted","reaInputList","waq_instantaneousIn","sensorData",
       "dsc_fieldDataIn", "dsc_individualFieldDataIn", "dsc_fieldDataADCPIn",
       "rea_backgroundFieldCondDataIn", "rea_backgroundFieldSaltDataIn",
       "rea_fieldDataIn", "rea_plateauMeasurementFieldDataIn",
       "rea_plateauSampleFieldDataIn", "rea_externalLabDataSaltIn",
       "rea_externalLabDataGasIn", "rea_widthFieldDataIn","qInputList");gc();tictoc::toc()
}
# rea_backgroundFieldCondData-basic
 # products <- neonstore::neon_products()
#  neonstore::neon_download(product = "DP4.00130.001",
#                          site = streams)
# # 
# neonstore::neon_index("DP1.20190.001") %>% View()
# 
# x = neonstore::neon_read(product = "DP1.00004.001",
#                      table = "BP_30min-basic",
#                      site = "BLDE",
#                      altrep = FALSE
# )
# ### Check index of  water quality data
#  ## These data are too large to subset locally, must subset on Larry
# x = neonstore::neon_index("DP1.00004.001")
# ### Index discharge data
#  ## These data are too large to subset locally must subset on Larry
# neonstore::neon_index("DP4.00130.001") -> Q
# ### Index depth data
# neonstore::neon_index(product = "DP1.20016.001",
#                       table = "EOS_30_min-basic",
#                       )
# ### Index Temp data
#  ## Data have been subset to each stream
#  neonstore::neon_index("DP1.20053.001")
# ### Index rearation data
#  ## currently need to assess if this is needed
# View(neonstore::neon_index("DP1.20190.001"))
# 
# # download the water quality data
# neon_store(table = 'waq_instantaneous-basic', site = "BLDE")
# for(i in 1:length(streams)){
#   x = neonstore::neon_table(table = 'waq_instantaneous-basic', site = streams[i])
#   saveRDS(x, file = paste0("./ignore/site-gpp-data/",streams[i],"_waq.rds"))
# }
# 
# for(i in 1:length(streams)){
#   x = neonstore::neon_table(table = "TSW_30min-basic", site = streams[i])
#   saveRDS(x, file = paste0("./ignore/site-gpp-data/",streams[i],"_30min_temp.rds"))
# }
# 
# for(i in 1:length(streams)){
#   x = neonstore::neon_table(table = "", site = streams[i])
#   saveRDS(x, file = paste0("./ignore/site-gpp-data/",streams[i],"_continuousZ.rds"))
# }

# Allochthonous products -----
## organic matter data
# download the canopy cover data
# 
# neonstore::neon_download("DP1.20191.001")
# neon_canopy_tables <- neonstore::neon_index(product = "DP1.20191.001")

# view the currently downloaded files
# 
# neon_sed_tables <- neonstore::neon_index(product = "DP1.20194.001")
# neonstore::neon_store(table = "rip_percentComposition-basic")
# can_tbl <- neonstore::neon_table(table = "rip_percentComposition-basic") %>%
#   dplyr::select(siteID, startDate, canopyCoverPercent) %>%
#   group_by(siteID) %>%
#   dplyr::summarise(cover_perc = mean(canopyCoverPercent, na.rm = TRUE))

# download any new tables
# neonstore::neon_download("DP1.20194.001")

# import Sediment lab data to local database
# neonstore::neon_store(table = "asc_externalLabData-basic")

# get just the organic Carbon of
# sed_TOC <- neonstore::neon_table(table = "asc_externalLabData-basic") %>%
#   dplyr::filter(analyte == "TOC")
# sed_TOC %>%
#   dplyr::select(siteID, analyteConcentration) %>%
#   group_by(siteID) %>%
#   dplyr::summarise(
#     meanConc = mean(analyteConcentration, na.rm = TRUE),
#     quant75Conc = quantile(analyteConcentration, 0.75, na.rm = TRUE),
#     quant25Conc = quantile(analyteConcentration, 0.25, na.rm = TRUE)
#   ) %>%
#   left_join(can_tbl, by = "siteID") %>%
#   na.omit() %>%
#   dplyr::arrange(sort(cover_perc)) %>%
#   ggplot() +
#   geom_segment(aes(x = cover_perc, xend = cover_perc, y = quant25Conc, yend = quant75Conc, color = siteID)) +
#   geom_point(aes(x = cover_perc, y = meanConc, color = siteID), size = 2) +
#   scale_y_continuous(name = "TOC", limits = c(0, 10)) +
#   scale_x_continuous(name = "Percent Canopy Cover (%)") +
#   viridis::scale_color_viridis(discrete = TRUE, option = "D") +
#   theme_minimal() +
#   theme(
#     legend.position = c(0, 1),
#     legend.justification = c(0, 1),
#     legend.title = element_blank()
#   )
# # facet_wrap(~siteID, scales = 'free_x')
# 
# # # read the external lab data in
# debugonce(neonstore::neon_read)
# neonstore::neon_read(
#   table = "asc_externalLabData-basic", product = "DP1.20194.001",
#   ext = "csv", sho
# )

# # connect to sediment db
# con = neon_db()
# sed = tbl(con, "asc_externalLabData-basic-DP1.20194.001")
# dbDisconnect(con, shutdown = TRUE)

# EPILITHON ----
## Download the epilithon files
## Check for updates if needed, Don't need to do this 
# neonstore::neon_download("DP1.20166.001")
## Check the tables already downloaded
# epilithon_tables = neonstore::neon_index("DP1.20166.001")

## read in Field data if needed
# neonstore::neon_store(table = "alg_fieldData-basic")
# epi_field_tab = neonstore::neon_table(table = "alg_fieldData-basic")

## read in and clean epilithon AFDM

# - DOM flux# epi_bio_tab = neonstore::neon_table(table = "alg_biomass-basic") %>%
#   dplyr::filter(analysisType == "AFDM" & siteID %in% streams) %>%
#   dplyr::select(siteID, collectDate, AFDM_g = "adjAshFreeDryMass") %>%
#   dplyr::filter(AFDM_g < 1) %>%
#   dplyr::mutate(AFDM_mg = AFDM_g*1000,
#                 Date = as.Date(collectDate, format = "%y-%m-%d HH:MM:SS"))
# 
# epi_bio_summ = epi_bio_tab %>%
#   group_by(siteID, Date) %>%
#   dplyr::summarise(across(AFDM_mg, list(mean = ~mean(.x, na.rm = TRUE),
#                                        quant2.5 = ~quantile(.x, 0.025, na.rm= TRUE),
#                                        quant97.5 = ~quantile(.x, 0.975, na.rm = TRUE))))
# 
# epi_bio_summ %>%
#   ggplot()+
#   # geom_ribbon(aes(x = Date, ymin= AFDM_mg_quant2.5, ymax = AFDM_mg_quant97.5))+
#   geom_line(aes(x = Date, y = log(AFDM_mg_mean)))+
#   geom_point(aes(x = Date, y = log(AFDM_mg_mean)), size = 3)+
#   theme_minimal()+
#   facet_wrap(~siteID)
