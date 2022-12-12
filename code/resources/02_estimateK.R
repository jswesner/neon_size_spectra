# Reaeration calcs ----
source("./code/resources/01_load-packages.R")

#
## HOPB reaeration calcs ----
HOPB_reaerationList = readRDS(file = "./ignore/site-gpp-data/HOPB_reaeration.rds")
HOPB_reaFormatted = HOPB_reaerationList[['reaFormatted']] %>% as.data.frame
HOPB_condFieldData = HOPB_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = HOPB_reaFormatted,
                                        savePlotPath = "./temp")

# Take a look at the background data
reaRate::bkgd.salt.conc.plot(inputFile = plotsOut,
                             savePlotPath = NULL)

# Calculate travel times
# debugonce()
HOPBHOPBreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                loggerData = HOPB_condFieldData,
                                                meanBackgroundCond = "backgroundSensorCond",
                                                plot = TRUE,
                                                savePlotPath = NULL)
# Notes --
## - HOPB.20180926, upstream is ~17:00 not below
# Example of a manual step... A few I didn't like where I picked the range, so trying again
badEventIDs <- c("HOPB.20160414", "HOPB.20160616","HOPB.20180920",#swapped loggers
                 "HOPB.20180926")
reaFormattedTake2 <- HOPBreaRatesTrvlTime$inputFile[HOPBreaRatesTrvlTime$inputFile$eventID %in% badEventIDs,]

HOPBreaRatesTrvlTimeTake2 <- reaRate::def.calc.trvl.time(inputFile = reaFormattedTake2,
                                                     loggerData = HOPB_condFieldData,
                                                     plot = TRUE,
                                                     savePlotPath = NULL)
HOPBreaRatesTrvlTimeAll <- rbind(HOPBreaRatesTrvlTime$outputDF[!HOPBreaRatesTrvlTime$outputDF$eventID %in% badEventIDs,],
                             HOPBreaRatesTrvlTimeTake2$outputDF)

# combine plotsOut calcs with travel times for rearation calcs
saveRDS(HOPBreaRatesTrvlTimeAll, "./ignore/site-gpp-data/HOPB_reaTrvlTimeAll.rds")

# ## BLDE reaeration calcs ----
rm(list = ls())
BLDE_reaerationList = readRDS(file = "./ignore/site-gpp-data/BLDE_reaeration.rds")
BLDE_reaFormatted = BLDE_reaerationList[['reaFormatted']] %>% as.data.frame
BLDE_condFieldData = BLDE_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = BLDE_reaFormatted,
                                        savePlotPath = "./temp")

# Take a look at the background data
reaRate::bkgd.salt.conc.plot(inputFile = plotsOut,
                             savePlotPath = NULL)

# Calculate travel times
# debugonce()
BLDEreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                        loggerData = BLDE_condFieldData,
                                                        meanBackgroundCond = "backgroundSensorCond",
                                                        plot = TRUE,
                                                        savePlotPath = NULL)
BLDEreaRatesTrvlTimeAll = BLDEBLDEreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(BLDEreaRatesTrvlTimeAll, "./ignore/site-gpp-data/BLDE_reaTrvlTimeAll.rds")
BLDEreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/BLDE_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge = 'meanQ_lps')


BLDEreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

BLDE_kGAM = mgcv::gam(k ~ s(discharge, bs = 'tp'), family = Gamma(link = log), data = BLDEreaRatesTrvlTimeAll[-1,])
plot(BLDE_kGAM)

saveRDS(BLDE_kGAM, "./ignore/site-gpp-data/BLDE_kGAM.rds")

## BLUE reaeration calcs ----
rm(list = ls())
BLUE_reaerationList = readRDS(file = "./ignore/site-gpp-data/BLUE_reaeration.rds")
BLUE_reaFormatted = BLUE_reaerationList[['reaFormatted']] %>% as.data.frame
BLUE_condFieldData = BLUE_reaerationList[["reaInputList"]]$rea_conductivityFieldData

saveRDS("Error: No gas releases", "./ignore/site-gpp-data/BLUE_reaTrvlTimeAll.rds")

## CARI reaeration calcs ----
rm(list = ls())
CARI_reaerationList = readRDS(file = "./ignore/site-gpp-data/CARI_reaeration.rds")
CARI_reaFormatted = CARI_reaerationList[['reaFormatted']] %>% as.data.frame
CARI_condFieldData = CARI_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = CARI_reaFormatted,
                                        savePlotPath = "./temp")

# Calculate travel times
# debugonce()
CARIreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = CARI_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)
CARIreaRatesTrvlTimeAll = CARIreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(CARIreaRatesTrvlTimeAll, "./ignore/site-gpp-data/CARI_reaTrvlTimeAll.rds")
CARIreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/CARI_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge = 'meanQ_lps')


CARIreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

CARI_kGAM = mgcv::gam(k ~ s(discharge, bs = 'tp'), family = Gamma(link = log), data = CARIreaRatesTrvlTimeAll %>% dplyr::filter(between(k,0.01,5e4)))
plot(CARI_kGAM)

saveRDS(CARI_kGAM, "./ignore/site-gpp-data/CARI_kGAM.rds")

## COMO reaeration calcs ----
rm(list = ls())
COMO_reaerationList = readRDS(file = "./ignore/site-gpp-data/COMO_reaeration.rds")
COMO_reaFormatted = COMO_reaerationList[['reaFormatted']] %>% as.data.frame
COMO_condFieldData = COMO_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = COMO_reaFormatted,
                                        savePlotPath = "./temp")

# Calculate travel times
COMOreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = COMO_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

# Notes --
# Example of a manual step... A few I didn't like where I picked the range, so trying again
badEventIDs <- c("COMO.20170627", "COMO.20170925" )

COMOreaRatesTrvlTimeAll = COMOreaRatesTrvlTime$outputDF %>% dplyr::filter(eventID %ni% badEventIDs)
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(COMOreaRatesTrvlTimeAll, "./ignore/site-gpp-data/COMO_reaTrvlTimeAll.rds")
COMOreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/COMO_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge = 'meanQ_lps')

COMOreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =log(discharge), y = log(k)))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

COMO_kGAM = mgcv::gam( log(k) ~ s(log(discharge), bs = 'tp'),family = gaussian(), data = COMOreaRatesTrvlTimeAll) 
                       # family = Gamma(link = log), data = COMOreaRatesTrvlTimeAll)
plot(COMO_kGAM)

COMO_kLM = lm(log(k) ~ log(discharge), data = COMOreaRatesTrvlTimeAll) 

saveRDS(COMO_kLM, "./ignore/site-gpp-data/COMO_kGAM.rds")

## CUPE reaeration calcs ----
rm(list = ls())
CUPE_reaerationList = readRDS(file = "./ignore/site-gpp-data/CUPE_reaeration.rds")
CUPE_reaFormatted = CUPE_reaerationList[['reaFormatted']] %>% as.data.frame
CUPE_condFieldData = CUPE_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = CUPE_reaFormatted,
                                        savePlotPath = "./temp")

# Calculate travel times
CUPEreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = CUPE_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

saveRDS("No gas injections", "./ignore/site-gpp-data/CUPE_kGAM.rds")

## GUIL reaeration calcs ----
rm(list = ls())
GUIL_reaerationList = readRDS(file = "./ignore/site-gpp-data/GUIL_reaeration.rds")
GUIL_reaFormatted = GUIL_reaerationList[['reaFormatted']] %>% as.data.frame
GUIL_condFieldData = GUIL_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = GUIL_reaFormatted,
                                        savePlotPath = "./temp")

# Calculate travel times
GUILreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = GUIL_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

# Notes --
# Example of a manual step... A few I didn't like where I picked the range, so trying again

GUILreaRatesTrvlTimeAll = GUILreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(GUILreaRatesTrvlTimeAll, "./ignore/site-gpp-data/GUIL_reaTrvlTimeAll.rds")
GUILreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/GUIL_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge = 'meanQ_lps') %>%
  dplyr::filter(k < 1e6)

GUILreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))
  # coord_trans('log10')

GUIL_kGAM = mgcv::gam( k ~ s(discharge, bs = 'tp'), family = Gamma(link = log), data = GUILreaRatesTrvlTimeAll)

# family = Gamma(link = log), data = GUILreaRatesTrvlTimeAll)
plot(GUIL_kGAM)

GUIL_kLM = lm(log(k) ~ log(discharge), data = GUILreaRatesTrvlTimeAll) 

saveRDS(GUIL_kGAM, "./ignore/site-gpp-data/GUIL_kGAM.rds")
















##### 


HOPBreaRatesTrvlTimeAll = readRDS("./ignore/site-gpp-data/HOPB_reaTrvlTimeAll.rds")
reaRateFull = HOPBreaRatesTrvlTimeAll %>%
  dplyr::select(-c(slopeRaw, slopeClean, slopeSaltCorr, slopeNormClean, slopeNormCorr)) %>%
  left_join(plotsOut %>%
              dplyr::select(siteID, eventID, slopeRaw, slopeClean, slopeSaltCorr,slopeNormClean, slopeNormCorr, stationToInjectionDistance, fieldDischarge_lps, waterTemp, wettedWidth))



# Use SF6 loss rates for clean data to get k600 and K600
reaRatesCalc <- reaRate::def.calc.reaeration(inputFile = HOPBreaRatesTrvlTimeAll,
                                             lossRateSF6 = "slopeClean",
                                             outputSuffix = "clean")

# Use SF6 loss rates for background and plateau salt corrected data to get k600 and K600
reaRatesCalc <- reaRate::def.calc.reaeration(inputFile = reaRatesCalc,
                                             lossRateSF6 = "slopeBackCorr",
                                             outputSuffix = "allCorr")

# Take a quick look at the different k660 v discharge depending on salt correction choice
plot(reaRatesCalc$meanQ,
     reaRatesCalc$k600.clean,
     pch = 19,
     xlab = "discharge",
     ylab = "k600")
points(reaRatesCalc$meanQ,
       reaRatesCalc$k600.allCorr,
       pch = 19,
       col = "blue")
legend("topright",
       legend = c("gas uncorrected","gas background salt corrected"),
       pch = 19,
       col = c("black", "blue"))


##### This is an example for the model-slug only sites #####

#User Inputs
siteID <- "ARIK"
plotPath <- paste0("~/reaOutputs/",siteID,"/QAQC_plots")

#String constants
reaDPID <- "DP1.20190.001"
dscDPID <- "DP1.20048.001"
wqDPID <- "DP1.20288.001"

# # Download Reaeration Data (just delete the input for dates to get data for all time for a site)
# reaInputList <- neonUtilities::loadByProduct(dpID = reaDPID, 
#                                              site = siteID,
#                                              startdate = "2019-01-01", 
#                                              enddate = "2021-12-01",
#                                              check.size = FALSE)
# 
# rea_backgroundFieldCondDataIn <- reaInputList$rea_backgroundFieldCondData
# rea_backgroundFieldSaltDataIn <- reaInputList$rea_backgroundFieldSaltData
# rea_fieldDataIn <- reaInputList$rea_fieldData
# rea_plateauMeasurementFieldDataIn <- reaInputList$rea_plateauMeasurementFieldData
# rea_plateauSampleFieldDataIn <- reaInputList$rea_plateauSampleFieldData
# rea_externalLabDataSaltIn <- reaInputList$rea_externalLabDataSalt
# rea_externalLabDataGasIn <- reaInputList$rea_externalLabDataGas
# rea_widthFieldDataIn <- reaInputList$rea_widthFieldData
# 
# # Download Discharge Data
# qInputList <- neonUtilities::loadByProduct(dpID = dscDPID, site = siteID, check.size = FALSE)
# 
# dsc_fieldDataIn <- qInputList$dsc_fieldData
# dsc_individualFieldDataIn <- qInputList$dsc_individualFieldData
# dsc_fieldDataADCPIn <- qInputList$dsc_fieldDataADCP
# 
# # Download Sensor Data
# sensorData <- neonUtilities::loadByProduct(dpID = wqDPID, 
#                                            site = siteID,
#                                            check.size = FALSE)
# waq_instantaneousIn <- sensorData$waq_instantaneous
# 
# # It can take a while to download data, so save it in case you need to go back
# save.image(paste0("~/reaOutputs/",siteID,"/downloadedData.RData")) 
load(paste0("~/reaOutputs/",siteID,"/downloadedData.RData"))

# Format the downloaded data so everything is in one table
reaFormatted <- reaRate::def.format.reaeration(rea_backgroundFieldCondData = rea_backgroundFieldCondDataIn,
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

# Fix an issue with the data
reaFormatted$namedLocation[reaFormatted$namedLocation == "ARIK.AOS.reaeration.station.02"] <- "ARIK.AOS.reaeration.station.04"

# # Calculate SF6 loss rates (this should just give an error)
# plotsOut <- reaRate::gas.loss.rate.plot(inputFile = reaFormatted,
#                                         savePlotPath = plotPath)
# 
# # Take a look at the background data (this just produces an error now)
# reaRate::bkgd.salt.conc.plot(inputFile = reaFormatted,
#                              savePlotPath = plotPath)

# Calculate travel times
HOPBreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = reaFormatted,
                                                loggerData = reaInputList$rea_conductivityFieldData,
                                                meanBackgroundCond = "backgroundSensorCond",
                                                plot = TRUE,
                                                savePlotPath = plotPath)