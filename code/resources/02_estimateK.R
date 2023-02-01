# Reaeration calcs ----

## ## ARIK reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")
ARIK_reaerationList = readRDS(file = "./ignore/site-gpp-data/ARIK_reaeration.rds")
ARIK_reaFormatted = ARIK_reaerationList[['reaFormatted']] %>% as.data.frame
ARIK_condFieldData = ARIK_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = ARIK_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times

ARIKreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = ARIK_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)
badEventIDs <- c("HOPB.20160414")


ARIKreaRatesTrvlTimeAll = ARIKreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(ARIKreaRatesTrvlTimeAll, "./ignore/site-gpp-data/ARIK_reaTrvlTimeAll.rds")

ARIKreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/ARIK_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge = 'meanQ_lps')


ARIKreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

ARIK_kGAM = mgcv::gam(k ~ s(discharge, bs = 'tp'), family = Gamma(link = log), data = ARIKreaRatesTrvlTimeAll[-1,])
plot(ARIK_kGAM)
gam.check(ARIK_kGAM)

saveRDS("Error: No gas releases", "./ignore/site-gpp-data/ARIK_kGAM.rds")

### ## BIGC reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")
BIGC_reaerationList = readRDS(file = "./ignore/site-gpp-data/BIGC_reaeration.rds")
BIGC_reaFormatted = BIGC_reaerationList[['reaFormatted']] %>% as.data.frame
BIGC_condFieldData = BIGC_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = BIGC_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times

BIGCreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = BIGC_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

BIGCreaRatesTrvlTimeAll = BIGCreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(BIGCreaRatesTrvlTimeAll, "./ignore/site-gpp-data/BIGC_reaTrvlTimeAll.rds")

BIGCreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/BIGC_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0)

BIGCreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

# remove a single very high value and fit GAM model
BIGC_kGAM = mgcv::gam(k ~ s(discharge.daily, bs = 'cr'), data = BIGCreaRatesTrvlTimeAll, family = nb())#Gamma(link = log))
summary(BIGC_kGAM)
plot(BIGC_kGAM)
gam.check(BIGC_kGAM)
BIGC_kLM = lm(log(k)~log(discharge.daily), data = BIGCreaRatesTrvlTimeAll)
summary(BIGC_kLM);plot(BIGC_kLM)
saveRDS(BIGC_kLM, "./ignore/site-gpp-data/BIGC_kGAM.rds")

## BLDE reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")
BLDE_reaerationList = readRDS(file = "./ignore/site-gpp-data/BLDE_reaeration.rds")
BLDE_reaFormatted = BLDE_reaerationList[['reaFormatted']] %>% as.data.frame
BLDE_condFieldData = BLDE_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = BLDE_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
# debugonce()
BLDEreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                        loggerData = BLDE_condFieldData,
                                                        meanBackgroundCond = "backgroundSensorCond",
                                                        plot = TRUE,
                                                        savePlotPath = NULL)
BLDEreaRatesTrvlTimeAll = BLDEreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(BLDEreaRatesTrvlTimeAll, "./ignore/site-gpp-data/BLDE_reaTrvlTimeAll.rds")

BLDEreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/BLDE_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0) %>% .[-1,]

BLDEreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

BLDE_kGAM = mgcv::gam(k ~ s(discharge.daily, bs = 'cs'), data = BLDEreaRatesTrvlTimeAll, family = Gamma(link = "log"))
summary(BLDE_kGAM)
plot(BLDE_kGAM)
gam.check(BLDE_kGAM)
summary(BLDE_kGAM)
BLDE_kLM = lm(log(k) ~ log(discharge.daily), data = BLDEreaRatesTrvlTimeAll)
summary(BLDE_kLM)
saveRDS(BLDE_kGAM, "./ignore/site-gpp-data/BLDE_kGAM.rds")

## BLUE reaeration calcs ----
rm(list = ls())
BLUE_reaerationList = readRDS(file = "./ignore/site-gpp-data/BLUE_reaeration.rds")
BLUE_reaFormatted = BLUE_reaerationList[['reaFormatted']] %>% as.data.frame
BLUE_condFieldData = BLUE_reaerationList[["reaInputList"]]$rea_conductivityFieldData

plotsOut <- reaRate::gas.loss.rate.plot(inputFile = BLUE_reaFormatted,
                                        savePlotPath = "./ignore/temp")


saveRDS("Error: No gas releases", "./ignore/site-gpp-data/BLUE_reaTrvlTimeAll.rds")
saveRDS("Error: No gas releases", "./ignore/site-gpp-data/BLUE_kGAM.rds")
## CUPE reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

CUPE_reaerationList = readRDS(file = "./ignore/site-gpp-data/CUPE_reaeration.rds")
CUPE_reaFormatted = CUPE_reaerationList[['reaFormatted']] %>% as.data.frame
CUPE_condFieldData = CUPE_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = CUPE_reaFormatted,
                                        savePlotPath = "./ignore/temp")

plotsOut = plotsOut[-1,]

# Calculate travel times
CUPEreaRatesTrvlTime <- 
  reaRate::def.calc.trvl.time(inputFile = plotsOut,
                              loggerData = CUPE_condFieldData,
                              meanBackgroundCond = "backgroundSensorCond",
                              plot = TRUE,
                              savePlotPath = NULL)

bad_events = c("CUPE.20161102","CUPE.20170823")

CUPEreaRatesTrvlTimeAll = CUPEreaRatesTrvlTime$outputDF %>%
  dplyr::filter(eventID %ni% bad_events | peakMaxTravelTime > 0)
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(CUPEreaRatesTrvlTimeAll, "./ignore/site-gpp-data/CUPE_reaTrvlTimeAll.rds")

debugonce(calc_k600)
CUPEreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/CUPE_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxVelocity > 0)


CUPEreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

CUPE_kGAM = mgcv::gam(k ~ s(discharge.daily, bs = 'tp'), family = Gamma(link = log), data = CUPEreaRatesTrvlTimeAll)
plot(CUPE_kGAM)
gam.check(CUPE_kGAM)
CUPE_kLM = lm(log(k)~log(discharge.daily), data = CUPEreaRatesTrvlTimeAll)
summary(CUPE_kLM)
saveRDS(CUPE_kGAM, "./ignore/site-gpp-data/CUPE_kGAM.rds")

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
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxVelocity > 0,
                between(k, 0.01, 5e4))


CARIreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

CARI_kGAM = mgcv::gam(k ~ s(discharge.daily, bs = 'cs'), data = CARIreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))

summary(CARI_kGAM)
plot(CARI_kGAM)
CARI_kLM = lm(log(k) ~ log(discharge.daily), data = CARIreaRatesTrvlTimeAll)
summary(CARI_kLM)
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
  dplyr::rename(discharge.daily = 'meanQ_lps')

COMOreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

COMO_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs'), data = COMOreaRatesTrvlTimeAll,family = nb())#Gamma(link = "log"))
plot(COMO_kGAM)

COMO_kLM = lm(log(k) ~ log(discharge.daily), data = COMOreaRatesTrvlTimeAll) 

saveRDS(COMO_kGAM, "./ignore/site-gpp-data/COMO_kGAM.rds")

# ## CUPE reaeration calcs ----
# rm(list = ls())
# CUPE_reaerationList = readRDS(file = "./ignore/site-gpp-data/CUPE_reaeration.rds")
# CUPE_reaFormatted = CUPE_reaerationList[['reaFormatted']] %>% as.data.frame
# CUPE_condFieldData = CUPE_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# # Calculate SF6 loss rates
# plotsOut <- reaRate::gas.loss.rate.plot(inputFile = CUPE_reaFormatted,
#                                         savePlotPath = "./temp")
# 
# # Calculate travel times
# CUPEreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
#                                                     loggerData = CUPE_condFieldData,
#                                                     meanBackgroundCond = "backgroundSensorCond",
#                                                     plot = TRUE,
#                                                     savePlotPath = NULL)
# 
# saveRDS("No gas injections", "./ignore/site-gpp-data/CUPE_kGAM.rds")

## GUIL reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

GUIL_reaerationList = readRDS(file = "./ignore/site-gpp-data/GUIL_reaeration.rds")
GUIL_reaFormatted = GUIL_reaerationList[['reaFormatted']] %>% as.data.frame
GUIL_condFieldData = GUIL_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = GUIL_reaFormatted,
                                        savePlotPath = "./ignore/temp")

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
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(k < 5e+05)

GUILreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))
  # coord_trans('log10')

GUIL_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs'), data = GUILreaRatesTrvlTimeAll, family = Gamma(link = "log"))
plot(GUIL_kGAM)
gam.check(GUIL_kGAM)

GUIL_kLM = lm(log(k) ~ log(discharge.daily), data = GUILreaRatesTrvlTimeAll) 
summary(GUIL_kLM)
saveRDS(GUIL_kGAM, "./ignore/site-gpp-data/GUIL_kGAM.rds")

## HOPB reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

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

HOPBreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/HOPB_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(k < 750000,
                peakMaxTravelTime > 0)

HOPBreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))
# coord_trans('log10')

HOPB_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'tp'), data = HOPBreaRatesTrvlTimeAll, family = Gamma(link = "log"))
plot(HOPB_kGAM)
gam.check(HOPB_kGAM)

saveRDS(HOPB_kGAM, "./ignore/site-gpp-data/HOPB_kGAM.rds")

## KING reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

KING_reaerationList = readRDS(file = "./ignore/site-gpp-data/KING_reaeration.rds")
KING_reaFormatted = KING_reaerationList[['reaFormatted']] %>% as.data.frame
KING_condFieldData = KING_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = KING_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
KINGreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                        loggerData = KING_condFieldData,
                                                        meanBackgroundCond = "backgroundSensorCond",
                                                        plot = TRUE,
                                                        savePlotPath = NULL)

KINGreaRatesTrvlTimeAll <- KINGreaRatesTrvlTime$outputDF

# combine plotsOut calcs with travel times for rearation calcs
saveRDS(KINGreaRatesTrvlTimeAll, "./ignore/site-gpp-data/KING_reaTrvlTimeAll.rds")

KINGreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/KING_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter( peakMaxTravelTime > 0)

KINGreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

KING_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs'), family = Gamma(link = "log"), data = KINGreaRatesTrvlTimeAll)
plot(KING_kGAM)
gam.check(KING_kGAM)

KING_kLM = lm(log(k)~log(discharge.daily), data = KINGreaRatesTrvlTimeAll)
summary(KING_kLM)
saveRDS(KING_kLM, "./ignore/site-gpp-data/KING_kGAM.rds")

## LEWI reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

LEWI_reaerationList = readRDS(file = "./ignore/site-gpp-data/LEWI_reaeration.rds")
LEWI_reaFormatted = LEWI_reaerationList[['reaFormatted']] %>% as.data.frame
LEWI_condFieldData = LEWI_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = LEWI_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
LEWIreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = LEWI_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

bad_events <- c("LEWI.20161205")

LEWIreaRatesTrvlTimeAll <- LEWIreaRatesTrvlTime$outputDF[-which(LEWIreaRatesTrvlTime$outputDF$eventID %in% bad_events),]

# combine plotsOut calcs with travel times for rearation calcs
saveRDS(LEWIreaRatesTrvlTimeAll, "./ignore/site-gpp-data/LEWI_reaTrvlTimeAll.rds")

LEWIreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/LEWI_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter( peakMaxTravelTime > 0) %>%
  .[-c(11,24,34,37),]

LEWIreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')#+geom_smooth(aes(x = discharge.daily, y = k), method = 'lm')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

LEWI_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr'), data = LEWIreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"), )
summary(LEWI_kGAM)
plot(LEWI_kGAM)
gam.check(LEWI_kGAM)

LEWI_kLM = lm(log(k)~log(discharge.daily), data = LEWIreaRatesTrvlTimeAll)
summary(LEWI_kLM)
saveRDS(LEWI_kGAM, "./ignore/site-gpp-data/LEWI_kGAM.rds")

## MART reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

MART_reaerationList = readRDS(file = "./ignore/site-gpp-data/MART_reaeration.rds")
MART_reaFormatted = MART_reaerationList[['reaFormatted']] %>% as.data.frame
MART_condFieldData = MART_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = MART_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
MARTreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = MART_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

MARTreaRatesTrvlTimeAll <- MARTreaRatesTrvlTime$outputDF

# combine plotsOut calcs with travel times for rearation calcs
saveRDS(MARTreaRatesTrvlTimeAll, "./ignore/site-gpp-data/MART_reaTrvlTimeAll.rds")

MARTreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/MART_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0)

MARTreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

MART_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr'), data = MARTreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
plot(MART_kGAM)
gam.check(MART_kGAM)

MART_kLM = lm(log(k)~log(discharge.daily), data = MARTreaRatesTrvlTimeAll)
summary(MART_kLM);plot(MART_kLM)
saveRDS(MART_kGAM, "./ignore/site-gpp-data/MART_kGAM.rds")

## MAYF reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

MAYF_reaerationList = readRDS(file = "./ignore/site-gpp-data/MAYF_reaeration.rds")
MAYF_reaFormatted = MAYF_reaerationList[['reaFormatted']] %>% as.data.frame
MAYF_condFieldData = MAYF_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = MAYF_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
MAYFreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = MAYF_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)
badEventIDs <- c("MAYF.20141024")
MAYFreaRatesTrvlTimeAll <- MAYFreaRatesTrvlTime$outputDF[-which(MAYFreaRatesTrvlTime$outputDF$eventID %in% badEventIDs),]

# combine plotsOut calcs with travel times for rearation calcs
saveRDS(MAYFreaRatesTrvlTimeAll, "./ignore/site-gpp-data/MAYF_reaTrvlTimeAll.rds")

MAYFreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/MAYF_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(k < 5e+06,
                peakMaxTravelTime > 0)

MAYFreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))
# 
MAYF_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr'), data = MAYFreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
summary(MAYF_kGAM)
plot(MAYF_kGAM)
# gam.check(MAYF_kGAM)
# 
MAYF_kLM = lm(log(k)~log(discharge.daily), data = MAYFreaRatesTrvlTimeAll)
summary(MAYF_kLM);plot(MAYF_kLM)

saveRDS(MAYF_kGAM, "./ignore/site-gpp-data/MAYF_kGAM.rds")

## MCDI reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

MCDI_reaerationList = readRDS(file = "./ignore/site-gpp-data/MCDI_reaeration.rds")
MCDI_reaFormatted = MCDI_reaerationList[['reaFormatted']] %>% as.data.frame
MCDI_condFieldData = MCDI_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = MCDI_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
MCDIreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = MCDI_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)
MCDIreaRatesTrvlTimeAll <- MCDIreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(MCDIreaRatesTrvlTimeAll, "./ignore/site-gpp-data/MCDI_reaTrvlTimeAll.rds")

MCDIreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/MCDI_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0)

MCDIreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))
 
MCDI_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr', k = 4), data = MCDIreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
plot(MCDI_kGAM)
gam.check(MCDI_kGAM)
# 
MCDI_kLM = lm(log(k)~log(discharge.daily), data = MCDIreaRatesTrvlTimeAll)
summary(MCDI_kLM);plot(MCDI_kLM)

saveRDS(MCDI_kLM, "./ignore/site-gpp-data/MCDI_kGAM.rds")

## OKSR reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

OKSR_reaerationList = readRDS(file = "./ignore/site-gpp-data/OKSR_reaeration.rds")
OKSR_reaFormatted = OKSR_reaerationList[['reaFormatted']] %>% as.data.frame
OKSR_condFieldData = OKSR_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = OKSR_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
OKSRreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = OKSR_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)
OKSRreaRatesTrvlTimeAll <- OKSRreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(OKSRreaRatesTrvlTimeAll, "./ignore/site-gpp-data/OKSR_reaTrvlTimeAll.rds")

OKSRreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/OKSR_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0) %>% 
  .[-13,]

OKSRreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

OKSR_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr'), data = OKSRreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
plot(OKSR_kGAM)
gam.check(OKSR_kGAM)
# 
OKSR_kLM = lm(log(k)~log(discharge.daily), data = OKSRreaRatesTrvlTimeAll)
summary(OKSR_kLM);plot(OKSR_kLM)

saveRDS(OKSR_kGAM, "./ignore/site-gpp-data/OKSR_kGAM.rds")

## POSE reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

POSE_reaerationList = readRDS(file = "./ignore/site-gpp-data/POSE_reaeration.rds")
POSE_reaFormatted = POSE_reaerationList[['reaFormatted']] %>% as.data.frame
POSE_condFieldData = POSE_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = POSE_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
POSEreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = POSE_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

bad_events <- c('POSE.20190617')
POSEreaRatesTrvlTimeAll <- POSEreaRatesTrvlTime$outputDF[-which(POSEreaRatesTrvlTime$outputDF$eventID %in% bad_events),]
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(POSEreaRatesTrvlTimeAll, "./ignore/site-gpp-data/POSE_reaTrvlTimeAll.rds")

POSEreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/POSE_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0) #%>% 
  # .[-13,]

POSEreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

POSE_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr'), data = POSEreaRatesTrvlTimeAll, family = Gamma(link = "log"))
plot(POSE_kGAM)
gam.check(POSE_kGAM)
# 
POSE_kLM = lm(log(k)~log(discharge.daily), data = POSEreaRatesTrvlTimeAll)
summary(POSE_kLM);plot(POSE_kLM)

saveRDS(POSE_kGAM, "./ignore/site-gpp-data/POSE_kGAM.rds")

## PRIN reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

PRIN_reaerationList = readRDS(file = "./ignore/site-gpp-data/PRIN_reaeration.rds")
PRIN_reaFormatted = PRIN_reaerationList[['reaFormatted']] %>% as.data.frame
PRIN_condFieldData = PRIN_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = PRIN_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
PRINreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = PRIN_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

PRINreaRatesTrvlTimeAll <- PRINreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(PRINreaRatesTrvlTimeAll, "./ignore/site-gpp-data/PRIN_reaTrvlTimeAll.rds")

PRINreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/PRIN_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0) #%>% 
# .[-13,]

PRINreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

PRIN_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cr', k = 7), data = PRINreaRatesTrvlTimeAll, family = Gamma(link = "log"))
plot(PRIN_kGAM)
gam.check(PRIN_kGAM)
# 
PRIN_kLM = lm(log(k)~log(discharge.daily), data = PRINreaRatesTrvlTimeAll)
summary(PRIN_kLM);plot(PRIN_kLM)

saveRDS(PRIN_kGAM, "./ignore/site-gpp-data/PRIN_kGAM.rds")

## REDB reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

REDB_reaerationList = readRDS(file = "./ignore/site-gpp-data/REDB_reaeration.rds")
REDB_reaFormatted = REDB_reaerationList[['reaFormatted']] %>% as.data.frame
REDB_condFieldData = REDB_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = REDB_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
REDBreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = REDB_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

REDBreaRatesTrvlTimeAll <- REDBreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(REDBreaRatesTrvlTimeAll, "./ignore/site-gpp-data/REDB_reaTrvlTimeAll.rds")

REDBreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/REDB_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0,
                k < 5e+05) #%>% 
# .[-13,]

REDBreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

REDB_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs'), data = REDBreaRatesTrvlTimeAll, family = Gamma(link = "log"))
plot(REDB_kGAM)
gam.check(REDB_kGAM)
# 
REDB_kLM = lm(log(k)~log(discharge.daily), data = REDBreaRatesTrvlTimeAll)
summary(REDB_kLM);plot(REDB_kLM)

saveRDS(REDB_kGAM, "./ignore/site-gpp-data/REDB_kGAM.rds")

## SYCA reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

SYCA_reaerationList = readRDS(file = "./ignore/site-gpp-data/SYCA_reaeration.rds")
SYCA_reaFormatted = SYCA_reaerationList[['reaFormatted']] %>% as.data.frame
SYCA_condFieldData = SYCA_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = SYCA_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
SYCAreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = SYCA_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

SYCAreaRatesTrvlTimeAll <- SYCAreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(SYCAreaRatesTrvlTimeAll, "./ignore/site-gpp-data/SYCA_reaTrvlTimeAll.rds")

SYCAreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/SYCA_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0)#,
                # k < 5e+05) #%>% 
# .[-13,]

SYCAreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

SYCA_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs', k = 9), data = SYCAreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
plot(SYCA_kGAM)
gam.check(SYCA_kGAM)
# 
SYCA_kLM = lm(log(k)~log(discharge.daily), data = SYCAreaRatesTrvlTimeAll)
summary(SYCA_kLM);plot(SYCA_kLM)

saveRDS(SYCA_kGAM, "./ignore/site-gpp-data/SYCA_kGAM.rds")

## TECR reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

TECR_reaerationList = readRDS(file = "./ignore/site-gpp-data/TECR_reaeration.rds")
TECR_reaFormatted = TECR_reaerationList[['reaFormatted']] %>% as.data.frame
TECR_condFieldData = TECR_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = TECR_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
TECRreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = TECR_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

TECRreaRatesTrvlTimeAll <- TECRreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(TECRreaRatesTrvlTimeAll, "./ignore/site-gpp-data/TECR_reaTrvlTimeAll.rds")

TECRreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/TECR_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0)#,
# k < 5e+05) #%>% 
# .[-13,]

TECRreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

TECR_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs', k = 9), data = TECRreaRatesTrvlTimeAll, family = nb())#family = Gamma(link = "log"))
plot(TECR_kGAM)
gam.check(TECR_kGAM)
# 
TECR_kLM = lm(log(k)~log(discharge.daily), data = TECRreaRatesTrvlTimeAll)
summary(TECR_kLM);plot(TECR_kLM)

saveRDS(TECR_kGAM, "./ignore/site-gpp-data/TECR_kGAM.rds")
##### 

## WALK reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

WALK_reaerationList = readRDS(file = "./ignore/site-gpp-data/WALK_reaeration.rds")
WALK_reaFormatted = WALK_reaerationList[['reaFormatted']] %>% as.data.frame
WALK_condFieldData = WALK_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = WALK_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
WALKreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = WALK_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

WALKreaRatesTrvlTimeAll <- WALKreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(WALKreaRatesTrvlTimeAll, "./ignore/site-gpp-data/WALK_reaTrvlTimeAll.rds")

WALKreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/WALK_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0,
                discharge.daily >1)#,
# k < 5e+05) #%>% 
# .[-13,]

WALKreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

WALK_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs'), data = WALKreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
summary(WALK_kGAM)
plot(WALK_kGAM)
gam.check(WALK_kGAM)
# 
WALK_kLM = lm(log(k)~log(discharge.daily), data = WALKreaRatesTrvlTimeAll)
summary(WALK_kLM);plot(WALK_kLM)

saveRDS(WALK_kGAM, "./ignore/site-gpp-data/WALK_kGAM.rds")
##### 

## WLOU reaeration calcs ----
rm(list = ls())
source("./code/resources/01_load-packages.R")

WLOU_reaerationList = readRDS(file = "./ignore/site-gpp-data/WLOU_reaeration.rds")
WLOU_reaFormatted = WLOU_reaerationList[['reaFormatted']] %>% as.data.frame
WLOU_condFieldData = WLOU_reaerationList[["reaInputList"]]$rea_conductivityFieldData
# Calculate SF6 loss rates
# debugonce(reaRate::gas.loss.rate.plot)
plotsOut <- reaRate::gas.loss.rate.plot(inputFile = WLOU_reaFormatted,
                                        savePlotPath = "./ignore/temp")

# Calculate travel times
WLOUreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = plotsOut,
                                                    loggerData = WLOU_condFieldData,
                                                    meanBackgroundCond = "backgroundSensorCond",
                                                    plot = TRUE,
                                                    savePlotPath = NULL)

WLOUreaRatesTrvlTimeAll <- WLOUreaRatesTrvlTime$outputDF
# combine plotsOut calcs with travel times for rearation calcs
saveRDS(WLOUreaRatesTrvlTimeAll, "./ignore/site-gpp-data/WLOU_reaTrvlTimeAll.rds")

WLOUreaRatesTrvlTimeAll = readRDS(file = "./ignore/site-gpp-data/WLOU_reaTrvlTimeAll.rds") %>% calc_k600(.) %>%
  dplyr::rename(discharge.daily = 'meanQ_lps') %>%
  dplyr::filter(peakMaxTravelTime > 0)#,
                discharge.daily >1)#,
# k < 5e+05) #%>% 
# .[-13,]

WLOUreaRatesTrvlTimeAll %>% ggplot() + geom_point(aes(x =discharge.daily, y = k))+
  # coord_trans('log10')
  coord_cartesian(xlim = c(0,NA), ylim = c(0,NA))

WLOU_kGAM = mgcv::gam( k ~ s(discharge.daily, bs = 'cs'), data = WLOUreaRatesTrvlTimeAll, family = nb())#Gamma(link = "log"))
summary(WLOU_kGAM)
plot(WLOU_kGAM)
gam.check(WLOU_kGAM)
# 
WLOU_kLM = lm(log(k)~log(discharge.daily), data = WLOUreaRatesTrvlTimeAll)
summary(WLOU_kLM);plot(WLOU_kLM)

saveRDS(WLOU_kLM, "./ignore/site-gpp-data/WLOU_kGAM.rds")
##### 















###### SPARE(D) CODE ######

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

# #User Inputs
# siteID <- "ARIK"
# plotPath <- paste0("~/reaOutputs/",siteID,"/QAQC_plots")
# 
# #String constants
# reaDPID <- "DP1.20190.001"
# dscDPID <- "DP1.20048.001"
# wqDPID <- "DP1.20288.001"

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
# load(paste0("~/reaOutputs/",siteID,"/downloadedData.RData"))
# 
# # Format the downloaded data so everything is in one table
# reaFormatted <- reaRate::def.format.reaeration(rea_backgroundFieldCondData = rea_backgroundFieldCondDataIn,
#                                                rea_backgroundFieldSaltData = rea_backgroundFieldSaltDataIn,
#                                                rea_fieldData = rea_fieldDataIn,
#                                                rea_plateauMeasurementFieldData = rea_plateauMeasurementFieldDataIn,
#                                                rea_plateauSampleFieldData = rea_plateauSampleFieldDataIn,
#                                                rea_externalLabDataSalt = rea_externalLabDataSaltIn,
#                                                rea_externalLabDataGas = rea_externalLabDataGasIn,
#                                                rea_widthFieldData = rea_widthFieldDataIn,
#                                                dsc_fieldData = dsc_fieldDataIn,
#                                                dsc_individualFieldData = dsc_individualFieldDataIn,
#                                                dsc_fieldDataADCP = dsc_fieldDataADCPIn,
#                                                waq_instantaneous = waq_instantaneousIn)
# 
# # Fix an issue with the data
# reaFormatted$namedLocation[reaFormatted$namedLocation == "ARIK.AOS.reaeration.station.02"] <- "ARIK.AOS.reaeration.station.04"
# 
# # # Calculate SF6 loss rates (this should just give an error)
# # plotsOut <- reaRate::gas.loss.rate.plot(inputFile = reaFormatted,
# #                                         savePlotPath = plotPath)
# # 
# # # Take a look at the background data (this just produces an error now)
# # reaRate::bkgd.salt.conc.plot(inputFile = reaFormatted,
# #                              savePlotPath = plotPath)
# 
# # Calculate travel times
# HOPBreaRatesTrvlTime <- reaRate::def.calc.trvl.time(inputFile = reaFormatted,
#                                                 loggerData = reaInputList$rea_conductivityFieldData,
#                                                 meanBackgroundCond = "backgroundSensorCond",
#                                                 plot = TRUE,
#                                                 savePlotPath = plotPath)