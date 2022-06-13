# custom functions for prepping data products for metabolism models

#' @title get_site_data
#' @description This function subsets all the relevant data products to estimate stream metabolism  for a site
#' @param sitecode character. Character string representing the stream identification code 
#' @param startDate character. Character coercible to Date, representing start date of data series. If NULL (default), the entire data series are downloaded.
#' @param endDate character. Character, coercible to Date, representing end data of data series. If NULL (default), the entire data series are downloaed.
#'

get_site_data = function(siteCode = NA, startDate = NULL, endDate = NULL,...){
  require(tidyverse)
  require(lutz)
  
  ## Load in necessary data objects
  latlong = read_csv(file = "./data/site_latlong.csv")
  siteLatLong = latlong[which(latlong$site == siteCode),]
  
  ## estimate the timezone 
  siteTZ = lutz::tz_lookup_coords(lat = unlist(latlong[which(latlong$site == siteCode), 'lat']),
                                  lon = unlist(latlong[which(latlong$site == siteCode), 'long']),
                                  method = 'accurate')
  
  ## Get site files
  fileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,".*.rds"), full.names = TRUE)
  
  ## Get site file names
  fileNames = fileList %>%
    lapply(., function(x) gsub("^(\\w{4})_\\d{1,2}.*_(\\w{2,}).rds","\\1_\\2",
                               sapply(strsplit(x,"/"),"[",4))) %>%
    unlist
  
  ## Read in the files and set names
  siteList = fileList %>%
    purrr::map(readRDS) %>% setNames(., fileNames)
  
  oldColNames = c(
    "timePeriod",
    "dissolvedOxygen",
    "surfWaterTempMean",
    "maxpostDischarge",
    "staPresMean"
  )
  
  newColNames = c(
    "startDateTime",
    "DO.obs",
    "temp.water",
    "discharge",
    "air.pressure"
  )
  
  columnKeyVal = setNames(newColNames,oldColNames)

  ## Convert all the data sets
  ### DO
  doDf = siteList[[grep('DO', names(siteList))]] %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    dplyr::select(startDateTime, DO.obs)
  ### temperature
  tempDf = siteList[[grep('temp', names(siteList))]] %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    dplyr::select(startDateTime, temp.water)
  
  ### discharge
  qDf = siteList[[grep('dischargeQ', names(siteList))]] %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    dplyr::select(startDateTime, discharge)
    
  ### air pressure 
  airDf = siteList[[grep('airPressure', names(siteList))]] %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    dplyr::select(startDateTime, air.pressure)
    
  
  dfList = list(doDf, tempDf, qDf, airDf)
  
  metDf = dfList %>% 
    reduce(full_join, by='startDateTime') %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    group_by(startDateTime) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))
  
  metDf = metDf %>% 
    dplyr::mutate(depth = streamMetabolizer::calc_depth(discharge),
                  solar.time = streamMetabolizer::calc_solar_time(startDateTime,
                                                                  longitude = unlist(
                                                                    siteLatLong$long)),
                  light = streamMetabolizer::calc_light(solar.time,
                                                        longitude = unlist(
                                                          siteLatLong$long),
                                                        latitude = unlist(
                                                          siteLatLong$lat)),
                  depth = zoo::na.approx(depth, maxgap = 4, na.rm = FALSE),
                  temp.water = zoo::na.approx(temp.water, maxgap = 2, na.rm = FALSE),
                  discharge = zoo::na.approx(discharge, maxgap= 4, na.rm = FALSE)) %>%
    dplyr::mutate(DO.sat = streamMetabolizer::calc_DO_sat(temp.water = temp.water,
                                                          pressure.air = air.pressure*10),
                  DO.sat = zoo::na.approx(DO.sat, maxgap = 2, na.rm = FALSE),
                  DO.pctsat = 100*(DO.obs/DO.sat)) %>%
    dplyr::select(solar.time, DO.obs, DO.sat, DO.pctsat, depth,
                  temp.water, light, discharge)
  
  return(metDf)
              
  
  }


#' @title clean_met_data
#' @description This function subsets all the relevant data products to calculate Q-Z relationships for a site
#' @param siteCode 
#' @param startDate
#' @param endDate
#'

clean_met_data = function(siteData = NULL, doCutOff = 25, doProbThresh = 0.95,... ){
  siteDataMod = siteData %>%
    dplyr::mutate(outQF = case_when(DO.obs >= doCutOff ~ 1,
                                    DO.obs < 0 ~ 1,
                                    DO.pctsat >= 150 ~ 1,
                                    DO.pctsat <= 70 ~ 1,
                                     TRUE ~ 0),
                  probQF = case_when(!between(DO.obs, quantile(DO.obs, 0.975, na.rm = TRUE),
                                              quantile(DO.obs, 0.025, na.rm = TRUE)) ~ 1,
                                     TRUE ~ 0),
                  doDiff = c(NA,diff(DO.obs)),
                  doSatDiff = c(NA, diff(DO.pctsat))) %>%
    dplyr::filter(!is.na(solar.time)) %>%
    dplyr::filter(abs(doDiff) <= 5 & abs(doSatDiff) <= 5)

  return(siteDataMod)
}

#' @title plot_site
#'
#'

#'
#'
#'
plot_site = function(siteCode = NULL,...){
  library(tidyverse)
  site_file = list.files(path = "./data/derived_data/clean-met-files/", pattern = siteCode, full.names = TRUE)
  
  if(length(site_file) == 0) return(NA)
  
  siteDf = lapply(site_file, readRDS) %>%
    flatten %>% data.frame %>%
    dplyr::select(solar.time,DO.obs,depth,temp.water,discharge) %>%
    pivot_longer(-solar.time, names_to = 'variable', values_to = 'value')
  
  theme_set(theme_minimal())
  siteDf %>%
    ggplot()+
    ggtitle(siteCode)+
    geom_path(aes(x = solar.time, y = value))+
    facet_grid(rows = 'variable', scales = 'free_y')
  
  }
# 
# streams = readRDS(file = "./data/derived_data/streams.rds")
# pdf("./plots/site-data-streams.pdf")
# lapply(streams, function(x) plot_site(x))
# dev.off()
# 
# debugonce(plot_site)
# plot_site('KING')





