# custom functions for prepping data products for metabolism models
#' Slice min and max rows
#'
#' Select rows of highest and lowest values based on variables 
#'
#' @param df data
#' 
#' @param order_by variable to order by
#' 
#' @param n number of rows to select, default is 1
#' 
#' @author Shona Wilde
#' 
#' @return tibble
#' 
#' @export
slice_min_max <- function(df, order_by = value, n = 1) {
  
  order_by = enquo(order_by)
  
  min <- slice_min(df, !!order_by, n = n) %>%
    mutate(type = "min")
  
  max <- slice_max(df, !!order_by, n = n) %>%
    mutate(type = "max")
  
  df <- bind_rows(min, max) %>%
    as_tibble()
  
  return(df)
  
}

#' @title clean_DO
#'
#'
clean_DO = function(siteCode = NA,startDate = NULL, endDate = NULL, doLims = c(0,30), doDiffLims = c(0.001,0.999), return = TRUE, save = TRUE,...){
  require(tidyverse)
  require(lutz)
  library(lubridate)
  
  latlong = read_csv(file = "./data/site_latlong.csv")
  siteLatLong = latlong[which(latlong$site == siteCode),]
  
  ## estimate the timezone 
  siteTZ = lutz::tz_lookup_coords(lat = unlist(latlong[which(latlong$site == siteCode), 'lat']),
                                  lon = unlist(latlong[which(latlong$site == siteCode), 'long']),
                                  method = 'accurate')
  ## Get site files
  fileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,"_DO.rds"), full.names = TRUE)
  DOList = fileList %>%
    purrr::map(readRDS)
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
  
  ### DO
  sensorNames = DOList %>% flatten %>% names(.)
  
  doDfmod = DOList %>% 
    flatten %>%
    purrr::map(~.x %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ),
                  startDateTime = ceiling_date(startDateTime, 'minute')) %>%
    dplyr::select(startDateTime, DO.obs) %>%
    dplyr::mutate(diff = c(NA,diff(DO.obs))) %>%
    dplyr::mutate(DO.obs = ifelse(between(diff, quantile(diff, doDiffLims[1], na.rm = TRUE), quantile(diff, doDiffLims[2], na.rm = TRUE)), DO.obs, NA)) %>%
    dplyr::select(-diff)) %>%
    reduce(merge, by = 'startDateTime', all = TRUE) %>%
    setNames(.,nm = c('startDateTime', paste0("DO_",sensorNames)))
  
  doFullDf = doDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime, tz = siteTZ)) %>%
    group_by(day) %>%
    dplyr::mutate(fullFlag = case_when(all(is.na(across(matches('DO')))) ~ FALSE,
                                       TRUE ~ TRUE)) %>%
    dplyr::select(day, fullFlag) %>%
    group_by(day) %>%
    dplyr::summarise(fullFlag = unique(fullFlag))
  
  # combine and remove days with all NAs  
  doDf = doDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime)) %>%
    left_join(doFullDf, by = 'day') %>%
    dplyr::filter(fullFlag) %>%
    dplyr::filter(!is.na(startDateTime)) %>%
    dplyr::select(startDateTime, day, matches('DO'))
  
  
  doDates =  doDf%>% dplyr::select(day) %>% reduce(c) %>% range %>% purrr::map(~paste0(.x, "00:00:00") %>% as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>% reduce(c)
  fullTime = data.frame(startDateTime = seq(doDates[1], doDates[2], by = 'mins'))

  doDf = fullTime %>%
    left_join(doDf, by = 'startDateTime') %>%
    dplyr::mutate(across(matches('DO'), ~ifelse(between(.x, doLims[1],doLims[2]), .x, NA))) %>%
    dplyr::mutate(across(matches('DO'), ~zoo::na.approx(.x, maxgap = 100, na.rm = FALSE))) %>%
    dplyr::mutate(timePeriod = cut(startDateTime, breaks = "15 min")) %>%
    group_by(timePeriod) %>%
    dplyr::summarise(across(matches('DO'), ~mean(.x, na.rm = TRUE))) %>% 
    dplyr::mutate(timePeriod = as.POSIXct(timePeriod, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    ungroup %>%
    dplyr::mutate(hour = lubridate::hour(timePeriod))

    missingDO = doDf %>% dplyr::mutate(missing := case_when(is.na(!!as.name(paste0("DO_",sensorNames[length(sensorNames)]))) & any(!is.na(!!as.name(paste0("DO_",sensorNames[(length(sensorNames)-1)])))) ~ 1,
                                                                       TRUE ~ 0)) %>%
    dplyr::select(missing) %>% unlist %>% sum
  
  cat("\nNumber of missing observations in downstream sensor with non-na in other sensors is ",missingDO)
  
  if(save){
  saveRDS(doDf, file = paste0("./ignore/site-gpp-data/",siteCode,"_clean_DO.rds"))
  }
  
  if(return){
  return(doDf)
  }
}

#' @title clean_temp
#'
#'
clean_temp = function(siteCode = NA,startDate = NULL, endDate = NULL, tempLims = c(0,50), return = TRUE, save = TRUE,...){
  require(tidyverse)
  require(lutz)
  library(lubridate)
  
  latlong = read_csv(file = "./data/site_latlong.csv")
  siteLatLong = latlong[which(latlong$site == siteCode),]
  
  ## estimate the timezone 
  siteTZ = lutz::tz_lookup_coords(lat = unlist(latlong[which(latlong$site == siteCode), 'lat']),
                                  lon = unlist(latlong[which(latlong$site == siteCode), 'long']),
                                  method = 'accurate')
  ## Get site files
  fileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,".*30min_temp.rds"), full.names = TRUE)
  tempList = fileList %>%
    purrr::map(readRDS)
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
  
  ### temp
  sensorNames = tempList %>% flatten %>% names(.)
  
  tempDfmod = tempList %>% 
    flatten %>%
    purrr::map(~.x %>%
                 plyr::rename(replace = columnKeyVal,
                              warn_missing = FALSE) %>%
                 dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ),
                               startDateTime = ceiling_date(startDateTime, 'minute')) %>%
                 dplyr::select(startDateTime, temp.water)) %>%
    reduce(merge, by = 'startDateTime', all = TRUE) %>%
    setNames(.,nm = c('startDateTime', paste0("temp_",sensorNames)))
  
  tempFullDf = tempDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime, tz = siteTZ)) %>%
    group_by(day) %>%
    dplyr::mutate(fullFlag = case_when(all(is.na(across(matches('temp')))) ~ FALSE,
                                       TRUE ~ TRUE)) %>%
    dplyr::select(day, fullFlag) %>%
    group_by(day) %>%
    dplyr::summarise(fullFlag = unique(fullFlag))
  
  # combine and remove days with all NAs  
  tempDf = tempDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime)) %>%
    left_join(tempFullDf, by = 'day') %>%
    dplyr::filter(fullFlag) %>%
    dplyr::filter(!is.na(startDateTime)) %>%
    dplyr::select(startDateTime, day, matches('temp'))
  
  
  tempDates =  tempDf%>% dplyr::select(day) %>% reduce(c) %>% range %>% purrr::map(~paste0(.x, "00:00:00") %>% as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>% reduce(c)
  fullTime = data.frame(startDateTime = seq(tempDates[1], tempDates[2], by = 'mins'))
  
  tempDf = fullTime %>%
    left_join(tempDf, by = 'startDateTime') %>%
    dplyr::mutate(across(matches('temp'), ~ifelse(between(.x, tempLims[1],tempLims[2]), .x, NA))) %>%
    dplyr::mutate(across(matches('temp'), ~zoo::na.approx(.x, maxgap = 100, na.rm = FALSE))) %>%
    dplyr::mutate(timePeriod = cut(startDateTime, breaks = "15 min")) %>%
    group_by(timePeriod) %>%
    dplyr::summarise(across(matches('temp'), ~mean(.x, na.rm = TRUE))) %>% 
    dplyr::mutate(timePeriod = as.POSIXct(timePeriod, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    ungroup %>%
    dplyr::mutate(hour = lubridate::hour(timePeriod))
  
  missingTEMP = tempDf %>% dplyr::mutate(missing := case_when(is.na(!!as.name(paste0("temp_",sensorNames[length(sensorNames)]))) & any(!is.na(!!as.name(paste0("temp_",sensorNames[(length(sensorNames)-1)])))) ~ 1,
                                                          TRUE ~ 0)) %>%
    dplyr::select(missing) %>% unlist %>% sum
  
  cat("\nNumber of missing observations in downstream sensor with non-na in other sensors is ",missingTEMP)
  if(save){
    saveRDS(tempDf, file = paste0("./ignore/site-gpp-data/",siteCode,"_clean_temp.rds"))
  }
  if(return) return(tempDf)
}

#' @title clean_Q
#'
#'
clean_Q = function(siteCode = NA,startDate = NULL, endDate = NULL, QLims = c(0,10e5), QDiffLims = c(0.001,0.999), return = TRUE, save = TRUE,...){
  require(tidyverse)
  require(lutz)
  library(lubridate)
  
  latlong = read_csv(file = "./data/site_latlong.csv")
  siteLatLong = latlong[which(latlong$site == siteCode),]
  
  ## estimate the timezone 
  siteTZ = lutz::tz_lookup_coords(lat = unlist(latlong[which(latlong$site == siteCode), 'lat']),
                                  lon = unlist(latlong[which(latlong$site == siteCode), 'long']),
                                  method = 'accurate')
  ## Get site files
  QfileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,"_dischargeQ.rds"), full.names = TRUE)
  ZfileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,"_30min_depthZ.rds"), full.names = TRUE)
  QList = QfileList %>%
    purrr::map(readRDS)
  ZList = ZfileList %>% 
    purrr::map(readRDS)
  oldColNames = c(
    "timePeriod",
    "maxpostDischarge",
    "staPresMean",
    "surfacewaterElevMean"
  )
  
  newColNames = c(
    "startDateTime",
    "Q.obs",
    "air.pressure",
    "Z.obs"
  )
  
  columnKeyVal = setNames(newColNames,oldColNames)
  
  ### Q
  QDfmod = QList %>% 
    flatten %>%
    data.frame %>% 
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ),
                  startDateTime = ceiling_date(startDateTime, 'minute')) %>%
    dplyr::select(startDateTime, Q.obs, equivalentStage) %>%
    dplyr::mutate(diff = c(NA,diff(Q.obs))) %>%
    dplyr::mutate(Q.obs = ifelse(between(diff, quantile(diff, QDiffLims[1], na.rm = TRUE), quantile(diff, QDiffLims[2], na.rm = TRUE)), Q.obs, NA)) %>%
    dplyr::select(-diff)
  
  ### Z
  ZDfmod = ZList %>% 
    flatten %>%
    data.frame %>% 
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ),
                  startDateTime = ceiling_date(startDateTime, 'minute')) %>%
    dplyr::select(startDateTime, Z.obs) %>%
    dplyr::mutate(diff = c(NA,diff(Z.obs))) %>%
    dplyr::select(-diff)
  ### ZQ 
  QZDfmod = QDfmod %>% 
    left_join(ZDfmod, by = 'startDateTime')
  
  
  QZFullDf = QZDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime, tz = siteTZ)) %>%
    group_by(day) %>%
    dplyr::mutate(fullFlag = case_when(all(is.na(across(matches('Q')))) ~ FALSE,
                                       TRUE ~ TRUE)) %>%
    dplyr::select(day, fullFlag) %>%
    group_by(day) %>%
    dplyr::summarise(fullFlag = unique(fullFlag))
  
  # combine and remove days with all NAs  
  QZDf = QZDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime)) %>%
    left_join(QZFullDf, by = 'day') %>%
    dplyr::filter(fullFlag) %>%
    dplyr::filter(!is.na(startDateTime)) %>%
    dplyr::select(startDateTime, day, matches('Q|Z'))
  
  
  QZDates =  QZDf%>% dplyr::select(day) %>% reduce(c) %>% range %>% purrr::map(~paste0(.x, "00:00:00") %>% as.POSIXct(., format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>% reduce(c)
  fullTime = data.frame(startDateTime = seq(QZDates[1], QZDates[2], by = 'mins'))
  
  QZDf = fullTime %>%
    left_join(QZDf, by = 'startDateTime') %>%
    dplyr::mutate(across(matches('Q'), ~ifelse(between(.x, QLims[1],QLims[2]), .x, NA))) %>%
    dplyr::mutate(across(matches('Q|Z'), ~zoo::na.approx(.x, maxgap = 100, na.rm = FALSE))) %>%
    dplyr::mutate(timePeriod = cut(startDateTime, breaks = "15 min")) %>%
    group_by(timePeriod) %>%
    dplyr::summarise(across(matches('Q|Z'), ~mean(.x, na.rm = TRUE))) %>% 
    dplyr::mutate(timePeriod = as.POSIXct(timePeriod, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    ungroup %>%
    dplyr::mutate(hour = lubridate::hour(timePeriod))
  
  if(save){
    saveRDS(QZDf, file = paste0("./ignore/site-gpp-data/",siteCode,"_clean_ZQ.rds"))
  }
  
  if(return){
    return(QZDf)
  }
}

#' @title get_site_data
#' @description This function subsets all the relevant data products to estimate stream metabolism  for a site
#' @param sitecode character. Character string representing the stream identification code 
#' @param startDate character. Character coercible to Date, representing start date of data series. If NULL (default), the entire data series are downloaded.
#' @param endDate character. Character, coercible to Date, representing end data of data series. If NULL (default), the entire data series are downloaed.
#'

get_site_data = function(siteCode = NA, startDate = NULL, endDate = NULL,...){
  require(tidyverse)
  require(streamMetabolizer)
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
    unlist %>%
    lapply(., function(x) gsub(".rds","",x)) %>% unlist
  
  ## Read in the files and set names
  siteList = fileList %>%
    purrr::map(readRDS) %>% setNames(., fileNames)
  
  oldColNames = list(
    "timePeriod",
    c("dissolvedOxygen","DO_102"),
    c("surfWaterTempMean","temp_102"),
    "maxpostDischarge",
    "staPresMean"
  )
  
  newColNames = list(
    "startDateTime",
    "DO.obs",
    "temp.water",
    "discharge",
    "air.pressure"
  )

  columnKeyVal = setNames(rep(newColNames,lengths(oldColNames)), unlist(oldColNames))

  ## Convert all the data sets
  # grab the highest sensor number
  # downstreamStation = siteList[[grep('clean_DO', names(siteList))]] %>%
  #   .[,c(dim(.)[2]-1)] %>% names(.) 
  # grab sensor at position 
  downstreamStation = siteList[[grep('clean_DO', names(siteList))]] %>%
    .[,"DO_102"] %>% names(.)
  # startEndDf = siteList[[grep('clean_DO', names(siteList))]] %>%
  #   dplyr::select("timePeriod") %>%
  #   plyr::rename(replace = columnKeyVal,
  #                warn_missing = FALSE) %>%
  #   dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ), row = 1:n()) %>% 
  #   slice_min_max(row) %>%
  #   dplyr::select(startDateTime) 
  # # create full timeseries
  #   timeDf = data.frame(startDateTime = seq(startEndDf[1], startEndDf[2], by = "mins"));rm(startEndDf)

      ### DO
  doDfmod = siteList[[grep('clean_DO', names(siteList))]] %>%
    dplyr::select(all_of(c("timePeriod", downstreamStation))) %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    dplyr::select(startDateTime, DO.obs)

  # determine when days are all NAs
  doFullDf = doDfmod %>%
    dplyr::mutate(day = as.Date(startDateTime, tz = siteTZ)) %>%
    group_by(day) %>%
    dplyr::mutate(fullFlag = case_when(all(is.na(DO.obs)) ~ FALSE,
                                       TRUE ~ TRUE)) %>%
    ungroup %>%
    dplyr::select(-DO.obs, -day)

  # combine and remove days with all NAs  
  doDf = doDfmod %>%
    left_join(doFullDf) %>%
    dplyr::filter(fullFlag) %>%
    dplyr::filter(!is.na(startDateTime)) %>%
    dplyr::select(startDateTime, DO.obs)
  ### temperature
  # downstreamStation = siteList[[grep('clean_temp', names(siteList))]] %>%
  #   .[,c(dim(.)[2]-1)] %>% names(.) 
  downstreamStation = siteList[[grep('clean_temp', names(siteList))]] %>%
    .[,"temp_102"] %>% names(.)
  tempDf = siteList[[grep('clean_temp', names(siteList))]] %>%
    dplyr::select(all_of(c("timePeriod", downstreamStation))) %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) %>%
    dplyr::select(startDateTime, temp.water)
  
  ### discharge
  qDf = siteList[[grep('dischargeQ', names(siteList))]] %>%
    dplyr::mutate(timePeriod = cut(startDateTime, breaks = '15 mins')) %>%
    group_by(timePeriod) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>% 
    ungroup %>% 
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
    reduce(merge, by='startDateTime', all = TRUE) %>%
    dplyr::mutate(day = as.Date(startDateTime, tz = siteTZ)) %>%
    group_by(day) %>%
    dplyr::mutate(fullFlag = ifelse(all(is.na(across(where(is.numeric)))), FALSE, TRUE)) %>%
    ungroup %>%
    dplyr::select(-day) %>%
    dplyr::filter(fullFlag) %>%
    dplyr::mutate(timePeriod = cut(startDateTime, breaks = '15 mins')) %>%
    group_by(timePeriod) %>%
    dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE))) %>%
    plyr::rename(replace = columnKeyVal,
                 warn_missing = FALSE) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ)) #%>%
    # dplyr::filter_at(vars(DO.obs, temp.water, discharge, air.pressure), all_vars(!is.na(.))) #%>%
    # group_by(startDateTime) %>%
    # dplyr::summarise(across(where(is.numeric), ~mean(.x, na.rm = TRUE)))

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
                  depth = zoo::na.approx(depth, maxgap = 700, na.rm = FALSE),
                  temp.water = zoo::na.approx(temp.water, maxgap = 16, na.rm = FALSE),
                  discharge = zoo::na.approx(discharge, maxgap= 700, na.rm = FALSE),
                  air.pressure = zoo::na.approx(air.pressure, maxgap = 700, na.rm = FALSE)) %>%
    dplyr::mutate(air.pressure = case_when(!all(is.na(temp.water) & !is.na(DO.obs) & is.na(air.pressure)) ~ mean(air.pressure, na.rm = TRUE),
                                           TRUE ~ air.pressure),
                  DO.sat = calc_DO_sat(temp.water = temp.water,
                                                          pressure.air = air.pressure*10),
                  DO.sat = zoo::na.approx(DO.sat, maxgap = 16, na.rm = FALSE),
                  DO.pctsat = 100*(DO.obs/DO.sat)) %>%
    dplyr::select(solar.time, DO.obs, DO.sat, DO.pctsat, depth,
                  temp.water, light, discharge) %>%
    dplyr::filter(!is.na(solar.time))#vars(DO.obs, DO.sat, DO.pctsat, depth,
                          # temp.water, light, discharge), all_vars(!is.na(.)))  
  return(metDf)
              
  
  }


#' @title clean_met_data
#' @description This function subsets all the relevant data products to calculate Q-Z relationships for a site
#' @param siteCode 
#' @param startDate
#' @param endDate
#'

clean_met_data = function(siteData = NULL, doCutOff = 25, doProbThresh = 0.95, tempCutOff = 40, doPctCutoff = c(70,150),... ){
  siteDataMod = siteData %>%
    dplyr::mutate(outQF = 0,
                  outQF = case_when(DO.obs >= doCutOff ~ 1,
                                    DO.obs < 0 ~ 1,
                                    TRUE ~ outQF),
                  outQF = case_when(DO.pctsat <= doPctCutoff[1] ~ 1,
                                    DO.pctsat >= doPctCutoff[2] ~ 1,
                                    TRUE ~ outQF),
                  outQF = case_when(temp.water >= tempCutOff ~ 1,
                                     TRUE ~ outQF),
                  probQF = case_when(!between(DO.obs, quantile(DO.obs, 0.975, na.rm = TRUE),
                                              quantile(DO.obs, 0.025, na.rm = TRUE)) ~ 1,
                                     TRUE ~ 0),
                  doDiff = c(NA,diff(DO.obs)),
                  doSatDiff = c(NA, diff(DO.pctsat))) %>%
    dplyr::filter(!is.na(solar.time)) %>%
    dplyr::mutate(DO.obs = case_when(abs(doSatDiff) > 10 ~ NA_real_,
                                     TRUE ~ DO.obs)) %>%
    dplyr::mutate(DO.obs = zoo::na.approx(DO.obs, maxgap = 13, na.rm = FALSE))

  return(siteDataMod)
}

#' @title plot_site
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

#' @title plot_met_series
#'
#'
plot_met_series = function(df = NULL, selCols = c("solar.time","DO.obs","DO.pctsat","temp.water","discharge"),...){
  require(ggplot2)
  require(magrittr)
  require(dplyr)
  require(plyr)
  require(viridis)
  plotDf = df %>%
    dplyr::select(any_of(selCols)) %>%
    pivot_longer(c(-solar.time), names_to = 'variable', values_to = 'value')
  
  p = plotDf %>%
    ggplot()+
    geom_line(aes(x = solar.time, y = value, color = variable)) +
    scale_color_viridis(discrete = TRUE) +
    facet_wrap(~variable, ncol = 1, scales = 'free_y') + 
    theme_minimal()+
    theme(axis.title = element_blank(),
          axis.text = element_text(size = 16),
          legend.title = element_blank())
  
  return(p)
}

#' @title run_metab_multimodel
#'
#'
run_metab_multimodel = function(siteCode = NULL, metDf = NULL, kLim = 4e+05, return = FALSE, save = TRUE, parallel = FALSE, badDates = NULL, refitKmod = FALSE,...){
  if(!is.null(siteCode) & is.null(metDf)){
  metFull = get_site_data(siteCode) %>%
    dplyr::select(-DO.pctsat)
  if(!is.null(badDates)){
    metFull = metFull %>%
      dplyr::filter(as.Date(solar.time) %ni% as.Date(badDates))
  }
  } else if(is.null(siteCode) & !is.null(metDf)){
    warning("Warning: No `siteCode` provided. No empirical K-model will be used.")
    metFull = metDf
  } else if(is.null(siteCode) & is.null(metDf)){
    stop("Error: no `siteCode` or `metDf` provided")
  } else if(!is.null(siteCode) & !is.null(metDf)){
    warning("Warning: both `siteCode` and `metDf` are provided. We will consider `metDf` as the data and use `siteCode` to search for empirical K-model.")
    metFull = metDf
  }
  
  # Empirical K-model check
  kmodName = paste0("./ignore/site-gpp-data/",siteCode,"_kGAM.rds")
  
  kmod = tryCatch(
    {
    readRDS(kmodName)
    },
    error = function(cond){
      message("There is no empirical K model.")
      return(NULL)
    })
  if(is.null(kmod)|is.character(kmod)){
    warning("No empirical K models were found.")
    discharge.daily = metFull %>%
      dplyr::mutate(date = as.Date(solar.time)) %>%
      group_by(date) %>%
      dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE),
                       model = NA) 
    discharge.daily$K600.daily = NA
  } else if(any(grepl("gam", class(kmod)))){
    
    discharge.daily = metFull %>%
      dplyr::mutate(date = as.Date(solar.time)) %>%
      group_by(date) %>%
      dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE),
                       model = 'empirical-gam') 
    
    discharge.daily$K600.daily = exp(predict(kmod, newdata = discharge.daily))
    
  } else if(all(grepl("lm", class(kmod)))){
    if(any(grepl('log', rlang::f_rhs(kmod$terms)))){
      discharge.daily = metFull %>%
        dplyr::mutate(date = as.Date(solar.time)) %>%
        group_by(date) %>%
        dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE),
                         model = 'empirical-lm') 
      
      discharge.daily$K600.daily = exp(predict(kmod, newdata = discharge.daily))
    } else{
      discharge.daily = metFull %>%
        dplyr::mutate(date = as.Date(solar.time)) %>%
        group_by(date) %>%
        dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE),
                         model = 'empirical-lm') 
      
      discharge.daily$K600.daily = predict(kmod, newdata = discharge.daily)
    }
    
  }
  
  ## set all mle model specs
  mle_specs <- specs(mm_name(type = "mle"))
  mle_specs_sat <- specs(mm_name(type = "mle", GPP_fun = 'satlight'))
  mle_specs_satq10 <- specs(mm_name(type = "mle", GPP_fun = 'satlight', ER_fun = 'q10temp'))
  
  # run models on raw data
  mm1 <- metab_mle(mle_specs, data = metFull, info = list(name = "mm1", model = "raw"))
  mm1_sat <- metab_mle(mle_specs_sat, data = metFull, info = list(name = "mm1_sat", model = "raw"))
  mm1_satq10 <- metab_mle(mle_specs_satq10, data = metFull, info = list(name = "mm1_satq10", model = "raw"))

  # fit different k models
  k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
    select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
    left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
  
  km1 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                            day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
  km2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                            day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
  km3 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
                            day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
  km4 <- metab_night(data = metFull)
  # 
  k600_mm2 <- get_params(km1) %>% 
    select(date, K600.daily) %>%  
    dplyr::mutate(model = 'loess') %>%
    bind_rows(get_params(km2) %>%
                select(date, K600.daily) %>%  
                dplyr::mutate(model = 'lm')) %>%
    bind_rows(get_params(km3) %>%
                select(date, K600.daily) %>%
                dplyr::mutate(model = 'mean')) %>%
    bind_rows(get_params(km4) %>%
                select(date, K600.daily) %>%
                dplyr::mutate(model = 'night')) %>%
    bind_rows(discharge.daily %>% select(date, model, K600.daily)) %>%
    left_join(discharge.daily %>% dplyr::select(date, discharge.daily))

  # fit metab models with varying k models
  # if(parallel){
  #   modelNames = c('mm2','mm3','mm4','mm5')
  #   modelTypes = c('loess','lm','mean','night')
  #   mmList = furrr::future_map2(modelTypes,modelNames, ~metab_mle(mle_specs, data = metFull,data_daily = k600_mm2 %>% dplyr::filter( model == .x) %>% dplyr::select(date, K600.daily), info = list(name = .y, model = .x)))
  #   return(mmList)
  # } else{
  mm2 <- metab_mle(mle_specs, data = metFull, data_daily = k600_mm2 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = "mm2", model = "loess"))
  
  mm3 <- metab_mle(mle_specs, data = metFull, data_daily = k600_mm2 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily),info = list(name = "mm3", model = "lm"))
  
  mm4 <- metab_mle(mle_specs, data = metFull, data_daily = k600_mm2 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = "mm4", model = "mean"))
  
  mm5 <- metab_mle(mle_specs, data = metFull, data_daily = k600_mm2 %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = "mm5", model = "night"))
  # return(list(mm2,mm3,mm4,mm5))
  # }
  #fit saturation models
  k600_mm1_sat <- get_params(mm1_sat, uncertainty = 'ci') %>%
    left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
    select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
    left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
  
  km1_sat <- tryCatch({
    metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                                day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))
  }, error = function(e){
    metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                       day_start = -1, day_end = 23), data_daily = k600_mm1%>% select(-GPP.daily))
    })
  
km2_sat <- tryCatch({
    metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                                day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))
  }, error = function(e){
    metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                       day_start = -1, day_end = 23), data_daily = k600_mm1%>% select(-GPP.daily))
  })
  
  km3_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
                                day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))
  
  # 
  k600_mm2_sat <- get_params(km1_sat) %>% 
    select(date, K600.daily) %>%  
    dplyr::mutate(model = 'loess') %>%
    bind_rows(get_params(km2_sat) %>%
                select(date, K600.daily) %>%  
                dplyr::mutate(model = 'lm')) %>%
    bind_rows(get_params(km3_sat) %>%
                select(date, K600.daily) %>%
                dplyr::mutate(model = 'mean')) %>%
    bind_rows(get_params(km4) %>%
                select(date, K600.daily) %>%
                dplyr::mutate(model = 'night')) %>%
    bind_rows(discharge.daily %>% select(date, model, K600.daily)) %>%
    left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
  
  mm2_sat <- metab_mle(mle_specs_sat, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = "mm2_sat", model = "loess"))
  
  mm3_sat <- metab_mle(mle_specs_sat, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily),info = list(name = "mm3_sat", model = "lm"))
  
  mm4_sat <- metab_mle(mle_specs_sat, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = "mm4_sat", model = "mean"))
  
  mm5_sat <- metab_mle(mle_specs_sat, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = "mm5_sat", model = "night"))
  
  ## fit saturation + q10 models
  # identify negative values
  k600_mm1_satq10 <- get_params(mm1_satq10, uncertainty = 'ci') %>%
    left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
    select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
    left_join(discharge.daily %>% dplyr::select(date, discharge.daily)) 
  
  k600_mm1_satq10 = k600_mm1_satq10 %>%
    dplyr::filter(K600.daily < kLim,
                  GPP.daily > 0)
  # 
  km1_satq10 <- tryCatch({
    metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                                   day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))
  }, error = function(e){
    metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                       day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
  })
  
km2_satq10 <- tryCatch({
  metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                                   day_start = -1, day_end = 23), data_daily = k600_mm1_satq10%>% select(-GPP.daily))
}, error = function(e){
  metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                     day_start = -1, day_end = 23), data_daily = k600_mm1 %>% select(-GPP.daily))
})
  
  km3_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
                                   day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))
  # 
  k600_mm2_satq10 <- get_params(km1_satq10) %>% 
    select(date, K600.daily) %>%  
    dplyr::mutate(model = 'loess') %>%
    bind_rows(get_params(km2_satq10) %>%
                select(date, K600.daily) %>%  
                dplyr::mutate(model = 'lm')) %>%
    bind_rows(get_params(km3_satq10) %>%
                select(date, K600.daily) %>%
                dplyr::mutate(model = 'mean')) %>%
    bind_rows(get_params(km4) %>%
                select(date, K600.daily) %>%
                dplyr::mutate(model = 'night')) %>%
    bind_rows(discharge.daily %>% select(date, model, K600.daily)) %>%
    left_join(discharge.daily %>% dplyr::select(date, discharge.daily))
  
 
  mm2_satq10 <- metab_mle(mle_specs_satq10, data = metFull, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily),info = list(name = "mm2_satq10", model = "loess"))
  
  mm3_satq10 <- metab_mle(mle_specs_satq10, data = metFull, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily), info = list(name = "mm3_satq10", model = "lm"))
  
  mm4_satq10 <- metab_mle(mle_specs_satq10, data = metFull, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily),info = list(name = "mm4_satq10", model = "mean"))
  
  mm5_satq10 <- metab_mle(mle_specs_satq10, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily),info = list(name = "mm5_satq10", model = "night"))
  browser()
  # run empirical models if they exist
  if(!any(is.character(kmod) |is.null(kmod))){
    # fit all the empirical models if emprical k model exists
    mm6 <- metab_mle(mle_specs, data = metFull, data_daily = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% dplyr::select(date, K600.daily),info = list(name = "mm6", model = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% select(model) %>% unlist %>% unique))
    
    mm6_sat <- metab_mle(mle_specs_sat, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(grepl('empirical', model)) %>% dplyr::select(date, K600.daily),info = list(name = "mm6_sat", model = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% select(model) %>% unlist %>% unique))
    
    mm6_satq10 <- metab_mle(mle_specs_satq10, data = metFull, data_daily = k600_mm2_sat %>% dplyr::filter(grepl('empirical', model)) %>% dplyr::select(date, K600.daily), info = list(name = "mm6_satq10", model = k600_mm2 %>% dplyr::filter(grepl('empirical', model)) %>% select(model) %>% unlist %>% unique))
  }
  
  # identify fit models
  modList = ls()[grep("^mm\\d{1}.*", ls())] %>% purrr::map(~eval(as.symbol(.x))) 
  
  # save model fits
  if(save){
    save(modList, file = paste0("./ignore/metab-models/mleModLists/",siteCode,"mlemods.Rdata"))
  }
  # return model fits
  if(return){
    return(modList)
  }
}

#'
#'
#'
calc_mod_RMSE = function(metObj = NULL, relative = FALSE, ...){
  data = metObj$data 
  
  if(relative){
    error = data %>%
      dplyr::mutate(diff = mod-obs) %>%
      dplyr::select(diff) %>%
      unlist %>% na.omit
    
    n = length(error)
    rrmse = sqrt(sum((error/mean(data$obs, na.rm = TRUE))^2)/n)*100
    return(round(rrmse,2))
    
  } else{
    error = data %>%
      dplyr::mutate(diff = mod-obs) %>%
      dplyr::select(diff) %>%
      unlist %>% na.omit
    
    n = length(error)
    rmse = sqrt(sum(error^2))/n
  return(rmse)
  }
}

#'
#'
#'
calc_mod_dev = function(metObj = NULL,...){
  data = metObj$data 
  
  dev = NULL

    return(dev)
  }

#'
#'
#'
pick_model = function(df, ...){
  if(any(unlist(df$GPPtot) <= 0)){
    dfMod = df[which(df$GPPtot > 0),]
  } else if(abs(range(df$RMSE)[1] - range(df$RMSE)[2]) < 3){
    topMod = 'mm3'
  } else{
    topMod = df %>% slice_min(RMSE) %>% select(modelID) %>% unlist %>% as.character
  }
  return(topMod)
}

#'
#'
#'
count_negative_dates = function(mm,...){
  df = mm@metab_daily
  gppDf = na.omit(df$GPP)
  negGpp = length(which(gppDf <= 0))
  negGpp
}

#'
#'
#'
count_positive_dates = function(mm,...){
  df = mm@metab_daily
  erDf = na.omit(df$ER)
  posER = length(which(erDf >= 0))
  posER
}

#'
#'
#'
calc_gpp_mean = function(mm, PQ = 1.28, scaler = NULL,...){
  metab = mm@metab_daily

  if(is.null(scaler)){
    gppMean = mean(metab$GPP, na.rm = TRUE) * ((1/PQ)*(12/32))
  } else{
    gppEqn = paste0("(mean(metab$GPP, na.rm = TRUE) * ((1/PQ)*(12/32)))",as.character(scaler))
    gppMean = eval(parse(text = gppEqn))
  }
  
  return(round(gppMean,3))
}

#'
#'
#'
slim_models = function(df,RMSEcutoff = 0.7,...){
  meanGPPunits = grepl("g C m-2 y-1", attr(df$meanGPP,"comment"))
  highRRMSE = quantile(df$RMSE, RMSEcutoff)
  
  
  if(meanGPPunits){
    
    dfSlim = df %>%
      dplyr::filter(between(meanGPP, 0, 5000))
    if(nrow(dfSlim %>%
            dplyr::filter(RMSE < highRRMSE)) < 5){
      return(dfSlim)
    } else{ dfSlim %>%
          dplyr::filter(RMSE < highRRMSE)
      }
  } else{
    dfSlim = df %>%
      dplyr::filter(between(meanGPP, 0, 5000/365))
    if(nrow(dfSlim %>%
            dplyr::filter(RMSE < highRRMSE)) < 5){
      return(dfSlim)
    } else{ dfSlim %>%
        dplyr::filter(RMSE < highRRMSE)
    }
  }
}

#'
#'
#'
calc_max_k = function(mm,...){
  if(all(is.na(mm@fit$K600.daily))){
    k600vec = mm@data_daily$K600.daily[which(mm@data_daily$K600.daily > 0)] %>% na.omit %>% .[!is.infinite(.)]
    round(max(k600vec, na.rm = TRUE),5)
  } else{
    k600vec = mm@fit$K600.daily[which(mm@fit$K600.daily > 0)] %>% na.omit %>% .[!is.infinite(.)]
    round(max(k600vec, na.rm = TRUE),5)
  }
}

#'
#'
#'
# calc K600
calc_k600 = function(df, LRcol = "slopeNormClean",...){
  ##### Constants #####
  #Coefficients for Least Squares Third-Order Polynomial Fits of Schmidt Number Versus Temperature
  #Valid for 0 - 30 Celsius temperature range
  #Table A1, Fresh water, Wanninkhof (1992), DOI: 10.1029/92JC00188
  #See also Jahne et al. (1987), DOI: 10.4236/jep.2014.511103
  A_O2 = 1800.6
  B_O2 = 120.10
  C_O2 = 3.7818
  D_O2 = 0.047608
  
  A_CO2 = 1911.1
  B_CO2 = 118.11
  C_CO2 = 3.4527
  D_CO2 = 0.041320
  
  A_SF6 = 3255.3
  B_SF6 = 217.13
  C_SF6 = 6.8370
  D_SF6 = 0.086070
  
  Sc_CO2 = 600 #Schmidt number of O2 at 20 C in fresh water
  
  convLpsCms = 1/1000 #Conversion from litersPerSecond to cubicMetersPerSecond
  
  #Reaeration Rate Conversion
  #Equation 7, Wanninkhof (1990), DOI: 10.1029/WR026i007p01621
  Sc_O2_25 <- A_O2 - B_O2 * 25 + C_O2 * 25^2 - D_O2 * 25^3
  Sc_SF6_25 <- A_SF6 - B_SF6 * 25 + C_SF6 * 25^2 - D_SF6 * 25^3
  reaRateConv <- (Sc_O2_25/Sc_SF6_25) ^ (-0.5)
  
  df %>%
    dplyr::mutate(kSF6 = !!rlang::sym(LRcol) * (btwStaDist/peakMaxVelocity)*-1*86400) %>%# m^-1 * m/s * -1 for negative slope and 86400 for number of seconds in a day
    dplyr::mutate(kO2 = kSF6 * reaRateConv * meanDepth) %>%  #Calculate the gas transfer velocity for oxygen# d^-1 * m
    dplyr::mutate(sc02 = A_O2 - B_O2 * meanTemp + C_O2 * meanTemp^2 - D_O2 * meanTemp^3,#Normalize to schmidt number of 600
           k = (Sc_CO2/sc02)^(-0.5)* kO2,#Equation 1, Wanninkhof (1992) "little k"
           K600 = k/meanDepth) %>% # d^-1 "Big K"
    dplyr::mutate(across(c(k,K600), ~ifelse(.x < 0, NA, .x)))
  
}

#' @title get_metab_info
#' @param mm mixing model object.
#' @param values string. string of 
get_metab_info = function(mm = NULL, values = NULL,...){
  if(is.character(mm)) mm = as.symbol(mm)
  if(is.null(mm@info)) stop("'Info' slot of mm is empty")
  mmInfo = mm@info
  if(is.null(values)){
    mmInfoDf = bind_rows(mmInfo)
    return(mmInfoDf)
  } else{
    mmInfoDf = bind_rows(mmInfo) %>% select(any_of(values))
    return(mmInfoDf)
  }
}

#' @title quiet
#' @description 
#' `quiet` stops output of all warnings and messages of a function
#'
#'
quiet <- function(x) { 
  sink(tempfile()) 
  on.exit(sink()) 
  invisible(force(x)) 
}

#'
#'
#'
#'
corrER_K = function(mm,...){
  
  if(all(is.na(mm@fit$K600.daily))){
    mmDf= mm@metab_daily %>%
      dplyr::select(date, ER) %>%
      left_join(mm@data_daily %>% dplyr::select(date, k600 = 'K600.daily'), by = 'date') %>%
      na.omit
  } else{
    
    mmDf= mm@metab_daily %>%
      dplyr::select(date, ER) %>%
      left_join(mm@fit %>% dplyr::select(date, k600 = 'K600.daily'), by = 'date') %>%
      na.omit
  }
  cor(mmDf$ER,mmDf$k600, use = 'complete.obs',...)
}

#'
#'
#'
#'
find_best_fit = function(mmDf = NULL,...){
  minRMSE = which(mmDf$RMSE == min(mmDf$RMSE))
  if(length(minRMSE) == 1){
    return(mmDf[minRMSE,])
  } else{
    slimDf1 = mmDf[minRMSE,]
    minNegGPP = which(slimDf1$negativeGPP == min(slimDf1$negativeGPP))
    if(length(minNegGPP) == 1){
      return(slimDf1[minNegGPP,])
    } else{
        slimDf2 = slimDf1[minNegGPP,]
        minPosER = which(slimDf2$positiveER == min(slimDf2$positiveER))
        if(length(minPosER) == 1){
          return(slimDf2[minPosER,])
        } else{return(slimDf2[runif(1,min = 1, max = nrow(slimDf2)),])}
      }
  }
  
}

#'
#'
#'
weight_model_preds = function(mm = NULL, weight = NULL,...){
  modName = get_metab_info(mm, values = 'name')
  
  modWtDf = mm@metab_daily %>% 
    as.data.frame %>% 
    dplyr::select(date, matches("GPP")) %>% 
    dplyr::mutate(GPP = case_when(GPP > 100 ~ NA_real_,
                                  GPP <= 0 ~ NA_real_,
                                  TRUE ~ GPP)) %>% 
    dplyr::mutate(GPP.lower = as.numeric(GPP.lower),
                  GPP.lower = case_when(is.na(GPP) ~ NA_real_,
                                        is.na(GPP.lower) ~ NA_real_,
                                        GPP.lower <= 0 ~ 0.001,
                                        TRUE ~ GPP.lower)) %>% 
    dplyr::mutate(GPP.upper = as.numeric(GPP.upper),
                  GPP.upper = case_when(is.na(GPP) ~ NA_real_,
                                        is.na(GPP.upper) ~ NA_real_,
                                        GPP.upper <= 0 ~ 0.1,
                                        TRUE ~ GPP.upper)) %>% 
    dplyr::mutate(across(matches("GPP"), ~.x*weight)) %>% 
    dplyr::rename_with(~str_c(as.character(modName),"_",.), .cols = matches("GPP")) %>% 
    group_by(date) %>% 
    pivot_longer(-date, names_to = "variable", values_to = "value")
  
  return(modWtDf)

}

#'
#'
#'
build_ensemble_mod = function(mmDf = NULL, ...){

  modsDf = mmDf %>% 
    dplyr::mutate(dRMSE = min(RMSE, na.rm = TRUE) - RMSE,
                  RMSEwt = exp(0.5*dRMSE)/sum(exp(0.5*dRMSE), na.rm = TRUE))
  if(round(sum(modsDf$RMSEwt, na.rm = TRUE),4) != 1) stop("Error: weights do not sum to one (1).")
  
  mods = unlist(modsDf$modelID)
  weights = unlist(modsDf$RMSEwt)
  
  ensembleList = purrr::map2(mods, weights, ~weight_model_preds(mm = eval(as.symbol(.x)), weight = .y)) 
  
keepNAsum = function(x = NULL,...){
  if(all(is.na(x))){
    y = NA_real_
  } else{
    y = sum(x, na.rm = TRUE)
  }
}
  
  ensemble = ensembleList %>% 
    bind_rows %>% 
    dplyr::mutate(statValue = gsub(".*_(GPP.*)$", "\\1", variable)) %>% 
    group_by(date, statValue) %>% 
    dplyr::summarise(value = keepNAsum(value)) %>% 
    group_by(date) %>% 
    pivot_wider(names_from = "statValue", values_from = "value") %>% 
    dplyr::select(date, GPP, GPP.lower, GPP.upper)

  return(ensemble)
  
}
