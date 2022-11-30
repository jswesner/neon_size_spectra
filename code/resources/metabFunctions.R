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

#' @name clean_DO
#'
#'

clean_DO = function(siteCode = NA,startDate = NULL, endDate = NULL, doLims = c(0,30), return = TRUE,...){
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
  fileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,".*DO.*.rds"), full.names = TRUE)
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
    dplyr::select(startDateTime, DO.obs)) %>%
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
  
  saveRDS(doDf, file = paste0("./ignore/site-gpp-data/",siteCode,"_clean_DO.rds"))
  
  if(return){
  return(doDf)
  }
}

clean_temp = function(siteCode = NA,startDate = NULL, endDate = NULL, tempLims = c(0,50), return = TRUE,...){
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
  fileList = list.files("./ignore/site-gpp-data/", paste0(siteCode,".*temp.*.rds"), full.names = TRUE)
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
  
  saveRDS(tempDf, file = paste0("./ignore/site-gpp-data/",siteCode,"_clean_temp.rds"))
  if(return) return(tempDf)
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


#'
#'
#'
plot_met_series = function(df = NULL, selCols = c("solar.time","DO.obs","DO.pctsat","temp.water","discharge"),...){
  require(ggplot2)
  require(magrittr)
  require(plyr)
  require(dplyr)
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

#'
#'
#'
calc_mod_RSME = function(metObj = NULL, relative = FALSE, ...){
  data = metObj$data 
  error = data %>%
    dplyr::mutate(diff = obs-mod,
                  diff2 = diff^2) %>%
    dplyr::select(diff2) %>%
    unlist %>% na.omit
  
  n = length(error)
  if(relative){
    rrsme = (sqrt(sum(error)/n))/mean(data$obs, na.rm = TRUE)
    return(rrsme)
    
  } else{
    rsme = sqrt(sum(error)/n)
  return(rsme)
  }
}

#'
#'
#'
pick_model = function(df, ...){
  if(any(unlist(df$GPPtot) <= 0)){
    dfMod = df[which(df$GPPtot > 0),]
  } else if(abs(range(df$RSME)[1] - range(df$RSME)[2]) < 3){
    topMod = 'mm3'
  } else{
    topMod = df %>% slice_min(RSME) %>% select(modelID) %>% unlist %>% as.character
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
  gppTot = sum(metab$GPP, na.rm = TRUE)
  days = length(which(!is.na(metab$GPP)))
  
  if(is.null(scaler)){
    gppMean = gppTot/days * ((1/PQ)*(12/32))
  } else{
    gppEqn = paste0("(gppTot/days * ((1/PQ)*(12/32)))",as.character(scaler))
    gppMean = eval(parse(text = gppEqn))
  }
  
  comment(gppMean) <- "mg C m-2 t-1"
  return(gppMean)
}

#'
#'
#'
slim_models = function(df,RSMEcutoff = 0.7,...){
  meanGPPunits = grep("mg C m-2 y-1", attr(df$meanGPP,"comment"))
  highRRSME = quantile(df$RSME, RSMEcutoff)
  
  
  if(meanGPPunits){
    
    dfSlim = df %>%
      dplyr::filter(between(meanGPP, 0, 5000))
    if(nrow(dfSlim) < 5){
      return(dfSlim)
    } else{ dfSlim %>%
          dplyr::filter(RSME < RSMEcutoff)
      }
  } else{
    dfSlim = df %>%
      dplyr::filter(between(meanGPP, 0, 5000/365))
    if(nrow(dfSlim) < 5){
      return(dfSlim)
    } else{ dfSlim %>%
        dplyr::filter(RSME < RSMEcutoff)
    }
  }
}

#'
#'
#'
max_k = function(mm,...){
  mmDf = data.frame(mm@fit)
  k600vec = mmDf$K600.daily
  max(k600vec, na.rm = TRUE)
}
