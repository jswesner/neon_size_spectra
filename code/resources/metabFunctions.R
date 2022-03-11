# custom functions for prepping data products for metabolism models

#' @title get_site_data
#' @description This function subsets all the relevant data products to estimate stream metabolism  for a site
#' @param sitecode character. Character string representing the stream identification code 
#' @param startDate character. Character coercible to Date, representing start date of data series. If NULL (default), the entire data series are downloaded.
#' @param endDate character. Character, coercible to Date, representing end data of data series. If NULL (default), the entire data series are downloaed.
#'

get_site_data = function(siteCode = NA, startDate = NA, endDate = NA,...){
  require(tidyverse)
  require(lutz)
  
  ## Load in necessary data objects
  latlong = read_csv(file = "./data/site_latlong.csv")
  
  ## estimate the timezone 
  siteTZ = lutz::tz_lookup_coords(lat = latlong[which(site == siteCode), 'lat'],
                                  long = latlong[which(site == siteCode), 'long'],
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

  ## Convert all the data sets
  ### DO
  doDf = siteList[[grep('DO', names(siteList))]] %>%
    dplyr::select(startDateTime = 'timePeriod', dissolvedOxygen) %>%
    dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ))
  ### temperature
  tempDf = siteList[[grep('temp', names(siteList))]]
  dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ))
  
  ### discharge
  qDf = siteList[[grep('dischargeQ', names(siteList))]]
  dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ))
  
  ### air pressure 
  airDf = siteList[[grep('airPressure', names(siteList))]]
  dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = siteTZ))
  
  
  }


#' @title 
#' @description This function subsets all the relevant data products to calculate Q-Z relationships for a site
#' @param siteCode 
#' @param startDate
#' @param endDate
#'


#' @title get_
#'
#'

#'
#'
#'




