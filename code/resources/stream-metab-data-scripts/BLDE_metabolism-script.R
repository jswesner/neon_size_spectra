rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
BLDE_DO = clean_DO(siteCode ='BLDE', save = FALSE, return = TRUE);names(BLDE_DO)
BLDE_DO_xts = xts(BLDE_DO[,grep("DO_*",names(BLDE_DO))], order.by = BLDE_DO$timePeriod)
dygraph(BLDE_DO_xts, main = "BLDE DOs") %>% dyRangeSelector()

BLDEbadDates = c(seq(as.Date("2022-03-09"), as.Date("2022-03-15"), by =1))

BLDE_DO = BLDE_DO %>% 
  dplyr::mutate(across(matches('DO'), ~case_when(as.Date(timePeriod) %in% BLDEbadDates ~ NA_real_,
                                   TRUE ~ .x)))

lm101to102 = lm(DO_102~DO_101, BLDE_DO);summary(lm101to102);plot(DO_102~DO_101, BLDE_DO)
BLDE_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = BLDE_DO$DO_101))
BLDE_clean_DO = BLDE_DO %>% 
  dplyr::mutate(DO_102 = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   TRUE ~ DO_102)) %>% 
  dplyr::select(timePeriod, DO_102, hour)

saveRDS(BLDE_clean_DO, file = here::here("ignore/site-gpp-data/BLDE_clean_DO.rds"))

BLDE_temp = clean_temp(siteCode = 'BLDE', return = TRUE, save = FALSE);names(BLDE_temp)
BLDE_temp_xts = xts(BLDE_temp[,grep("temp_*",names(BLDE_temp))], order.by = BLDE_temp$timePeriod)
dygraph(BLDE_temp_xts, main = "BLDE temps") %>% dyRangeSelector()

BLDE102badDates = c(seq(as.Date("2021-08-16"), as.Date("2021-08-17"), by = 1))

BLDE_temp = BLDE_temp %>% 
  dplyr::mutate(temp_102 = ifelse(as.Date(timePeriod) %in% BLDE102badDates, NA_real_, temp_102))

lm101to102 = lm(temp_102 ~ temp_101, BLDE_temp);summary(lm101to102);plot(BLDE_temp$temp_101, BLDE_temp$temp_102)
BLDE_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = BLDE_temp$temp_101))

BLDE_clean_temp = BLDE_temp %>% 
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_102)) %>% 
  dplyr::select(timePeriod, temp_102 = 'temp.obs', hour)

saveRDS(BLDE_clean_temp, file = here::here("ignore/site-gpp-data/BLDE_clean_temp.rds"))
# remove some data points above 25 which are anomolous
# debugonce(clean_met_data)
BLDE_met_clean = clean_met_data(BLDE_met_full)
# Quick plot to check out the data series
BLDE_met_clean %>% 
  # dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = temp.water))+
  theme_minimal()

plot_site("BLDE")

rstudioapi::jobRunScript(
  path = "./ignore/metab-models/BLDE_metModel.R",
  name = "BLDE metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

BLDE_full_mle = readRDS("./ignore/metab-models/BLDE_full_mle.rds")


BLDE_mle_params = get_params(BLDE_full_mle, uncertainty = 'ci') %>%
  dplyr::mutate(GPP.daily = case_when(GPP.daily < 0 ~ 0,
                                      GPP.daily > 72 ~ NA_real_,
                                      TRUE ~ GPP.daily))

BLDE_mle_params %>%
  ggplot()+geom_line(aes(x = date, y = GPP.daily), color = 'green')



BLDE_met2019 = BLDE_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/BLDE2019_met.rds")
BLDE_met2020 = BLDE_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/BLDE2020_met.rds")

BLDE_met2021 = BLDE_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/BLDE2021_met.rds")

BLDE_met2022 = BLDE_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/BLDE2022_met.rds")

#Basic StreamPULSE data processing pipeline
#Updated 2020-05-11
#Contact Mike Vlah (michael.vlah@duke.edu) with questions or comments.

# Install StreamPULSE pipeline tools from GitHub
# The StreamPULSE package is in development and changes frequently!
# If something doesn't work as expected, first try reinstalling.
# remotes::install_github('streampulse/StreamPULSE', dependencies=TRUE)

# Install latest version of streamMetabolizer.
remotes::install_github('appling/unitted')
# remotes::install_github("appling/streamMetabolizer")
remotes::install_github("USGS-R/streamMetabolizer")

# Load packages.
library(StreamPULSE)
library(streamMetabolizer)
library(tidyverse)

# colname          class          units     need
# 1 solar.time POSIXct,POSIXt                required
# 2     DO.obs        numeric      mgO2 L^-1 required
# 3     DO.sat        numeric      mgO2 L^-1 required
# 4      depth        numeric              m required
# 5 temp.water        numeric           degC required
# 6      light        numeric umol m^-2 s^-1 required
# 7  discharge        numeric       m^3 s^-1 optional

# Source custom functions to extract and clean NEON data products for analysis
source("./code/resources/metabFunctions.R")
latlong = read_csv(file = "./data/site_latlong.csv")

# To disconnect the duckDB dbDisconnect(neonstore::neon_db())
# site_code is a combination of regionID and siteID
site_code = 'BLDE'
start_date = '2018-02-01'
end_date = '2018-05-30'

BLDE_files = list.files("./ignore/site-gpp-data/", "BLDE", full.names = TRUE)
BLDE_filenames = BLDE_files %>% 
  lapply(., function(x) gsub("^(\\w{4})_\\d{1,2}.*_(\\w{2,}).rds","\\1_\\2",
                             sapply(strsplit(x,"/"),"[",4))) %>%
  unlist

BLDE_list = BLDE_files %>%
  purrr::map(readRDS) %>% setNames(., BLDE_filenames)

BLDE_list[[grep("dischargeQ", names(BLDE_list))]] %>%
  na.omit %>%
  ggplot()+
  geom_point(aes(x = equivalentStage, y = maxpostDischarge))

do = BLDE_list[[grep("DO", names(BLDE_list))]]

do %>% ggplot()+
  geom_point(aes(x = timePeriod, y = dissolvedOxygenFinalQF)) +
  geom_line(aes(x = timePeriod, y = dissolvedOxygenSaturation/100))

temp = BLDE_list[[grep("temp", names(BLDE_list))]] %>%
  dplyr::select(startDateTime, surfWaterTempMean) %>%
  dplyr::mutate(startDateTime = as.POSIXlt(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")) %>%
    group_by(startDateTime) %>%
    dplyr::summarise(surfWaterTempMean = mean(surfWaterTempMean, na.rm = TRUE)) %>%
    left_join(BLDE_list[[grep("dischargeQ", names(BLDE_list))]] %>%
                dplyr::select(startDateTime = 'timePeriod', equivalentStage, maxpostDischarge, calibratedPressure, everything()) %>%
                dplyr::mutate(startDateTime = as.POSIXlt(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"),
                              depth = streamMetabolizer::calc_depth(maxpostDischarge)), by = "startDateTime") %>%
   left_join(BLDE_list[[grep("airPressure", names(BLDE_list))]] %>%
               dplyr::select(startDateTime, corPres, staPresMean) %>%
               dplyr::mutate(startDateTime = as.POSIXlt(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")), by = "startDateTime")

calc_DO_sat(temp[349,'surfWaterTempMean'], temp[349,'staPresMean']*10)


BLDE_met = BLDE_list[[grep("DO", names(BLDE_list))]] %>%
  dplyr::select(startDateTime = 'timePeriod', dissolvedOxygen) %>%
  dplyr::mutate(startDateTime = as.POSIXct(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")) %>%
  left_join(BLDE_list[[grep("dischargeQ", names(BLDE_list))]] %>%
              dplyr::select(startDateTime = 'timePeriod', equivalentStage, maxpostDischarge) %>%
              dplyr::mutate(startDateTime = as.POSIXlt(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"),
                            depth = streamMetabolizer::calc_depth(maxpostDischarge)), by = "startDateTime") %>%
  left_join(BLDE_list[[grep("temp", names(BLDE_list))]] %>%
              dplyr::select(startDateTime, surfWaterTempMean) %>%
              dplyr::mutate(startDateTime = as.POSIXlt(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")) %>%
              group_by(startDateTime) %>%
              dplyr::summarise(surfWaterTempMean = mean(surfWaterTempMean, na.rm = TRUE)), by = "startDateTime") %>%
  left_join(BLDE_list[[grep("airPressure", names(BLDE_list))]] %>%
              dplyr::select(startDateTime, staPresMean) %>%
              dplyr::mutate(startDateTime = as.POSIXlt(startDateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")), by = "startDateTime") %>%
  dplyr::mutate(siteID = "BLDE") %>%
  dplyr::select(siteID, everything()) %>%
  dplyr::mutate(solar.time = streamMetabolizer::calc_solar_time(startDateTime,
                                                                longitude = unlist(
                                                                  latlong[which(latlong$site == "BLDE"), "long"]
                                                                )),
                light = streamMetabolizer::calc_light(solar.time,longitude = unlist(
                  latlong[which(latlong$site == "BLDE"), "long"]
                ), latitude = unlist(
                  latlong[which(latlong$site == "BLDE"), "lat"]
                ))) %>%
  dplyr::mutate(depth = zoo::na.approx(depth, maxgap = 4, na.rm = FALSE),
                surfWaterTempMean = zoo::na.approx(surfWaterTempMean, maxgap = 2, na.rm = FALSE),
                maxpostDischarge = zoo::na.approx(maxpostDischarge, maxgap= 4, na.rm = FALSE)) %>%
  dplyr::mutate(DO.sat = streamMetabolizer::calc_DO_sat(temp.water = surfWaterTempMean,
                                                        pressure.air = staPresMean*10),
                DO.sat = zoo::na.approx(DO.sat, maxgap = 2, na.rm = FALSE),
                DO.pctsat = 100*(dissolvedOxygen/DO.sat)) %>%
  dplyr::select(solar.time, DO.obs = 'dissolvedOxygen', DO.sat, DO.pctsat,depth, temp.water = 'surfWaterTempMean',
                light, discharge = 'maxpostDischarge') %>%
  dplyr::filter(between(DO.obs, 0,50))
# colname          class          units     need
# 1 solar.time POSIXct,POSIXt                required
# 2     DO.obs        numeric      mgO2 L^-1 required
# 3     DO.sat        numeric      mgO2 L^-1 required
# 4      depth        numeric              m required
# 5 temp.water        numeric           degC required
# 6      light        numeric umol m^-2 s^-1 required
# 7  discharge        numeric       m^3 s^-1 optional

debugonce(get_site_data)
x = get_site_data(siteCode = "HOPB")

# plot the data series

BLDE_met %>%
  # dplyr::filter(solar.time < as.POSIXct("2019-07-01 00:00:00",format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"),
                # DO.obs < 17) %>% 
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs))

BLDE_metMod =
BLDE_met %>%
  dplyr::filter(between(solar.time, as.POSIXct("2018-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"), as.POSIXct("2019-07-01 00:00:00",format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")) |
                  between(solar.time, as.POSIXct("2019-10-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"), as.POSIXct("2020-06-15 00:00:00",format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver")),
                DO.obs < 17)


BLDE_test = BLDE_metMod %>% 
  dplyr::filter(between(solar.time, as.POSIXct("2018-10-01 04:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"), as.POSIXct("2018-10-11 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "America/Denver"))) %>%
  dplyr::select(-DO.pctsat) %>%
  dplyr::mutate(across(where(is.numeric), ~zoo::na.approx(.x, na.rm = FALSE)))


## run a quick version of the 
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,
                      err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
bayes_specs <- specs(bayes_name)
# Model specifications:
#   model_name                 b_Kn_oipi_tr_plrckm.stan                                                         
# engine                     stan                                                                             
# split_dates                FALSE                                                                            
# keep_mcmcs                 TRUE                                                                             
# keep_mcmc_data             TRUE                                                                             
# day_start                  4                                                                                
# day_end                    28                                                                               
# day_tests                  full_day, even_timesteps, complete_data, pos_discharge, pos_depth                
# required_timestep          NA                                                                               
# GPP_daily_mu               3.1                                                                              
# GPP_daily_lower            -Inf                                                                             
# GPP_daily_sigma            6                                                                                
# ER_daily_mu                -7.1                                                                             
# ER_daily_upper             Inf                                                                              
# ER_daily_sigma             7.1                                                                              
# K600_daily_meanlog_meanlog 2.484906649788                                                                   
# K600_daily_meanlog_sdlog   1.32                                                                             
# K600_daily_sdlog_sigma     0.05                                                                             
# err_obs_iid_sigma_scale    0.03                                                                             
# err_proc_iid_sigma_scale   5                                                                                
# params_in                  GPP_daily_mu, GPP_daily_lower, GPP_daily_sigma, ER_daily_mu, ER_daily_upper, E...
# params_out                 GPP, ER, DO_R2, GPP_daily, ER_daily, K600_daily, K600_daily_predlog, K600_dail...
# n_chains                   4                                                                                
# n_cores                    4                                                                                
# burnin_steps               500                                                                              
# saved_steps                500                                                                              
# thin_steps                 1                                                                                
# verbose                    TRUE

# revise the specs for a test run 
bayes_specs <- revise(bayes_specs, day_start = 0, day_end = 24, burnin_steps= 1000, saved_steps=1000,
                      n_cores=6, n_chains = 3, GPP_daily_mu=3, GPP_daily_sigma=2, verbose = TRUE)

tictoc::tic();mm_bin <- metab(bayes_specs, data=BLDE_test);tictoc::toc()

mm_bin

plot_metab_preds(mm);plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm);plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))
# bring in streamMetabolizer functions to covert columns to correct

# Choose model type for streamMetabolizer.
# Only "bayes" is available at this time.
model_type = 'bayes'

# Which modeling framework to use:
# Use "streamMetabolizer" (the default); "BASE" is not available at this time.
model_name = 'streamMetabolizer'

# Format data for metabolism modeling.
sp_data_prepped = prep_metabolism(d=sp_data, type=model_type,
                                  model=model_name, retrieve_air_pres=TRUE)

# Fit metabolism model and generate predictions (calls streamMetabolizer
# functions: mm_name, specs, metab, predict_metab).
model_fit = fit_metabolism(sp_data_prepped, pool_K600 = 'none')
saveRDS(model_fit, "./demo_model_fit.rds")

# Plot results and diagnostics (This behaves unpredictably on some machines.
#If you sent your results to the data portal in the step above, you can view
#them more robustly there. If your data do not all occur within the same
#calendar year, the visualizations may still not work.)
plot_output(model_fit)

#Here's where results and diagnostics live on the data portal:
# http://data.streampulse.org:3838/streampulse_diagnostic_plots/
