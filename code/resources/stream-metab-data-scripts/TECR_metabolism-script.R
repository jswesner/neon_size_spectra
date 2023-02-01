# TECR metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
TECR_DO = clean_DO(siteCode ='TECR', save = FALSE, return = TRUE);names(TECR_DO)
TECR_DO_xts = xts(TECR_DO[,grep("DO_*",names(TECR_DO))], order.by = TECR_DO$timePeriod)
dygraph(TECR_DO_xts, main = "TECR DOs") %>% dyRangeSelector()

siteTZ = lutz::tz_lookup_coords(lat = unlist(latlong[which(latlong$site == "TECR"), 'lat']),
                                lon = unlist(latlong[which(latlong$site == "TECR"), 'long']),
                                method = 'accurate')

TECR101badDates = c(seq(as.Date("2021-11-02"), as.Date("2021-11-09"), by = 1))

TECR102badDates = c(as.Date("2019-01-23"),
                    seq(as.Date("2020-09-15"), as.Date("2020-10-28"), by = 1))

TECR_DO = TECR_DO %>% 
  dplyr::mutate(across(matches('DO'), ~case_when(between(as.POSIXct(timePeriod), 
                                                 as.POSIXct("2021-08-04 14:45:00", tz = siteTZ),
                                                 as.POSIXct("2021-09-08 13:00:00", tz = siteTZ)) ~ .x-1.6,
                                         TRUE ~ .x))) %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% TECR101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% TECR102badDates, NA_real_, DO_102))

lm101to102= lm(DO_102 ~ DO_101, data = TECR_DO);summary(lm101to102);plot(DO_102 ~ DO_101, data = TECR_DO)

# fill with predicted values
TECR_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = TECR_DO$DO_101))

TECR_clean_DO = TECR_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na(DO_102) & is.na(DO_101) ~ NA_real_,
                                   TRUE ~ DO_102)) %>%
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
plot(ts(TECR_clean_DO$DO_102))

saveRDS(TECR_clean_DO, file = here::here("ignore/site-gpp-data/TECR_clean_DO.rds"))
rm(list = ls())
source("./code/resources/01_load-packages.R")
TECR_temp = clean_temp(siteCode = 'TECR', return = TRUE, save = FALSE)
TECR_temp_xts = xts(TECR_temp[,grep("temp_*",names(TECR_temp))], order.by = TECR_temp$timePeriod)
dygraph(TECR_temp_xts, main = "TECR temps") %>% dyRangeSelector()

TECR101badDates = c(seq(as.Date("2022-04-03"), as.Date("2022-04-27"), by = 1))

TECR102badDates = c(seq(as.Date("2020-10-10"), as.Date("2020-10-29"), by = 1),
                    as.Date("2020-12-26"))

TECR_temp = TECR_temp %>% 
  dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% TECR101badDates, NA_real_, temp_101),
                temp_102 = ifelse(as.Date(timePeriod) %in% TECR102badDates, NA_real_, temp_102))

lm101to102= lm(temp_102 ~ temp_101,data = TECR_temp);summary(lm101to102);plot(temp_102 ~ temp_101, data = TECR_temp)

# fill with predicted values
TECR_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = TECR_temp$temp_101))

TECR_clean_temp = TECR_temp %>%
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_102)) %>%
  dplyr::select(timePeriod, temp_102 = "temp.obs", hour)
plot(ts(TECR_clean_temp$temp_102))

saveRDS(TECR_clean_temp, file = here::here("ignore/site-gpp-data/TECR_clean_temp.rds"))
## run a quick version of the 
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,
                      err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
bayes_specs <- specs(bayes_name)
# bayes_specs
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
bayes_specs <- revise(bayes_specs, day_start = 0, day_end = 24, burnin_steps= 10000, saved_steps= 5000,
                      thin_steps = 10,n_cores=5, n_chains = 3, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = TRUE)

# tictoc::tic();
mm_bin <- metab(bayes_specs, data=TECR_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))