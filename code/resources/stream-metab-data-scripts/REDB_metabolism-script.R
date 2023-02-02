# REDB metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
REDB_DO = clean_DO(siteCode ='REDB', save = FALSE, return = TRUE);names(REDB_DO)
REDB_DO_xts = xts(REDB_DO[,grep("DO_*",names(REDB_DO))], order.by = REDB_DO$timePeriod)
dygraph(REDB_DO_xts, main = "REDB DOs") %>% dyRangeSelector()

siteTZ = lutz::tz_lookup_coords(lat = unlist(latlong[which(latlong$site == "REDB"), 'lat']),
                                lon = unlist(latlong[which(latlong$site == "REDB"), 'long']),
                                method = 'accurate')

REDB101badDates = c(seq(as.Date("2021-07-01"), as.Date("2021-09-01"), by = 1),
                    seq(as.Date("2022-04-12"), as.Date("2022-04-28"), by = 1),
                    as.Date("2022-08-02"))

REDB102badDates = c(seq(as.Date("2022-04-12"), as.Date("2022-04-28"), by = 1),
                    as.Date("2022-08-02"))

REDB_DO = REDB_DO %>% 
  dplyr::mutate(across(matches('DO'), ~case_when(between(as.POSIXct(timePeriod),
                                                         as.POSIXct("2022-08-02 17:15:00", tz = siteTZ),
                                                         as.POSIXct("2022-09-12 11:00:00", tz = siteTZ)) ~ .x - 1.9,
                                                 TRUE ~ .x))) %>% 
  dplyr::mutate(across(matches('DO'), ~case_when(between(as.POSIXct(timePeriod),
                                                         as.POSIXct("2020-10-16 14:00:00", tz = siteTZ),
                                                         as.POSIXct("2021-02-11 11:15:00", tz = siteTZ)) ~ .x - 2.5,
                                                                       TRUE ~ .x))) %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% REDB101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% REDB102badDates, NA_real_, DO_102))

lm101to102= lm(DO_102 ~ DO_101, data = REDB_DO);summary(lm101to102);plot(DO_102 ~ DO_101, data = REDB_DO)

# fill with predicted values
REDB_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = REDB_DO$DO_101))

REDB_clean_DO = REDB_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na(DO_102) & is.na(DO_101) ~ NA_real_,
                                   TRUE ~ DO_102)) %>%
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
plot(ts(REDB_clean_DO$DO_102))

saveRDS(REDB_clean_DO, file = here::here("ignore/site-gpp-data/REDB_clean_DO.rds"))
rm(list = ls())
source("./code/resources/01_load-packages.R")
REDB_temp = clean_temp(siteCode = 'REDB', return = TRUE, save = FALSE)
REDB_temp_xts = xts(REDB_temp[,grep("temp_*",names(REDB_temp))], order.by = REDB_temp$timePeriod)
dygraph(REDB_temp_xts, main = "REDB temps") %>% dyRangeSelector()

REDB101badDates = c(as.Date("2022-08-30"),
                    seq(as.Date("2021-03-30"), as.Date("2021-08-06"), by  =1))
# REDB102badDates = c(seq(as.Date("2019-05-06"), as.Date("2019-07-03"), by = 1),
                    # seq(as.Date("2021-05-16"), as.Date("2021-09-07"), by = 1))#make temp_101 here

REDB_temp = REDB_temp %>% 
  dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% REDB101badDates, NA_real_, temp_101))

lm101to102= lm(temp_102 ~ temp_101,data = REDB_temp);summary(lm101to102);plot(temp_102 ~ temp_101, data = REDB_temp)

# fill with predicted values
REDB_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = REDB_temp$temp_101))

REDB_clean_temp = REDB_temp %>%
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_102)) %>%
  dplyr::select(timePeriod, temp_102 = "temp.obs", hour)
plot(ts(REDB_clean_temp$temp_102))

saveRDS(REDB_clean_temp, file = here::here("ignore/site-gpp-data/REDB_clean_temp.rds"))
 
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
mm_bin <- metab(bayes_specs, data=REDB_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))