# CARI metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
CARI_DO = clean_DO(siteCode ='CARI', return = TRUE, save = FALSE);names(CARI_DO)
CARI_DO_xts = xts(CARI_DO[,grep('DO',names(CARI_DO))], order.by= CARI_DO$timePeriod)
dygraph(CARI_DO_xts, main = "CARI DOs") %>% dyRangeSelector()

CARI101badDates = c(seq(as.Date("2018-07-10"), as.Date("2018-08-17"), by = 1))

CARI102badDates = c(seq(as.Date("2018-07-05"), as.Date("2018-08-02"), by = 1),
                    seq(as.Date("2018-08-17"), as.Date("2018-08-30"), by = 1),
                    seq(as.Date("2019-09-06"), as.Date("2019-10-18"), by = 1))
CARI_clean_DO = CARI_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% CARI101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% CARI102badDates, NA_real_, DO_102)) %>% 
  dplyr::mutate(DO_102 = case_when(is.na(DO_102) & !is.na(DO_101) ~ DO_101,
                                   TRUE ~ DO_102)) %>% 
  dplyr::select(timePeriod, DO_102, hour)
plot(ts(CARI_clean_DO$DO_102))
saveRDS(CARI_clean_DO,here::here("ignore/site-gpp-data/CARI_clean_DO.rds"))
rm(list = ls())
source("./code/resources/01_load-packages.R")
CARI_temp = clean_temp(siteCode = 'CARI', return = TRUE, save = FALSE)
CARI_temp_xts = xts(CARI_temp[,grep('temp', names(CARI_temp))], order.by= CARI_temp$timePeriod)
dygraph(CARI_temp_xts, main = "CARI temps") %>% dyRangeSelector()

CARI101badDates = c(seq(as.Date("2019-06-27"), as.Date("2019-07-13"), by = 1),
                    seq(as.Date("2020-05-14"), as.Date("2020-05-31"), by = 1),
                    seq(as.Date("2020-08-05"), as.Date("2020-08-07"), by = 1),
                    seq(as.Date("2021-06-21"), as.Date("2021-06-25"), by = 1))

CARI102badDates = c(seq(as.Date("2020-05-28"), as.Date("2020-05-31"), by = 1))

CARI_clean_temp = CARI_temp %>% 
  dplyr::mutate(temp_102 = case_when(as.Date(timePeriod) < as.Date("2019-06-27") & is.na(temp_102) ~ temp_101,
                                     is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_102)) %>% 
  dplyr::select(timePeriod, temp_102, hour)
    
    
saveRDS(CARI_clean_temp, file = here::here("ignore/site-gpp-data/CARI_clean_temp.rds"))

# run here
debugonce(clean_Q)
CARI_Q = clean_Q("CARI", save = FALSE, return = TRUE)
# debugonce(get_site_data)
CARI_met = get_site_data(siteCode = "CARI")



# remove some data points above 25 which are anomolous
# debugonce(clean_met_data)
CARI_met = clean_met_data(CARI_met)
# Quick plot to check out the data series
# tz = attr(CARI_met$solar.time,"tzone")
CARI_met_xts = xts(CARI_met[,"discharge"], order.by = CARI_met$solar.time)
dygraph(CARI_met_xts, main = "CARI Q")
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
mm_bin <- metab(bayes_specs, data=CARI_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))