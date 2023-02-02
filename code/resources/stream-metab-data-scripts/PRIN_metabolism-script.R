# PRIN metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
PRIN_DO = clean_DO(siteCode ='PRIN', save = FALSE, return = TRUE);names(PRIN_DO)
PRIN_DO_xts = xts(PRIN_DO[,grep("DO_*",names(PRIN_DO))], order.by = PRIN_DO$timePeriod)
dygraph(PRIN_DO_xts, main = "PRIN DOs") %>% dyRangeSelector()

PRIN101badDates = c(seq(as.Date("2020-03-18"), as.Date("2020-05-23"), by = 1))

PRIN102badDates = c(seq(as.Date("2020-03-18"), as.Date("2020-05-23"), by = 1),
                    seq(as.Date("2021-08-17"), as.Date("2021-08-20"), by = 1),
                    seq(as.Date("2022-06-01"), as.Date("2022-06-09"), by = 1),
                    seq(as.Date("2022-07-18"), as.Date("2022-08-17"), by = 1))

PRIN_DO = PRIN_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% PRIN101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% PRIN102badDates, NA_real_, DO_102))

#bad correlation
lm101to102= lm(DO_102 ~ DO_101, data = PRIN_DO);summary(lm101to102);plot(DO_102 ~ DO_101, data = PRIN_DO)

# pretty bad correlation
# fill with predicted values

PRIN_clean_DO = PRIN_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & is.na(DO_101) ~ NA_real_,
                                   TRUE ~ DO_102)) %>%
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
plot(ts(PRIN_clean_DO$DO_102))

saveRDS(PRIN_clean_DO, file = here::here("ignore/site-gpp-data/PRIN_clean_DO.rds"))
dev.off()
rm(list = ls())
source("./code/resources/01_load-packages.R")
PRIN_temp = clean_temp(siteCode = 'PRIN', return = TRUE, save = FALSE)
PRIN_temp_xts = xts(PRIN_temp[,grep("temp_*",names(PRIN_temp))], order.by = PRIN_temp$timePeriod)
dygraph(PRIN_temp_xts, main = "PRIN temps") %>% dyRangeSelector()

PRIN102badDates = c(seq(as.Date("2018-03-01"), as.Date("2018-03-28"), by = 1))

PRIN_clean_temp = PRIN_temp %>% 
  dplyr::mutate(temp_102 = ifelse(as.Date(timePeriod) %in% PRIN102badDates, NA_real_, temp_102)) %>%
  dplyr::select(timePeriod, temp_102 , hour)
plot(ts(PRIN_clean_temp$temp_102))

saveRDS(PRIN_clean_temp, file = here::here("ignore/site-gpp-data/PRIN_clean_temp.rds"))
 
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
mm_bin <- metab(bayes_specs, data=PRIN_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))