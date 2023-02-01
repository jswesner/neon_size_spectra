# WALK metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
WALK_DO = clean_DO(siteCode ='WALK', save = FALSE, return = TRUE);names(WALK_DO)
WALK_DO_xts = xts(WALK_DO[,grep("DO_*",names(WALK_DO))], order.by = WALK_DO$timePeriod)
dygraph(WALK_DO_xts, main = "WALK DOs") %>% dyRangeSelector()

WALK101badDates = c(seq(as.Date("2019-08-18"), as.Date("2019-10-12"),by =1),
                    as.Date("2022-04-25"),
                    seq(as.Date("2022-06-21"), as.Date("2022-06-29"), by = 1),
                    seq(as.Date("2022-08-12"), as.Date("2022-08-30"), by = 1))


WALK102badDates = c(seq(as.Date("2018-02-11"), as.Date("2018-02-14"), by = 1),
                    seq(as.Date("2021-12-20"), as.Date("2022-01-04"), by = 1))

WALK_DO = WALK_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% WALK101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% WALK102badDates, NA_real_, DO_102))

lm101to102= lm(DO_102 ~ DO_101, data = WALK_DO);summary(lm101to102);plot(DO_102 ~ DO_101, data = WALK_DO)

# fill with predicted values
WALK_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = WALK_DO$DO_101))

WALK_clean_DO = WALK_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na(DO_102) & is.na(DO_101) ~ NA_real_,
                                   TRUE ~ DO_102)) %>%
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
plot(ts(WALK_clean_DO$DO_102))

saveRDS(WALK_clean_DO, file = here::here("ignore/site-gpp-data/WALK_clean_DO.rds"))
rm(list = ls())
source("./code/resources/01_load-packages.R")
WALK_temp = clean_temp(siteCode = 'WALK', return = TRUE, save = TRUE)
WALK_temp_xts = xts(WALK_temp[,grep("temp_*",names(WALK_temp))], order.by = WALK_temp$timePeriod)
dygraph(WALK_temp_xts, main = "WALK temps") %>% dyRangeSelector()

# WALK101badDates = c(seq(as.Date("2019-05-06"), as.Date("2019-07-03"), by = 1))
# WALK102badDates = c(seq(as.Date("2019-05-06"), as.Date("2019-07-03"), by = 1),
                    # seq(as.Date("2021-05-16"), as.Date("2021-09-07"), by = 1))#make temp_101 here

# WALK_temp = WALK_temp %>% 
  # dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% WALK101badDates, NA_real_, temp_101),
                # temp_102 = ifelse(as.Date(timePeriod) %in% WALK102badDates, NA_real_, temp_102))

# lm101to102= lm(temp_102 ~ temp_101,data = WALK_temp);summary(lm101to102);plot(temp_102 ~ temp_101, data = WALK_temp)

# fill with predicted values
# WALK_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = WALK_temp$temp_101))

# WALK_clean_temp = WALK_temp %>%
  # dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     # between(as.Date(timePeriod),as.Date("2021-05-16"),as.Date("2021-09-07")) ~ temp_101,
                                     # is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     # TRUE ~ temp_102)) %>%
  # dplyr::select(timePeriod, temp_102 = "temp.obs", hour)
# plot(ts(WALK_clean_temp$temp_102))

# saveRDS(WALK_clean_temp, file = here::here("ignore/site-gpp-data/WALK_clean_temp.rds"))
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
mm_bin <- metab(bayes_specs, data=WALK_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))