# MAYF metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
MAYF_DO = clean_DO(siteCode ='MAYF', save = FALSE, return = TRUE);names(MAYF_DO)
MAYF_DO_xts = xts(MAYF_DO[,grep("DO_*",names(MAYF_DO))], order.by = MAYF_DO$timePeriod)
dygraph(MAYF_DO_xts, main = "MAYF DOs") %>% dyRangeSelector()

MAYF101badDates = c(seq(as.Date("2018-06-12"), as.Date("2018-07-26"), by = 1),
                    seq(as.Date("2018-12-27"), as.Date("2019-01-09"), by = 1),
                    seq(as.Date("2019-01-19"), as.Date("2019-02-13"), by = 1),
                    as.Date("2019-02-21"),as.Date("2019-04-09"),
                    seq(as.Date("2019-07-04"), as.Date("2019-07-19"), by = 1),
                    seq(as.Date("2020-02-18"), as.Date("2020-03-11"), by = 1),
                    seq(as.Date("2020-09-30"), as.Date("2020-10-03"), by = 1),
                    seq(as.Date("2020-11-04"), as.Date("2020-11-16"), by = 1)
                    )

MAYF102badDates = c(seq(as.Date("2018-12-06"), as.Date("2019-01-09"), by = 1),
                    seq(as.Date("2020-01-03"), as.Date("2020-01-07"), by = 1),
                    as.Date("2021-03-26"), as.Date("2021-03-27"))

MAYF111badDates = c(seq(as.Date("2022-06-17"), as.Date("2022-06-24"), by = 1))


MAYF_DO = MAYF_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% MAYF101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% MAYF102badDates, NA_real_, DO_102),
                DO_111 = ifelse(as.Date(timePeriod) %in% MAYF111badDates, NA_real_, DO_111))

lm101to102= lm(DO_102 ~ DO_101, data = MAYF_DO);summary(lm101to102);plot(DO_102 ~ DO_101, data = MAYF_DO)
lm111to112 = lm(DO_112 ~ DO_111, data = MAYF_DO);summary(lm111to112);plot(DO_112 ~ DO_111, data = MAYF_DO)

# fill with predicted values
MAYF_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = MAYF_DO$DO_101))
MAYF_DO$pred2 = predict(lm111to112, newdata = data.frame(DO_111 = MAYF_DO$DO_111))

MAYF_clean_DO = MAYF_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_112) & is.na(DO_111) & !is.na(DO_102) ~ DO_102,
                                   is.na(DO_112) & is.na(DO_111) & is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na(DO_112) & is.na(DO_102) & !is.na(DO_111) ~ pred2,
                                   is.na(DO_112) & is.na(DO_102) & is.na(DO_101) & is.na(DO_111) ~ NA_real_,
                                   TRUE ~ DO_112)) %>%
  dplyr::mutate(DO.obs = ifelse(DO.obs < 6, NA_real_, DO.obs)) %>% 
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
plot(ts(MAYF_clean_DO$DO_102))

saveRDS(MAYF_clean_DO, file = here::here("ignore/site-gpp-data/MAYF_clean_DO.rds"))
rm(list = ls())
source("./code/resources/01_load-packages.R")
MAYF_temp = clean_temp(siteCode = 'MAYF', return = TRUE, save = FALSE)
MAYF_temp_xts = xts(MAYF_temp[,grep("temp_*",names(MAYF_temp))], order.by = MAYF_temp$timePeriod)
dygraph(MAYF_temp_xts, main = "MAYF temps") %>% dyRangeSelector()

MAYF101badDates = c(seq(as.Date("2020-10-09"), as.Date("2020-10-28"), by = 1),
                    seq(as.Date("2020-11-24"), as.Date("2020-12-30"), by = 1))

MAYF102badDates = c(seq(as.Date("2021-01-12"), as.Date("2021-01-19"), by = 1),
                    seq(as.Date("2021-05-05"), as.Date("2021-11-03"), by = 1))

MAYF112badDates = c(seq(as.Date("2021-05-05"), as.Date("2021-11-03"), by = 1))


MAYF_temp = MAYF_temp %>% 
  dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% MAYF101badDates, NA_real_, temp_101),
                temp_102 = ifelse(as.Date(timePeriod) %in% MAYF102badDates, NA_real_, temp_102),
                temp_112 = ifelse(as.Date(timePeriod) %in% MAYF112badDates, NA_real_, temp_112))

lm101to102= lm(temp_102 ~ temp_101,data = MAYF_temp);summary(lm101to102);plot(temp_102 ~ temp_101, data = MAYF_temp)
lm111to112 = lm(temp_112 ~ temp_111,data = MAYF_temp);summary(lm111to112);plot(temp_112 ~ temp_111,data = MAYF_temp)

# fill with predicted values
MAYF_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = MAYF_temp$temp_101))
MAYF_temp$pred2 = predict(lm111to112, newdata = data.frame(temp_111 = MAYF_temp$temp_111))

MAYF_clean_temp = MAYF_temp %>%
  dplyr::mutate(temp.obs = case_when(is.na(temp_112) & is.na(temp_111) & !is.na(temp_102) ~ temp_102,
                                     is.na(temp_112) & is.na(temp_111) & is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     is.na(temp_112) & !is.na(temp_111) & is.na(temp_102) & is.na(temp_101) ~ pred2,
                                     is.na(temp_112)& is.na(temp_111)& is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_112)) %>%
  dplyr::select(timePeriod, temp_102 = "temp.obs", hour)
plot(ts(MAYF_clean_temp$temp_102))

saveRDS(MAYF_clean_temp, file = here::here("ignore/site-gpp-data/MAYF_clean_temp.rds"))
 
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
mm_bin <- metab(bayes_specs, data=MAYF_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))