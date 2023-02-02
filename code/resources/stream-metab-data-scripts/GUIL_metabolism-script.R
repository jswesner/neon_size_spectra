# GUIL metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
GUIL_DO = clean_DO(siteCode ='GUIL', save = FALSE, return = TRUE);names(GUIL_DO)
GUIL_DO_xts = xts(GUIL_DO[,grep("DO_*",names(GUIL_DO))], order.by = GUIL_DO$timePeriod)
dygraph(GUIL_DO_xts, main = "GUIL DOs") %>% dyRangeSelector()

GUIL101badDates = c(seq(as.Date("2018-07-06"), as.Date("2018-07-18"), by =1),
                    seq(as.Date("2020-06-04"), as.Date("2020-06-08"), by = 1),
                    seq(as.Date("2020-09-28"), as.Date("2020-10-01"), by = 1),
                    seq(as.Date("2022-02-18"), as.Date("2022-04-18"), by = 1),
                    seq(as.Date("2022-06-15"), as.Date("2022-06-22"), by = 1),
                    seq(as.Date("2022-08-11"), as.Date("2022-08-15"), by = 1))

GUIL102badDates = c(seq(as.Date("2018-08-12"), as.Date("2018-08-16"), by =1))

GUIL_DO = GUIL_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% GUIL101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% GUIL102badDates, NA_real_, DO_102))

# bad correction 
lm101to102 = lm(DO_102~DO_101, GUIL_DO);summary(lm101to102);plot(DO_102~DO_101, GUIL_DO)

GUIL_clean_DO = GUIL_DO %>% 
  dplyr::mutate(DO_102 = case_when(is.na(DO_102) & !is.na(DO_101) ~ DO_101,
                                   TRUE ~ DO_102)) %>% 
  dplyr::select(timePeriod, DO_102, hour)

saveRDS(GUIL_clean_DO, file = here::here("ignore/site-gpp-data/GUIL_clean_DO.rds"))
dev.off()
GUIL_temp = clean_temp(siteCode = 'GUIL', return = TRUE, save = FALSE);names(GUIL_temp)
GUIL_temp_xts = xts(GUIL_temp[,grep("temp_*",names(GUIL_temp))], order.by = GUIL_temp$timePeriod)
dygraph(GUIL_temp_xts, main = "GUIL temps") %>% dyRangeSelector()

GUIL101badDates = c(seq(as.Date("2018-11-17"), as.Date("2018-12-11"), by = 1),
                    seq(as.Date("2020-08-21"), as.Date("2021-03-02"), by = 1))

GUIL102badDates = c(seq(as.Date("2019-10-11"), as.Date("2019-10-12"), by = 1),
                    seq(as.Date("2021-02-03"), as.Date("2021-02-09"), by = 1))

GUIL_temp = GUIL_temp %>% 
  dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% GUIL101badDates, NA_real_, temp_101),
                temp_102 = ifelse(as.Date(timePeriod) %in% GUIL102badDates, NA_real_, temp_102))

# bad correction, not as bad as DO though
lm101to102 = lm(temp_102 ~ temp_101, GUIL_temp);summary(lm101to102);plot(GUIL_temp$temp_101, GUIL_temp$temp_102)

GUIL_clean_temp = GUIL_temp %>% 
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ temp_101,
                                     is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_102)) %>% 
  dplyr::select(timePeriod, temp_102 = 'temp.obs', hour)

saveRDS(GUIL_clean_temp, file = here::here("ignore/site-gpp-data/GUIL_clean_temp.rds"))
dev.off()

# remove some data points above 25 which are anomolous
# debugonce(clean_met_data)
GUIL_met_clean = clean_met_data(GUIL_met)
# Quick plot to check out the data series

plot_site("GUIL")

GUIL_met_clean %>% 
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs))+
  theme_minimal()

GUIL_met %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  saveRDS("./ignore/site-gpp-data/clean-met-files/GUIL_met.rds")

GUIL_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2018) %>%
  saveRDS("./data/derived_data/clean-met-files/GUIL2018_met.rds")
GUIL_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/GUIL2019_met.rds")
GUIL_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/GUIL2020_met.rds")
GUIL_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/GUIL2021_met.rds")
GUIL_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/GUIL2022_met.rds")








# GUIL test 
tz(GUIL_met$solar.time)


GUIL_test = GUIL_met %>%
  dplyr::filter(between(solar.time, as.POSIXct("2017-10-03 00:00:00"), as.POSIXct("2017-10-15 04:00:00")))

GUIL_test %>%
 
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
mm_bin <- metab(bayes_specs, data=GUIL_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))