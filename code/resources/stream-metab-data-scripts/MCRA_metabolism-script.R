# MCRA metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
MCRA_DO = clean_DO(siteCode ='MCRA', return = TRUE, save = FALSE);names(MCRA_DO)
MCRA_DO_xts = xts(MCRA_DO[,grepl("DO_*", names(MCRA_DO))], order.by= MCRA_DO$timePeriod)
dygraph(MCRA_DO_xts, main = "MCRA DOs") %>% dyRangeSelector()
lm101to102= lm(DO_102 ~ DO_101, data = MCRA_DO);summary(lm101to102);plot(MCRA_DO$DO_101,MCRA_DO$DO_102)
lm111to112 = lm(DO_112 ~ DO_111, data = MCRA_DO);summary(lm111to112);plot(MCRA_DO$DO_111, MCRA_DO$DO_112)

MCRAbadDates = c(seq(as.Date("2017-12-17"), as.Date("2018-04-06"), by = 1),
                 seq(as.Date("2020-01-07"), as.Date("2020-02-19"), by = 1),
                 seq(as.Date("2020-10-23"), as.Date("2020-11-11"), by = 1))
# 
# # fill with predicted values
MCRA_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = MCRA_DO$DO_101))
MCRA_DO$pred2 = predict(lm111to112, newdata = data.frame(DO_111 = MCRA_DO$DO_111))
# 
MCRA_clean_DO = MCRA_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na(DO_112) & is.na(DO_102) & !is.na(DO_111) ~ pred2,
                                   is.na(DO_112) & !is.na(DO_102) ~ DO_102,
                                   TRUE ~ DO_112)) %>%
  dplyr::mutate(DO.obs = case_when(as.Date(timePeriod) %in% MCRAbadDates ~ NA_real_,
                                   TRUE ~ DO.obs)) %>% 
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
# 
saveRDS(MCRA_clean_DO, file = "./ignore/site-gpp-data/MCRA_clean_DO.rds")
# 
# # debugonce(clean_temp)
MCRA_temp = clean_temp(siteCode = 'MCRA', return = TRUE, save = FALSE)
MCRA_temp_xts = xts(MCRA_temp[,grep("temp_*", names(MCRA_temp))], order.by= MCRA_temp$timePeriod)
dygraph(MCRA_temp_xts, main = "MCRA TEMPs") %>% dyRangeSelector()

MCRAbad112Dates = c(seq(as.Date("2022-01-08"),as.Date("2022-01-27"), by = 1),
                    seq(as.Date("2022-06-30"), as.Date("2022-08-31"), by = 1))

MCRA_temp = MCRA_temp %>% 
  dplyr::mutate(temp_112 = case_when(as.Date(timePeriod) %in% MCRAbad112Dates ~ NA_real_,
                                   TRUE ~ temp_112))

lm101to102 = lm(temp_102 ~ temp_101, data = MCRA_temp);summary(lm101to102)
lm111to112 = lm(temp_112 ~ temp_111, data = MCRA_temp);summary(lm111to112);plot(MCRA_temp$temp_111, MCRA_temp$temp_112)
# 
# # fill with predicted values
MCRA_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = MCRA_temp$temp_101))
MCRA_temp$pred2 = predict(lm111to112, newdata = data.frame(temp_111 = MCRA_temp$temp_111))
 
MCRA_clean_temp = MCRA_temp %>%
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     is.na(temp_112) & is.na( temp_102) ~ pred2,
                                     is.na(temp_112) & !is.na(temp_102) ~ temp_102,
                                     TRUE ~ temp_112)) %>%
  dplyr::select(timePeriod, temp_102 = "temp.obs", hour)

saveRDS(MCRA_clean_temp, file = "./ignore/site-gpp-data/MCRA_clean_temp.rds")


clean_temp(siteCode = 'MCRA', return = FALSE)
MCRA_clean_temp = readRDS(file = "./ignore/site-gpp-data/MCRA_clean_temp.rds")

# debugonce(get_site_data)
MCRA_met = get_site_data(siteCode = "MCRA")
plot_site('MCRA')

# remove some data points above 25 which are anomolous
# debugonce(clean_met_data)
MCRA_met_clean = clean_met_data(MCRA_met)
# Quick plot to check out the data series
# tz = attr(MCRA_met$solar.time,"tzone")

MCRA_met_clean %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs, color = as.logical(outQF)))+
  theme_minimal()

MCRA_met %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  saveRDS("./ignore/site-gpp-data/clean-met-files/MCRA_met.rds")

MCRA_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2018) %>%
  saveRDS("./data/derived_data/clean-met-files/MCRA2018_met.rds")
MCRA_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/MCRA2019_met.rds")
MCRA_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/MCRA2020_met.rds")
MCRA_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/MCRA2021_met.rds")
MCRA_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/MCRA2022_met.rds")
# MCRA test 
tz(MCRA_met$solar.time)


MCRA_test = MCRA_met %>%
  dplyr::filter(between(solar.time, as.POSIXct("2017-10-03 00:00:00"), as.POSIXct("2017-10-15 04:00:00")))

MCRA_test %>%
 
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
mm_bin <- metab(bayes_specs, data=MCRA_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))