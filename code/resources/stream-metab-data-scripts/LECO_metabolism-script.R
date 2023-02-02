# LECO metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
LECO_DO = clean_DO(siteCode ='LECO', save = FALSE, return = TRUE);names(LECO_DO)
LECO_DO_xts = xts(LECO_DO[,grep("DO_*",names(LECO_DO))], order.by = LECO_DO$timePeriod)
dygraph(LECO_DO_xts, main = "LECO DOs") %>% dyRangeSelector()

LECO101badDates = c(seq(as.Date("2018-05-30"), as.Date("2018-06-20"), by =1))

LECO102badDates = c(seq(as.Date("2017-09-22"), as.Date("2017-11-01"), by =1),
                    seq(as.Date("2020-04-14"), as.Date("2020-05-28"), by = 1))

LECO112badDates = c(seq(as.Date("2022-04-11"), as.Date("2022-05-03"), by =1),
                    seq(as.Date("2020-05-27"), as.Date("2022-06-02"), by = 1))


LECO_DO = LECO_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% LECO101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% LECO102badDates, NA_real_, DO_102),
                DO_112 = ifelse(as.Date(timePeriod) %in% LECO112badDates, NA_real_, DO_112))

lm101to102 = lm(DO_101~DO_102, LECO_DO);summary(lm101to102);plot(DO_102~DO_101, LECO_DO)
lm102to112 = lm(DO_112~DO_101, LECO_DO);summary(lm102to112);plot(DO_112~DO_102, LECO_DO)
lm101to112 = lm(DO_101~DO_112, LECO_DO);summary(lm101to112);plot(DO_112~DO_101, LECO_DO)

LECO_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_102 = LECO_DO$DO_102))
LECO_DO$pred2 = predict(lm101to112, newdata = data.frame(DO_112 = LECO_DO$DO_112))

LECO_clean_DO = LECO_DO %>% 
  dplyr::mutate(DO.obs = case_when(is.na(DO_101) & is.na(DO_112) & !is.na(DO_102) ~ pred1,
                                   is.na(DO_101) & is.na(DO_102) & !is.na(DO_112) ~ pred2,
                                   is.na(DO_101) & is.na(DO_102) & is.na(DO_112) ~ NA_real_,
                                   TRUE ~ DO_101)) %>% 
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)

plot(ts(LECO_clean_DO$DO_102))
saveRDS(LECO_clean_DO, file = here::here("ignore/site-gpp-data/LECO_clean_DO.rds"))
dev.off()
LECO_temp = clean_temp(siteCode = 'LECO', return = TRUE, save = FALSE);names(LECO_temp)
LECO_temp_xts = xts(LECO_temp[,grep("temp_*",names(LECO_temp))], order.by = LECO_temp$timePeriod)
dygraph(LECO_temp_xts, main = "LECO temps") %>% dyRangeSelector()

LECO101badDates = c(seq(as.Date("2017-11-07"), as.Date("2017-11-22"), by = 1),
                    seq(as.Date("2022-06-09"), as.Date("2022-08-17"), by = 1))

LECO102badDates = c(seq(as.Date("2018-07-26"), as.Date("2018-07-30"), by = 1))

LECO112badDates = c(seq(as.Date("2020-02-06"), as.Date("2020-06-01"), by = 1),
                    seq(as.Date("2022-01-09"), as.Date("2022-01-11"), by = 1))

LECO_temp = LECO_temp %>% 
  dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% LECO101badDates, NA_real_, temp_101),
                temp_102 = ifelse(as.Date(timePeriod) %in% LECO102badDates, NA_real_, temp_102),
                temp_112 = ifelse(as.Date(timePeriod) %in% LECO112badDates, NA_real_, temp_112))

lm101to102 = lm(temp_102~temp_101, LECO_temp);summary(lm101to102);plot(temp_102~temp_101, LECO_temp)
lm101to112 = lm(temp_112~temp_101, LECO_temp);summary(lm101to112);plot(temp_112~temp_101, LECO_temp)
lm102to112 = lm(temp_112~temp_102, LECO_temp);summary(lm102to112);plot(temp_112~temp_102, LECO_temp)

LECO_temp$pred2 = predict(lm101to112, newdata = data.frame(temp_101 = LECO_temp$temp_101))
LECO_temp$pred3 = predict(lm102to112, newdata = data.frame(temp_102 = LECO_temp$temp_102))

LECO_clean_temp = LECO_temp %>% 
  dplyr::mutate(temp.obs = case_when(is.na(temp_112) & !is.na(temp_101) & !is.na(temp_102) ~ temp_101,
                                     is.na(temp_112) & is.na(temp_101) & !is.na(temp_102) ~ pred3,
                                     is.na(temp_112) & !is.na(temp_101) & is.na(temp_102) ~ pred2,
                                     is.na(temp_112) & is.na(temp_101) & is.na(temp_102) ~ NA_real_,
                                     TRUE ~ temp_112)) %>% 
  dplyr::select(timePeriod, temp_102 = 'temp.obs', hour)
plot(ts(LECO_clean_temp$temp_102))
saveRDS(LECO_clean_temp, file = here::here("ignore/site-gpp-data/LECO_clean_temp.rds"))
dev.off()

LECO_tempLM = readRDS(file = "./ignore/metab-models/LECO_tempLM.rds")
LECO_clean_temp = LECO_clean_temp %>%
  cbind(.,predict(LECO_tempLM,
                  .,
                  se.fit = FALSE,
                  type = 'response'))

rstudioapi::jobRunScript(
  path = "./ignore/metab-models/LECO_metModel.R",
  name = "LECO metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

LECO_full_mle = readRDS("./ignore/metab-models/LECO_full_mle.rds")


LECO_mle_params = get_params(LECO_full_mle, uncertainty = 'ci') %>%
  dplyr::mutate(GPP.daily = case_when(GPP.daily < 0 ~ 0,
                                      GPP.daily > 72 ~ NA_real_,
                                      TRUE ~ GPP.daily))

LECO_mle_params %>%
  ggplot()+geom_line(aes(x = date, y = GPP.daily), color = 'green')





plot_met_series(LECO_met_full)
# remove some data points above 25 which are anomolous
debugonce(clean_met_data)
LECO_met_clean = clean_met_data(LECO_met_full, doCutOff = 15)
# Quick plot to check out the data series

LECO_met_clean %>% 
  # dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs, color = as.logical(outQF)))+
  theme_minimal()

LECO_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/LECO2019_met.rds")
LECO_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/LECO2020_met.rds")

LECO_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/LECO2021_met.rds")

LECO_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/LECO2022_met.rds")

plot_site("LECO")

LECO_met %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2019,2020)) %>%
  saveRDS("./data/derived/clean-met-files/LECO_met.rds")


rstudioapi::jobRunScript(
  path = "./ignore/metab-models/LECO_metModel.R",
  name = "LECO metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
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
mm_bin <- metab(bayes_specs, data=LECO_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))