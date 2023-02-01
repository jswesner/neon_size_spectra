# KING metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
KING_DO = clean_DO(siteCode ='KING', save = FALSE, return = TRUE);names(KING_DO)
KING_DO_xts = xts(KING_DO[,grep("DO_*",names(KING_DO))], order.by = KING_DO$timePeriod)
dygraph(KING_DO_xts, main = "KING DOs") %>% dyRangeSelector()

# the DO_101 series is very decoupled from DO_102, cannot impute from it
KING101badDates = c(seq(as.Date("2017-03-13"), as.Date("2017-04-04"), by = 1),
                    seq(as.Date("2017-09-22"), as.Date("2017-09-27"), by = 1),
                    seq(as.Date("2018-10-11"), as.Date("2018-10-31"), by = 1),
                    seq(as.Date("2018-05-07"), as.Date("2018-05-08"), by = 1))

KING102badDates = c(seq(as.Date("2017-03-13"), as.Date("2017-04-04"), by = 1),
                    seq(as.Date("2018-10-11"), as.Date("2018-10-31"), by = 1),
                    seq(as.Date("2018-05-07"), as.Date("2018-05-08"), by = 1),
                    seq(as.Date("2021-08-22"), as.Date("2021-09-08"), by = 1))

KING_clean_DO = KING_DO %>% 
  dplyr::mutate(DO_102 = ifelse(as.Date(timePeriod) %in% KING102badDates, NA_real_, DO_102)) %>% 
  dplyr::select(timePeriod, DO_102, hour)

saveRDS(KING_clean_DO, file = here::here("ignore/site-gpp-data/KING_clean_DO.rds"))
dev.off()
# temperature data are 'good', but no imputing from temp_101
KING_temp = clean_temp(siteCode = 'KING', return = TRUE, save = TRUE);names(KING_temp)
KING_temp_xts = xts(KING_temp[,grep("temp_*",names(KING_temp))], order.by = KING_temp$timePeriod)
dygraph(KING_temp_xts, main = "KING temps") %>% dyRangeSelector()



# remove some data points above 25 which are anomolous
# debugonce(clean_met_data)
KING_met_clean = clean_met_data(KING_met)
# Quick plot to check out the data series
# tz = attr(KING_met$solar.time,"tzone")

KING_met_clean %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs))+
  theme_minimal()

KING_met %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) %in% c(2019,2020)) %>%
  saveRDS("./ignore/site-gpp-data/clean-met-files/KING_met.rds")

KING_met_clean %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2018) %>%
  saveRDS("./data/derived_data/clean-met-files/KING2018_met.rds")
KING_met_clean %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/KING2019_met.rds")
KING_met_clean %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/KING2020_met.rds")
KING_met_clean %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/KINGB2021_met.rds")
KING_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/KING2022_met.rds")





KING_test %>%
 
## run a quick version of the 
bayes_name <- mm_name(type='bayes', pool_K600='complete', err_obs_iid=TRUE,
                      err_proc_iid=TRUE, err_proc_GPP = TRUE, ode_method = "trapezoid")
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
mm_bin <- metab(bayes_specs, data=KING_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))