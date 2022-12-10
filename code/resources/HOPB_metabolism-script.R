# HOPB metabolism script
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
HOPB_DO = clean_DO(siteCode ='HOPB', return = TRUE)

plot(x = HOPB_DO$timePeriod, y = HOPB_DO$DO_101, type = 'l')
lines(x = HOPB_DO$timePeriod, y = HOPB_DO$DO_102, type = 'l', col = 'red')
lines(x = HOPB_DO$timePeriod, y = HOPB_DO$DO_111, type = 'l', col = 'blue')
lines(x = HOPB_DO$timePeriod, y = HOPB_DO$DO_112, type = 'l', col = 'green')

lm101to102= lm(DO_102 ~ DO_101, data = HOPB_DO)
lm102to112 = lm(DO_112 ~ DO_102, data = HOPB_DO)
summary(lm102to112)

HOPB_DO$pred1 = predict(lm101to102)
HOPB_DO$pred2 = predict(lm102to112)
HOPB_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na() & is.na(DO_102) ~ DO_111,
                                   is.na(DO_101) & is.na(DO_102) & is.na(DO_111) ~ DO_112,
                                   TRUE ~ DO_102))

plot(HOPB_DO$DO_102,HOPB_DO$DO_112)

clean_temp(siteCode = 'HOPB', return = FALSE)
HOPB_clean_DO1 = readRDS(file = "./ignore/site-gpp-data/HOPB_clean_DO.rds")
HOPB_clean_temp = readRDS(file = "./ignore/site-gpp-data/HOPB_clean_temp.rds")
# latlong = read_csv(file = "./data/site_latlong.csv")
debugonce(get_site_data)
HOPB_met = get_site_data(siteCode = "HOPB")

# remove some data points above 25 which are anomolous

HOPB_met_clean = clean_met_data(HOPB_met)
# Quick plot to check out the data series

HOPB_met_clean %>% 
  # dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs))+
  theme_minimal()

plot_site("HOPB")

HOPB_met %>%
  filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  saveRDS("./ignore/site-gpp-data/clean-met-files/HOPB_met.rds")

HOPB_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2018) %>%
  saveRDS("./data/derived_data/clean-met-files/HOPB2018_met.rds")
HOPB_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/HOPB2019_met.rds")
HOPB_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/HOPB2020_met.rds")
HOPB_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/HOPB2021_met.rds")
HOPB_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/HOPB2022_met.rds")

# HOPB test 

HOPB_test = HOPB_met %>%
  dplyr::filter(between(solar.time, as.POSIXct("2017-10-03 00:00:00"), as.POSIXct("2017-10-15 04:00:00")))

HOPB_test %>%
  
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
mm_bin <- metab(bayes_specs, data=HOPB_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))