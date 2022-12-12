# OKSR metabolism script
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
clean_DO(siteCode ='OKSR', doLims = c(7.5,25), return = FALSE)
clean_temp(siteCode = 'OKSR', return = FALSE)
OKSR_clean_temp = readRDS(file = "./ignore/site-gpp-data/OKSR_clean_temp.rds")

# debugonce(get_site_data)
OKSR_met = get_site_data(siteCode = "OKSR")
plot_site('OKSR')
# remove some data points above 25 which are anomolous
# debugonce(clean_met_data)
OKSR_met_clean = clean_met_data(OKSR_met)
# Quick plot to check out the data series
# tz = attr(OKSR_met$solar.time,"tzone")

OKSR_met %>% 
  # dplyr::filter(!is.na(solar.time)) %>%
  # dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs, color = as.logical(outQF)))+
  theme_minimal()

OKSR_met %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) %in% c(2019,2020)) %>%
  saveRDS("./ignore/site-gpp-data/clean-met-files/OKSR_met.rds")


OKSR_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2018) %>%
  saveRDS("./data/derived_data/clean-met-files/OKSR2018_met.rds")
OKSR_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/OKSR2019_met.rds")
OKSR_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/OKSR2020_met.rds")
OKSR_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/OKSR2021_met.rds")
OKSR_met_clean %>%
  dplyr::filter(!as.logical(outQF)) %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/OKSR2022_met.rds")

# OKSR test 
tz(OKSR_met$solar.time)


OKSR_test = OKSR_met %>%
  dplyr::filter(between(solar.time, as.POSIXct("2017-10-03 00:00:00"), as.POSIXct("2017-10-15 04:00:00")))

OKSR_test %>%
 
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
mm_bin <- metab(bayes_specs, data=OKSR_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))