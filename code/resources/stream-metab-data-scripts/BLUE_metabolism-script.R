# BLUE metabolism script
source("./code/resources/01_load-packages.R")
theme_set(theme_minimal())

# debugonce(clean_DO)
BLUE_DO = clean_DO(siteCode ='BLUE', save = FALSE, return = TRUE);names(BLUE_DO)
BLUE_DO_xts = xts(BLUE_DO[,grep("DO",names(BLUE_DO))], order.by= BLUE_DO$timePeriod)
dygraph(BLUE_DO_xts, main = "Blue DOs") %>% dyRangeSelector()


BLUEbadDates = c(seq(as.Date("2018-12-14"), as.Date("2019-01-02"), by = 1),
                 seq(as.Date("2021-05-18"), as.Date("2021-07-08"), by = 1),
                 seq(as.Date("2021-11-01"), as.Date("2021-12-30"), by = 1))
BLUE_clean_DO = BLUE_DO %>% 
  select(timePeriod, DO_102 = DO_112, hour) %>% 
  dplyr::mutate(DO_102 = case_when(as.Date(timePeriod) %in% BLUEbadDates ~ NA_real_,
                                   TRUE ~ DO_102)) 
plot(ts(BLUE_clean_DO$DO_102))
saveRDS(BLUE_clean_DO, here::here("ignore/site-gpp-data/BLUE_clean_DO.rds"))

# debugonce(clean_temp)
BLUE_temp = clean_temp(siteCode = 'BLUE', save = FALSE, return = TRUE);names(BLUE_temp)
BLUE_temp_xts = xts(BLUE_temp[,grep('temp',names(BLUE_temp))], order.by = BLUE_temp$timePeriod)
dygraph(BLUE_temp_xts, main = "BLUE temp_112") %>% dyRangeSelector()

BLUE_clean_temp = BLUE_temp %>% 
  dplyr::mutate(temp_112 = case_when(is.na(temp_112) & !is.na(temp_102) ~ temp_102,
                                     TRUE ~ temp_112)) %>% 
  select(timePeriod, temp_102 = temp_112, hour) %>% 
  dplyr::mutate(temp_102 = case_when(as.Date(timePeriod) %in% seq(as.Date("2021-02-22"),as.Date("2021-02-24"), by = 1) ~ NA_real_,
                                     TRUE ~ temp_102)) 
plot(ts(BLUE_clean_temp$temp_102))
saveRDS(BLUE_clean_temp, file = here::here("ignore/site-gpp-data/BLUE_clean_temp.rds"))


# check to see that the file columns make sense
BLUE_clean_temp = readRDS(file = "./ignore/site-gpp-data/BLUE_clean_temp.rds")
BLUE_clean_DO = readRDS("./ignore/site-gpp-data/BLUE_clean_DO.rds")
debugonce(get_site_data)
BLUE_met = get_site_data(siteCode = "BLUE")

BLUE_met_clean = clean_met_data(BLUE_met, doCutOff = 18)


plot_met_series(BLUE_met_clean)

x = get_site_data(siteCode = "BLUE")
# remove some data points above 25 which are anomolous

# Quick plot to check out the data series

BLUE_met_clean %>% 
  # dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs, color = as.logical(outQF)))+
  theme_minimal()

rstudioapi::jobRunScript(
  path = "./ignore/metab-models/BLUE_metModel.R",
  name = "BLUE metMM",
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
mm_bin <- metab(bayes_specs, data=BLUE_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))