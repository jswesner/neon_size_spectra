# BIGC metabolism script
source("./code/resources/01_load-packages.R")
theme_set(theme_minimal())
# debugonce(clean_DO)
clean_DO(siteCode ='BIGC')
# debugonce(clean_temp)
clean_temp(siteCode = 'BIGC', return = FALSE)
BIGC_clean_temp = readRDS(file = "./ignore/site-gpp-data/BIGC_clean_temp.rds")
BIGC_clean_DO = readRDS("./ignore/site-gpp-data/BIGC_clean_DO.rds")
plot(ts(BIGC_clean_DO$DO_102))
plot(ts(BIGC_clean_temp$temp_102))
rstudioapi::jobRunScript(
  path = "./ignore/metab-models/BIGC_doLM.R",
  name = "BIGC LM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)
rstudioapi::jobRunScript(
  path = "./ignore/metab-models/BIGC_tempLM.R",
  name = "BIGC tempLM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

BIGC_tempLM = readRDS(file = "./ignore/metab-models/BIGC_tempLM.rds")
BIGC_clean_temp = BIGC_clean_temp %>%
  cbind(.,predict(BIGC_tempLM,
                  .,
                  se.fit = FALSE,
                  type = 'response'))

rstudioapi::jobRunScript(
  path = "./ignore/metab-models/BIGC_metModel.R",
  name = "BIGC metMM",
  workingDir = getwd(),
  importEnv = FALSE,
  exportEnv = FALSE
)

BIGC_full_mle = readRDS("./ignore/metab-models/BIGC_full_mle.rds")


BIGC_mle_params = get_params(BIGC_full_mle, uncertainty = 'ci') %>%
  dplyr::mutate(GPP.daily = case_when(GPP.daily < 0 ~ 0,
                                      GPP.daily > 72 ~ NA_real_,
                                      TRUE ~ GPP.daily))

BIGC_mle_params %>%
  ggplot()+geom_line(aes(x = date, y = GPP.daily), color = 'green')





plot_met_series(BIGC_met_full)
# remove some data points above 25 which are anomolous
debugonce(clean_met_data)
BIGC_met_clean = clean_met_data(BIGC_met_full, doCutOff = 15)
# Quick plot to check out the data series

BIGC_met_clean %>% 
  # dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2018,2019)) %>%
  ggplot()+
  geom_line(aes(x = solar.time, y = DO.obs, color = as.logical(outQF)))+
  theme_minimal()

BIGC_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2019) %>%
  saveRDS("./data/derived_data/clean-met-files/BIGC2019_met.rds")
BIGC_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2020)%>%
  saveRDS("./data/derived_data/clean-met-files/BIGC2020_met.rds")

BIGC_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2021)%>%
  saveRDS("./data/derived_data/clean-met-files/BIGC2021_met.rds")

BIGC_met_clean %>%
  dplyr::filter(lubridate::year(solar.time) == 2022)%>%
  saveRDS("./data/derived_data/clean-met-files/BIGC2022_met.rds")

plot_site("BIGC")

BIGC_met %>% 
  dplyr::filter(!is.na(solar.time)) %>%
  dplyr::filter(!as.logical(outQF)) %>%
  # dplyr::filter(lubridate::year(solar.time) %in% c(2019,2020)) %>%
  saveRDS("./data/derived/clean-met-files/BIGC_met.rds")


rstudioapi::jobRunScript(
  path = "./ignore/metab-models/BIGC_metModel.R",
  name = "BIGC metMM",
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
mm_bin <- metab(bayes_specs, data=BIGC_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))