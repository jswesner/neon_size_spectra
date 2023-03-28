# SYCA metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
debugonce(clean_DO)
SYCA_DO = clean_DO(siteCode ='SYCA', save = FALSE, return = TRUE);names(SYCA_DO)
SYCA_DO_xts = xts(SYCA_DO[,grep("DO_*",names(SYCA_DO))], order.by = SYCA_DO$timePeriod)
dygraph(SYCA_DO_xts, main = "SYCA DOs") %>% dyRangeSelector()

SYCA101badDates = c(seq(as.Date("2019-09-23"), as.Date("2019-10-03"), by = 1),
                    seq(as.Date("2019-11-29"), as.Date("2019-12-16"), by = 1),
                    seq(as.Date("2020-01-12"), as.Date("2020-01-14"), by = 1),
                    seq(as.Date("2020-02-11"), as.Date("2020-02-14"), by = 1),
                    seq(as.Date("2020-03-13"), as.Date("2020-06-01"), by = 1),
                    seq(as.Date("2021-07-15"), as.Date("2021-07-16"), by = 1),
                    seq(as.Date("2021-07-22"), as.Date("2021-08-12"), by = 1),
                    seq(as.Date("2022-08-05"), as.Date("2022-08-11"), by = 1))

SYCA102badDates = c(seq(as.Date("2019-02-10"), as.Date("2019-02-12"), by = 1),
                    seq(as.Date("2019-09-23"), as.Date("2019-10-03"), by = 1),
                    seq(as.Date("2019-11-29"), as.Date("2019-12-16"), by = 1),
                    seq(as.Date("2019-12-29"), as.Date("2020-01-14"), by = 1), #make 101 here
                    seq(as.Date("2020-10-19"), as.Date("2020-11-21"), by = 1),
                    seq(as.Date("2021-07-17"), as.Date("2021-07-21"), by = 1),
                    seq(as.Date("2021-07-23"), as.Date("2021-10-06"), by = 1),
                    seq(as.Date("2022-07-28"), as.Date("2022-08-01"), by = 1))

SYCA_DO = SYCA_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% SYCA101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% SYCA102badDates, NA_real_, DO_102))

# bad correlation
lm101to102= lm(DO_102 ~ DO_101, data = SYCA_DO);summary(lm101to102);plot(DO_102 ~ DO_101, data = SYCA_DO)
gam101to102 = mgcv::gam(DO_102 ~ s(DO_101, bs = 'tp') + s(hour, bs = 'cc', k = 24), data = SYCA_DO);gam.check(gam101to102)

# pretty bad correlation
# fill with predicted values
SYCA_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = SYCA_DO$DO_101));plot(SYCA_DO$DO_101,SYCA_DO$DO_102);abline(0,1)
SYCA_DO$pred2 = predict(gam101to102, newdata = data.frame(DO_101 = SYCA_DO$DO_101, hour = SYCA_DO$hour))

plot(SYCA_DO$DO_102~SYCA_DO$pred2);abline(0,1)

SYCA_clean_DO = SYCA_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred2,
                                   is.na(DO_102) & is.na(DO_101) ~ NA_real_,
                                   TRUE ~ DO_102)) %>%
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
dygraph(xts(SYCA_clean_DO$DO_102, order.by = SYCA_clean_DO$timePeriod))

saveRDS(SYCA_clean_DO, file = here::here("ignore/site-gpp-data/SYCA_clean_DO.rds"))
rm(list = ls())
source("./code/resources/01_load-packages.R")
SYCA_temp = clean_temp(siteCode = 'SYCA', return = TRUE, save = FALSE)
SYCA_temp_xts = xts(SYCA_temp[,grep("temp_*",names(SYCA_temp))], order.by = SYCA_temp$timePeriod)
dygraph(SYCA_temp_xts, main = "SYCA temps") %>% dyRangeSelector()

SYCA101badDates = c()

SYCA102badDates = c(seq(as.Date("2020-09-05"), as.Date("2020-10-20"), by = 1),
                    seq(as.Date("2021-05-16"), as.Date("2021-09-07"), by = 1))#make temp_101 here

SYCA_temp = SYCA_temp %>% 
  dplyr::mutate(temp_101 = ifelse(as.Date(timePeriod) %in% SYCA101badDates, NA_real_, temp_101),
                temp_102 = ifelse(as.Date(timePeriod) %in% SYCA102badDates, NA_real_, temp_102))

lm101to102= lm(temp_102 ~ temp_101,data = SYCA_temp);summary(lm101to102);plot(temp_102 ~ temp_101, data = SYCA_temp)

# fill with predicted values
SYCA_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = SYCA_temp$temp_101))

SYCA_clean_temp = SYCA_temp %>%
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     is.na(temp_102) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_102)) %>%
  dplyr::select(timePeriod, temp_102 = "temp.obs", hour)
plot(ts(SYCA_clean_temp$temp_102))

saveRDS(SYCA_clean_temp, file = here::here("ignore/site-gpp-data/SYCA_clean_temp.rds"))

rm(list = ls())
source("./code/resources/01_load-packages.R")
debugonce(clean_Q)
SYCA_Q = clean_Q(siteCode ='SYCA', save = FALSE, return = TRUE, QLims = c(0,5e4));names(SYCA_Q)
SYCA_ZQ_xts = xts(SYCA_Q[,grep("Q.*|Z.",names(SYCA_Q))], order.by = SYCA_Q$timePeriod)

dygraph(SYCA_ZQ_xts, main = "SYCA Qs") %>% dyRangeSelector()

SYCA_Q_badDates = c(seq(as.Date("2022-01-22"), as.Date("2022-08-31"), by = 1))

SYCA_Q = SYCA_Q %>% 
  dplyr::filter(as.Date(timePeriod) %ni% SYCA_Q_badDates,
                Q.obs > 0,
                !is.na(Z.obs))


ZQ_priors = c(set_prior('normal(0, 5)', nlpar = "a"),
              set_prior('normal(1, 0.5)', nlpar = "b", lb = 0))

SYCA_ZQmod = brm(bf(Q.obs ~ s(Z.obs, bs = 'tp', k = 4)), 
                 data = SYCA_Q, 
                 family = Gamma(link = 'log'),#
                 # prior = ZQ_priors,
                 chains= 4, cores = 4, seed = 42,
                 iter = 500, thin = 1,
                 # control = list(adapt_delta = 0.99),
                 save_pars = save_pars(all = TRUE),
                 file = "./ignore/metab-models/SYCAZQmod",
                 file_refit = 'always',
                 backend = 'cmdstanr')

conditional_effects(SYCA_ZQmod, points = TRUE)
pp_check(SYCA_ZQmod)

SYCA_Q$pred = predict(SYCA_ZQmod, newdata = data.frame(Z.obs = SYCA_Q$Z.obs))

SYCA_Q %>% 
  na.omit %>% 
  ggplot()+
  geom_line(aes(x = Z.obs, y = pred[,"Estimate"]), color = 'grey', alpha = 0.5)+
  geom_point(aes(x = Z.obs, y = Q.obs), color = 'blue', size = 1)+
  scale_y_continuous(limits = c(0,5e4))


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
mm_bin <- metab(bayes_specs, data=SYCA_test)#;tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))