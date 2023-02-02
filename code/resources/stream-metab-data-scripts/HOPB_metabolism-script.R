# HOPB metabolism script
rm(list = ls())
source("./code/resources/01_load-packages.R")
# debugonce(clean_DO)
HOPB_DO = clean_DO(siteCode ='HOPB', save = FALSE, return = TRUE);names(HOPB_DO)
HOPB_DO_xts = xts(HOPB_DO[,grep("DO_*",names(HOPB_DO))], order.by = HOPB_DO$timePeriod)
dygraph(HOPB_DO_xts, main = "HOPB DOs") %>% dyRangeSelector()

HOPB101badDates = c(seq(as.Date("2018-11-02"), as.Date("2018-11-07"), by = 1),
                    seq(as.Date("2019-03-29"), as.Date("2019-03-30"), by =1))

HOPB102badDates = c(seq(as.Date("2019-10-30"), as.Date("2019-11-01"), by =1))

HOPB111badDates = c(seq(as.Date("2019-10-30"), as.Date("2019-11-01"), by =1),
                    seq(as.Date("2020-06-04"), as.Date("2020-06-16"), by = 1))

HOPB112badDates = c(seq(as.Date("2019-10-30"), as.Date("2019-11-01"), by =1))

HOPB_DO = HOPB_DO %>% 
  dplyr::mutate(DO_101 = ifelse(as.Date(timePeriod) %in% HOPB101badDates, NA_real_, DO_101),
                DO_102 = ifelse(as.Date(timePeriod) %in% HOPB102badDates, NA_real_, DO_102),
                DO_111 = ifelse(as.Date(timePeriod) %in% HOPB111badDates, NA_real_, DO_111),
                DO_112 = ifelse(as.Date(timePeriod) %in% HOPB112badDates, NA_real_, DO_112))

lm101to102= lm(DO_102 ~ DO_101, data = HOPB_DO);summary(lm101to102)
lm102to112 = lm(DO_112 ~ DO_102, data = HOPB_DO);summary(lm102to112)
lm111to112 = lm(DO_112 ~ DO_111, data = HOPB_DO);summary(lm111to112)

# fill with predicted values
HOPB_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = HOPB_DO$DO_101))
HOPB_DO$pred2 = predict(lm111to112, newdata = data.frame(DO_111 = HOPB_DO$DO_111))
HOPB_DO$pred3 = predict(lm102to112, newdata = data.frame(DO_102 = HOPB_DO$DO_102))

HOPB_clean_DO = HOPB_DO %>%
  dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
                                   is.na(DO_112) & is.na(DO_102) & is.na(DO_111) ~ NA_real_,
                                   !is.na(DO_102) & is.na(DO_112) & is.na(DO_111) ~ pred2,
                                   is.na(DO_112) & !is.na(DO_111) ~ pred2,
                                   TRUE ~ DO_112)) %>%
  dplyr::select(timePeriod, DO_102 = "DO.obs", hour)

saveRDS(HOPB_clean_DO, file = here::here("ignore/site-gpp-data/HOPB_clean_DO.rds"))
dev.off()
HOPB_temp = clean_temp(siteCode = 'HOPB', return = TRUE, save = FALSE);names(HOPB_temp)
HOPB_temp_xts = xts(HOPB_temp[,grep("temp_*",names(HOPB_temp))], order.by = HOPB_temp$timePeriod)
dygraph(HOPB_temp_xts, main = "HOPB temps") %>% dyRangeSelector()

HOPB111badDates = c(seq(as.Date("2020-06-29"), as.Date("2020-10-22"), by = 1),
                    seq(as.Date("2022-06-24"), as.Date("2022-07-21"), by = 1))

HOPB_temp = HOPB_temp %>% 
  dplyr::mutate(temp_111 = ifelse(as.Date(timePeriod) %in% HOPB101badDates, NA_real_, temp_101))

# bad correction, not as bad as DO though
lm101to102= lm(temp_102 ~ temp_101, data = HOPB_temp);summary(lm101to102)
lm102to112 = lm(temp_112 ~ temp_102, data = HOPB_temp);summary(lm102to112)
lm111to112 = lm(temp_112 ~ temp_111, data = HOPB_temp);summary(lm111to112)

# fill with predicted values
HOPB_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = HOPB_temp$temp_101))
HOPB_temp$pred2 = predict(lm102to112, newdata = data.frame(temp_102 = HOPB_temp$temp_102))

HOPB_clean_temp = HOPB_temp %>% 
  dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
                                     !is.na(temp_102) & is.na(temp_111) & is.na(temp_112) ~ pred2,
                                     is.na(temp_102) & is.na(temp_112) & !is.na(temp_111) ~ temp_111,
                                     is.na(temp_102) & is.na(temp_111) & is.na(temp_112) & is.na(temp_101) ~ NA_real_,
                                     TRUE ~ temp_112)) %>% 
  dplyr::select(timePeriod, temp_102 = 'temp.obs', hour)

saveRDS(HOPB_clean_temp, file = here::here("ignore/site-gpp-data/HOPB_clean_temp.rds"))
dev.off()
# clean all the DO probes
# # HOPB_DO = clean_DO(siteCode ='HOPB', return = TRUE, save = FALSE)
# 
# lm101to102= lm(DO_102 ~ DO_101, data = HOPB_DO)
# lm101to112 = lm(DO_112 ~ DO_101, data = HOPB_DO)
# lm102to112 = lm(DO_112 ~ DO_102, data = HOPB_DO)
# lm111to112 = lm(DO_112 ~ DO_111, data = HOPB_DO)
# # summary(lm111to112)
# 
# # fill with predicted values
# HOPB_DO$pred1 = predict(lm101to102, newdata = data.frame(DO_101 = HOPB_DO$DO_101))
# HOPB_DO$pred2 = predict(lm111to112, newdata = data.frame(DO_111 = HOPB_DO$DO_111))
# HOPB_DO$pred3 = predict(lm102to112, newdata = data.frame(DO_102 = HOPB_DO$DO_102))
# 
# HOPB_clean_DO = HOPB_DO %>%
#   dplyr::mutate(DO.obs = case_when(is.na(DO_102) & !is.na(DO_101) ~ pred1,
#                                    is.na(DO_112) & is.na(DO_102) ~ pred2,
#                                    is.na(DO_112) & !is.na(DO_102) ~ pred3,
#                                    TRUE ~ DO_112)) %>%
#   dplyr::select(timePeriod, DO_102 = "DO.obs", hour)
# 
# # saveRDS(HOPB_clean_DO, file = "./ignore/site-gpp-data/HOPB_clean_DO.rds")
# 
# # debugonce(clean_temp)
# HOPB_temp = clean_temp(siteCode = 'HOPB', return = TRUE, save = FALSE)
# 
# plot(x = HOPB_temp$timePeriod, y = HOPB_temp$temp_101, type = 'l')
# lines(x = HOPB_temp$timePeriod,y = HOPB_temp$temp_102, col = 'red')
# lines(x = HOPB_temp$timePeriod,y = HOPB_temp$temp_111, col = 'blue')
# lines(x = HOPB_temp$timePeriod,y = HOPB_temp$temp_112, col = 'green')
# 
# lm101to102 = lm(temp_102 ~ temp_101, data = HOPB_temp)
# lm102to112 = lm(temp_112 ~ temp_102, data = HOPB_temp)
# lm111to112 = lm(temp_112 ~ temp_111, data = HOPB_temp)
# summary(lm101to102)
# 
# # fill with predicted values
# HOPB_temp$pred1 = predict(lm101to102, newdata = data.frame(temp_101 = HOPB_temp$temp_101))
# HOPB_temp$pred2 = predict(lm111to112, newdata = data.frame(temp_111 = HOPB_temp$temp_111))
# HOPB_temp$pred3 = predict(lm102to112, newdata = data.frame(temp_102 = HOPB_temp$temp_102))
# 
# HOPB_clean_temp = HOPB_temp %>%
#   dplyr::mutate(temp.obs = case_when(is.na(temp_102) & !is.na(temp_101) ~ pred1,
#                                      is.na(temp_112) & is.na( temp_102) ~ pred2,
#                                      is.na(temp_112) & !is.na(temp_102) ~ pred3,
#                                      TRUE ~ temp_112)) %>%
#   dplyr::select(timePeriod, temp_102 = "temp.obs", hour)
# 
# plot(ts(HOPB_clean_temp$temp_112))
# 
# saveRDS(HOPB_clean_temp, file = "./ignore/site-gpp-data/HOPB_clean_temp.rds")

HOPB_clean_DO = readRDS(file = "./ignore/site-gpp-data/HOPB_clean_DO.rds")
HOPB_clean_temp = readRDS(file = "./ignore/site-gpp-data/HOPB_clean_temp.rds")

# debugonce(get_site_data)
HOPB_met = get_site_data(siteCode = "HOPB")

# remove some data points above 25 which are anomolous
HOPB_met_clean = clean_met_data(HOPB_met)

plot_site("HOPB")
# Quick plot to check out the data series


## run a quick version of the 
bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,
                      err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
bayes_specs <- specs(bayes_name)
# bayes_specs
# Model specifications:
# model_name                 b_Kn_oipi_tr_plrckm.stan
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
bayes_specs <- revise(bayes_specs, day_start = 0, day_end = 24, burnin_steps= 1000, saved_steps= 1000,thin_steps = 5, n_cores=2, n_chains = 2, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = TRUE)

HOPB_met_run = HOPB_met_clean %>% dplyr::select(-c(DO.pctsat, outQF, probQF, doDiff, doSatDiff))
tictoc::tic();mm_bin <- metab(bayes_specs, data=HOPB_met_run);tictoc::toc()

mm_bin

plot_metab_preds(mm_bin)

# get_params(mm)

plot_DO_preds(mm_bin)
predict_metab(mm)

x = predict_metab(mm_bin)

x %>% ggplot()+geom_point(aes(x = GPP, y = ER))