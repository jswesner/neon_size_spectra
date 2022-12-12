rm(list = ls())
source("./code/resources/01_load-packages.R")
HOPB_met_full = get_site_data(siteCode = "HOPB") %>%
  dplyr::select(-DO.pctsat)

plot_met_series(HOPB_met_full)

discharge.daily = HOPB_met_full %>%
  dplyr::mutate(date = as.Date(solar.time)) %>%
  group_by(date) %>%
  dplyr::summarise(discharge = mean(discharge, na.rm = TRUE))

## 
mle_specs <- specs(mm_name(type = "mle"))
# debugonce(metab_mle)
mm1 <- metab_mle(specs(mm_name(type = "mle")), data = HOPB_met_full)

k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
  select(date, K600.daily, K600.daily.lower, K600.daily.upper) %>%
  left_join(HOPB_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

k600_mm1 = k600_mm1 %>%
  dplyr::filter(K600.daily < 400) 
# # 
km1 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.5),
                          day_start = -1, day_end = 23), data_daily = k600_mm1)
km2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                          day_start = -1, day_end = 23), data_daily = k600_mm1)
km3 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
                          day_start = -1, day_end = 23), data_daily = k600_mm1)
km4 <- metab_night(data = HOPB_met_full)
# 
k600_mm2 <- get_params(km1) %>% 
  select(date, K600.daily) %>%  
  dplyr::mutate(model = 'loess') %>%
  bind_rows(get_params(km2) %>%
              select(date, K600.daily) %>%  
              dplyr::mutate(model = 'lm')) %>%
  bind_rows(get_params(km3) %>%
              select(date, K600.daily) %>%
              dplyr::mutate(model = 'mean')) %>%
  bind_rows(get_params(km4) %>%
              select(date, K600.daily) %>%
              dplyr::mutate(model = 'night')) %>%
  left_join(HOPB_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

k600_mm2 %>%
  ggplot()+
  geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
  coord_trans('log10') +
  facet_wrap(~model, scales = 'free_y')
# 
mm2 <- metab_mle(mle_specs, data = HOPB_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily))

mm3 <- metab_mle(mle_specs, data = HOPB_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily))

mm4 <- metab_mle(mle_specs, data = HOPB_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily))

mm5 <- metab_mle(mle_specs, data = HOPB_met_full, data_daily = k600_mm2 %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily))
## 
mle_specs_sat <- specs(mm_name(type = "mle", GPP_fun = 'satlight'))
mm1_sat <- metab_mle(mle_specs_sat, data = HOPB_met_full)

# identify negative values
k600_mm1_sat <- get_params(mm1_sat, uncertainty = 'ci') %>%
  left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
  select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
  left_join(HOPB_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) #%>%
# dplyr::filter(GPP.daily >0,
#              K600.daily < 500) %>% dplyr::select(-GPP.daily)

k600_mm1_sat %>%
  ggplot()+
  geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
  coord_trans('log10')

k600_mm1_sat %>%
  ggplot()+
  geom_point(aes(x = GPP.daily, K600.daily))

k600_mm1_sat = k600_mm1_sat %>%
  dplyr::filter(K600.daily < 500,
                GPP.daily >= 1)

#
km1_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                              day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))

km2_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                              day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))

km3_sat <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
                              day_start = -1, day_end = 23), data_daily = k600_mm1_sat%>% select(-GPP.daily))

# 
k600_mm2_sat <- get_params(km1_sat) %>% 
  select(date, K600.daily) %>%  
  dplyr::mutate(model = 'loess') %>%
  bind_rows(get_params(km2_sat) %>%
              select(date, K600.daily) %>%  
              dplyr::mutate(model = 'lm')) %>%
  bind_rows(get_params(km3_sat) %>%
              select(date, K600.daily) %>%
              dplyr::mutate(model = 'mean')) %>%
  bind_rows(get_params(km4) %>%
              select(date, K600.daily) %>%
              dplyr::mutate(model = 'night')) %>%
  left_join(HOPB_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

k600_mm2_sat %>%
  ggplot()+
  geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
  coord_trans('log10') +
  facet_wrap(~model, scales = 'free_y')
# 
mm2_sat <- metab_mle(mle_specs_sat, data = HOPB_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily))

mm3_sat <- metab_mle(mle_specs_sat, data = HOPB_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily))

mm4_sat <- metab_mle(mle_specs_sat, data = HOPB_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily))

mm5_sat <- metab_mle(mle_specs_sat, data = HOPB_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily))

## satq10temp
mle_specs_satq10 <- specs(mm_name(type = "mle", GPP_fun = 'satlight', ER_fun = 'q10temp'))
mm1_satq10 <- metab_mle(mle_specs_satq10, data = HOPB_met_full)

# identify negative values
k600_mm1_satq10 <- get_params(mm1_satq10, uncertainty = 'ci') %>%
  left_join(predict_metab(mm1_sat) %>% select(date, GPP.daily = 'GPP')) %>%
  select(date, GPP.daily, K600.daily, K600.daily.lower, K600.daily.upper) %>%
  left_join(HOPB_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE))) #%>%
# dplyr::filter(GPP.daily >0,
#              K600.daily < 500) %>% dplyr::select(-GPP.daily)

k600_mm1_satq10 %>%
  ggplot()+
  geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
  coord_trans('log10')

k600_mm1_satq10 %>%
  ggplot()+
  geom_point(aes(x = GPP.daily, K600.daily))+
  coord_cartesian(ylim = c(NA, 700))

k600_mm1_satq10 = k600_mm1_satq10 %>%
  dplyr::filter(K600.daily < 500 & GPP.daily >0)
# 
km1_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'), predictors = 'discharge.daily', other_args = list(span = 0.6),
                                 day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))

km2_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'lm'),
                                 day_start = -1, day_end = 23), data_daily = k600_mm1_satq10%>% select(-GPP.daily))

km3_satq10 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'mean'),
                                 day_start = -1, day_end = 23), data_daily = k600_mm1_satq10 %>% select(-GPP.daily))
# 
k600_mm2_satq10 <- get_params(km1_satq10) %>% 
  select(date, K600.daily) %>%  
  dplyr::mutate(model = 'loess') %>%
  bind_rows(get_params(km2_satq10) %>%
              select(date, K600.daily) %>%  
              dplyr::mutate(model = 'lm')) %>%
  bind_rows(get_params(km3_satq10) %>%
              select(date, K600.daily) %>%
              dplyr::mutate(model = 'mean')) %>%
  bind_rows(get_params(km4) %>%
              select(date, K600.daily) %>%
              dplyr::mutate(model = 'night')) %>%
  left_join(HOPB_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

k600_mm2_satq10 %>%
  ggplot()+
  geom_point(aes(x = discharge.daily+0.001, y = K600.daily)) +
  coord_trans('log10') +
  facet_wrap(~model, scales = 'free_y')
# 
mm2_satq10 <- metab_mle(mle_specs_satq10, data = HOPB_met_full, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'loess') %>% dplyr::select(date, K600.daily))

mm3_satq10 <- metab_mle(mle_specs_satq10, data = HOPB_met_full, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'lm') %>% dplyr::select(date, K600.daily))

mm4_satq10 <- metab_mle(mle_specs_satq10, data = HOPB_met_full, data_daily = k600_mm2_satq10 %>% dplyr::filter(model == 'mean') %>% dplyr::select(date, K600.daily))

mm5_satq10 <- metab_mle(mle_specs_satq10, data = HOPB_met_full, data_daily = k600_mm2_sat %>% dplyr::filter(model == 'night') %>% dplyr::select(date, K600.daily))
####
modList = ls()[grep("^mm\\d{1}.*", ls())] %>% purrr::map(~eval(as.symbol(.x)))
save(modList, file = "./ignore/metab-models/mleModLists/HOPBmlemods.Rdata")

# model assessment ----
rm(list = ls())
source("./code/resources/01_load-packages.R")
load(file = "./ignore/metab-models/mleModLists/HOPBmlemods.Rdata")
modList_sub = unlist(lapply(modList, function(x) !is.data.frame(x)))
modList = modList[modList_sub]

modList = setNames(modList, nm = c("mm1","mm2","mm3","mm4","mm5","mm1_sat","mm2_sat","mm3_sat","mm4_sat","mm5_sat","mm1_satq10","mm2_satq10","mm3_satq10","mm4_satq10","mm5_satq10"))

list2env(modList, .GlobalEnv)

mods = data.frame(
  modelID = c("mm1","mm2","mm3","mm4","mm5","mm1_sat","mm2_sat","mm3_sat","mm4_sat","mm5_sat","mm1_satq10","mm2_satq10","mm3_satq10","mm4_satq10","mm5_satq10"),
  modelType = c("raw", "loess","lm","mean","night","raw", "loess","lm","mean","night","raw", "loess","lm","mean","night"),
  # gppTot = c(sum(mm1@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm2@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm3@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm4@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm5@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm1_sat@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm2_sat@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm3_sat@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm4_sat@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm5_sat@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm1_satq10@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm2_satq10@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm3_satq10@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm4_satq10@metab_daily$GPP, na.rm = TRUE),
  #            sum(mm5_satq10@metab_daily$GPP, na.rm = TRUE)),
  RSME = c(calc_mod_RSME(plot_DO_preds(mm1), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm2), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm3), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm4), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm5), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm1_sat), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm2_sat), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm3_sat), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm4_sat), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm5_sat), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm1_satq10), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm2_satq10), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm3_satq10), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm4_satq10), relative = TRUE),
           calc_mod_RSME(plot_DO_preds(mm5_satq10), relative = TRUE)),
  negativeGPP = c(count_negative_dates(mm1),
                  count_negative_dates(mm2),
                  count_negative_dates(mm3),
                  count_negative_dates(mm4),
                  count_negative_dates(mm5),
                  count_negative_dates(mm1_sat),
                  count_negative_dates(mm2_sat),
                  count_negative_dates(mm3_sat),
                  count_negative_dates(mm4_sat),
                  count_negative_dates(mm5_sat),
                  count_negative_dates(mm1_satq10),
                  count_negative_dates(mm2_satq10),
                  count_negative_dates(mm3_satq10),
                  count_negative_dates(mm4_satq10),
                  count_negative_dates(mm5_satq10)),
  positiveER = c(count_positive_dates(mm1),
                 count_positive_dates(mm2),
                 count_positive_dates(mm3),
                 count_positive_dates(mm4),
                 count_positive_dates(mm5),
                 count_positive_dates(mm1_sat),
                 count_positive_dates(mm2_sat),
                 count_positive_dates(mm3_sat),
                 count_positive_dates(mm4_sat),
                 count_positive_dates(mm5_sat),
                 count_positive_dates(mm1_satq10),
                 count_positive_dates(mm2_satq10),
                 count_positive_dates(mm3_satq10),
                 count_positive_dates(mm4_satq10),
                 count_positive_dates(mm5_satq10)),
  maxK = c(calc_max_k(mm1),
           calc_max_k(mm2),
           calc_max_k(mm3),
           calc_max_k(mm4),
           calc_max_k(mm5),
           calc_max_k(mm1_sat),
           calc_max_k(mm2_sat),
           calc_max_k(mm3_sat),
           calc_max_k(mm4_sat),
           calc_max_k(mm5_sat),
           calc_max_k(mm1_satq10),
           calc_max_k(mm2_satq10),
           calc_max_k(mm3_satq10),
           calc_max_k(mm4_satq10),
           calc_max_k(mm5_satq10)),
  meanGPP = c(calc_gpp_mean(mm1, scaler = "*0.365"),
              calc_gpp_mean(mm2, scaler = "*0.365"),
              calc_gpp_mean(mm3, scaler = "*0.365"),
              calc_gpp_mean(mm4, scaler = "*0.365"),
              calc_gpp_mean(mm5, scaler = "*0.365"),
              calc_gpp_mean(mm1_sat, scaler = "*0.365"),
              calc_gpp_mean(mm2_sat, scaler = "*0.365"),
              calc_gpp_mean(mm3_sat, scaler = "*0.365"),
              calc_gpp_mean(mm4_sat, scaler = "*0.365"),
              calc_gpp_mean(mm5_sat, scaler = "*0.365"),
              calc_gpp_mean(mm1_satq10, scaler = "*0.365"),
              calc_gpp_mean(mm2_satq10, scaler = "*0.365"),
              calc_gpp_mean(mm3_satq10, scaler = "*0.365"),
              calc_gpp_mean(mm4_satq10, scaler = "*0.365"),
              calc_gpp_mean(mm5_satq10, scaler = "*0.365"))
)
comment(mods$meanGPP) <- "g C m-2 y-1"
# comment(mods$gppTot) <- "mg O2 m-2"

knitr::kable(mods)
# debugonce(slim_models)
modsDf = slim_models(mods);knitr::kable(modsDf)
plot_metab_preds(mm1)
saveRDS(mm3_sat, "./ignore/metab-models/HOPB_full_mle.rds")
###

# ## run a quick version of the 
# bayes_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,err_proc_iid=FALSE, err_proc_GPP = TRUE, ode_method = "trapezoid")
# bayes_specs <- specs(bayes_name)
# # 
# bayes_specs <- revise(bayes_specs, day_start = 4, day_end = 28, burnin_steps= 500, saved_steps= 1000, thin_steps = 5,n_cores=3, n_chains = 3, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = TRUE)
# 
# data_daily = NULL
# info = NULL
# source("./ignore/metab-models/metab_fun.R")
# source("./ignore/metab-models/bayes_allply.R")
# # debugonce(metab_fun);
# debugonce(bayes_allply)
# metab_fun(specs = bayes_specs, data = HOPB_met_mod, data_daily = data_daily, info = info)
# 
# 
# 
# 
# mod <- cmdstanr::cmdstan_model("./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan")
# 
# fit <- mod$sample(
#   data = HOPB_met_mod, 
#   seed = 123, 
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# fit1 <- stan(
#   file = "./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan",  # Stan program
#   data = HOPB_met_mod,    # named list of data
#   chains = 4,             # number of Markov chains
#   warmup = 500,          # number of warmup iterations per chain
#   iter = 1000,            # total number of iterations per chain
#   cores = 4,              # number of cores (could use one per chain)
#   refresh = 100             # no progress shown
# )
# 
# 
# # 
# debugonce(metab)
# mm_bin <- metab(bayes_specs, data=HOPB_met_mod)
# 
# saveRDS(mm_bin, "./ignore/metab-models/HOPB_test.rds")