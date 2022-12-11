source("./code/resources/01_load-packages.R")
OKSR_met_full = get_site_data(siteCode = "OKSR") %>%
  dplyr::select(-DO.pctsat)

discharge.daily = OKSR_met_full %>%
  dplyr::mutate(date = as.Date(solar.time)) %>%
  group_by(date) %>%
  dplyr::summarise(discharge = mean(discharge, na.rm = TRUE))

## 
mle_specs <- specs(mm_name(type = "mle"))
# debugonce(metab_mle)
mm1 <- metab_mle(specs(mm_name(type = "mle")), data = OKSR_met_full)

k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
  select(date, K600.daily, K600.daily.lower, K600.daily.upper) %>%
  left_join(OKSR_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

mm1_vis = get_params(mm1, uncertainty = 'ci') %>%
  select(date, K600.daily, K600.daily.lower, K600.daily.upper)%>%
  left_join(OKSR_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

mm1_vis %>%
  ggplot()+
  geom_point(aes(x = discharge.daily, y = K600.daily)) +
  scale_y_log10()+
  scale_x_log10()
# 
mm2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'),
                          day_start = -1, day_end = 23), data_daily = k600_mm1)
# 
k600_mm2 <- get_params(mm2) %>% select(date, K600.daily) %>%  
  left_join(OKSR_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))

k600_mm2 %>%
  ggplot()+
  geom_point(aes(x = discharge.daily, y = K600.daily)) +
  coord_trans('log10')
# 
mm3 <- metab_mle(mle_specs, data = OKSR_met_full, data_daily = k600_mm2 %>% dplyr::select(-discharge.daily))

# model assessment
mods = data.frame(model = c("mm1","mm3"),
                  RSME = c(calc_mod_RSME(plot_DO_preds(mm1)),
                           calc_mod_RSME(plot_DO_preds(mm3))))
topMod = mods %>% slice_min(RSME) %>% select(model) %>% unlist %>% as.character

knitr::kable(mods)

saveRDS(eval(as.name(topMod)), "./ignore/metab-models/OKSR_full_mle.rds")


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
# metab_fun(specs = bayes_specs, data = OKSR_met_mod, data_daily = data_daily, info = info)
# 
# 
# 
# 
# mod <- cmdstanr::cmdstan_model("./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan")
# 
# fit <- mod$sample(
#   data = OKSR_met_mod, 
#   seed = 123, 
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# fit1 <- stan(
#   file = "./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan",  # Stan program
#   data = OKSR_met_mod,    # named list of data
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
# mm_bin <- metab(bayes_specs, data=OKSR_met_mod)
# 
# saveRDS(mm_bin, "./ignore/metab-models/OKSR_test.rds")