source("./code/resources/01_load-packages.R")
ARIK_met_full = get_site_data(siteCode = "ARIK") %>%
  dplyr::select(-DO.pctsat)

discharge.daily = ARIK_met_full %>%
  dplyr::mutate(date = as.Date(solar.time)) %>%
  group_by(date) %>%
  dplyr::summarise(discharge = mean(discharge, na.rm = TRUE))

# ARIK_met_mod = ARIK_met_full %>%
  # dplyr::select(-DO.pctsat) %>%
  # dplyr::filter(between(as.Date(solar.time), as.Date("2016-08-13"),as.Date("2016-10-13")))

## 
mle_specs <- specs(mm_name(type = "mle"))
# debugonce(metab_mle)
mm1 <- metab_mle(specs(mm_name(type = "mle")), data = ARIK_met_full)
k600_mm1 <- get_params(mm1, uncertainty = 'ci') %>%
   select(date, K600.daily, K600.daily.lower, K600.daily.upper) %>%
  left_join(ARIK_met_full %>%
              dplyr::mutate(date = as.Date(solar.time)) %>%
              group_by(date) %>%
              dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
# 
 mm2 <- metab_Kmodel(specs(mm_name('Kmodel', engine = 'loess'),
                           day_start = -1, day_end = 23), data_daily = k600_mm1)
# 
 k600_mm2 <- get_params(mm2) %>% select(date, K600.daily) %>%  
   left_join(ARIK_met_full %>%
               dplyr::mutate(date = as.Date(solar.time)) %>%
               group_by(date) %>%
               dplyr::summarise(discharge.daily = mean(discharge, na.rm = TRUE)))
 
 k600_mm2 %>%
   ggplot()+
   geom_point(aes(x = discharge.daily, y = K600.daily))
# 
 mm3 <- metab_mle(mle_specs, data = ARIK_met_full, data_daily = k600_mm2 %>% dplyr::select(-discharge.daily))
 x =get_params(mm3, fixed ='stars')
 x_mod = x %>% dplyr::mutate(K600.daily = gsub("NA","", K600.daily)) %>%
   dplyr::mutate(across(c(GPP.daily, ER.daily, K600.daily), ~as.numeric(.x)))
 x_mod %>% ggplot() + geom_point(aes(x = ER.daily, y = K600.daily)) +coord_cartesian(xlim = c(-100,0.1), ylim = c(0,30))
 y = get_param(mm1, fixed ='stars')
 predict_metab(mm3)
 
 plot_DO_preds(mm1)
 plot_DO_preds(mm3)
# 
# 
# mm = metab(mle_specs, data = ARIK_met_mod)
# debugonce(predict_DO);predict_DO(mm)

saveRDS(mm1, "./ignore/metab-models/ARIK_full_mle.rds")


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
# metab_fun(specs = bayes_specs, data = ARIK_met_mod, data_daily = data_daily, info = info)
# 
# 
# 
# 
# mod <- cmdstanr::cmdstan_model("./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan")
# 
# fit <- mod$sample(
#   data = ARIK_met_mod, 
#   seed = 123, 
#   chains = 4, 
#   parallel_chains = 4,
#   refresh = 500 # print update every 500 iters
# )
# 
# fit1 <- stan(
#   file = "./ignore/metab-models/b_Kb_oipp_tr_plrckm.stan",  # Stan program
#   data = ARIK_met_mod,    # named list of data
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
# mm_bin <- metab(bayes_specs, data=ARIK_met_mod)
# 
# saveRDS(mm_bin, "./ignore/metab-models/ARIK_test.rds")