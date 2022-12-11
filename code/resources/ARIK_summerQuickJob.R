source("./code/resources/01_load-packages.R")

ARIK_test = readRDS("./data/derived_data/clean-met-files/ARIK_met.rds") %>%
  dplyr::filter(between(as.Date(solar.time), as.Date("2020-01-02"), as.Date("2020-01-07"))) %>%
  dplyr::select(-c(DO.pctsat, outQF, probQF, doDiff, doSatDiff)) 

# plot_site('ARIK')
## run a quick version of the 
MLE_name <- mm_name(type='mle')
mle_specs <- specs(MLE_name)
mm_bin <- metab(mle_specs, data=ARIK_test)
plot_DO_preds(mm_bin)

MLE_name <- mm_name(type='bayes', pool_K600='binned', err_obs_iid=TRUE,
                      err_proc_iid=TRUE, err_proc_GPP = TRUE, ode_method = "trapezoid")
bayes_specs <- specs(bayes_name)

# stan_file = paste0("C:/Users/jrjunker/AppData/Local/R/win-library/4.2/streamMetabolizer/models/",bayes_name)
# ARIK_list = as.list(ARIK_test)
# ARIK_fit = rstan::stan(file = stan_file,
#             data = ARIK_list)

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
bayes_specs <- revise(bayes_specs, day_start = 0, day_end = 24, burnin_steps= 500, saved_steps= 250, thin_steps = 10, n_cores=6, n_chains = 3, GPP_daily_mu = 3, GPP_daily_sigma=2, verbose = TRUE)

# tictoc::tic();
# debugonce(metab)
mm_bin <- metab(bayes_specs, data=ARIK_test)#;tictoc::toc()
# saveRDS(mm_bin, )