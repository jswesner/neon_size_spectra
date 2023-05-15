library(rstan)
library(tidyverse)

rstan_options(autowrite = TRUE)
rstan_options(threads_per_chain = 1)

# load data
neon_sizes_2016_2021 = readRDS(file = "data/derived_data/fish_inverts_dw-allyears.rds") %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  filter(!is.na(log_om_s)) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  filter(!is.na(mat_s)) %>% 
  group_by(sample_id) %>% mutate(sample_int=cur_group_id())%>% 
  group_by(year) %>% mutate(year_int = cur_group_id()) %>% 
  group_by(site_id) %>% mutate(site_int=cur_group_id())

# compile model
stan_spectra_mod_om = stan_model("models/stan_spectra_mod_om.stan")
stan_spectra_mod_gpp = stan_model("models/stan_spectra_mod_gpp.stan")
stan_spectra_mod_temp = stan_model("models/stan_spectra_mod_temp.stan")


# make data and fit model ---------------------------------------------------------

dat = neon_sizes_2016_2021

stan_data_interaction = list(N = nrow(dat),
                             mat_s = dat$mat_s,
                             gpp_s = dat$log_gpp_s,
                             om_s = dat$log_om_s,
                             year = dat$year_int,
                             site = dat$site_int,
                             sample = dat$sample_int,
                             n_years = length(unique(dat$year_int)),
                             n_sites = length(unique(dat$site_int)),
                             n_samples = length(unique(dat$sample_int)),
                             counts = dat$no_m2,
                             x = dat$dw,
                             xmin = dat$xmin,
                             xmax = dat$xmax)

fit_temp = sampling(object = stan_spectra_mod_temp,
                           data = stan_data_interaction,
                           iter = 2000, chains = 2, cores = 4)

saveRDS(fit_temp, file = paste0("models/stan_temp",Sys.Date(),".rds"))

fit_om = sampling(object = stan_spectra_mod_om,
                    data = stan_data_interaction,
                    iter = 2000, chains = 2, cores = 4)

saveRDS(fit_om, file = paste0("models/stan_om",Sys.Date(),".rds"))

fit_gpp = sampling(object = stan_spectra_mod_gpp,
                    data = stan_data_interaction,
                    iter = 2000, chains = 2, cores = 4)

saveRDS(fit_gpp, file = paste0("models/stan_gpp",Sys.Date(),".rds"))










