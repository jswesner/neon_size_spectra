library(rstan)
library(tidyverse)

rstan_options(autowrite = TRUE)
rstan_options(threads_per_chain = 1)

# load data
neon_sizes_2016_2021 = readRDS(file = "data/derived_data/fish_inverts_dw-allyears.rds") %>% 
  filter(year >= 2016 & year <= 2021)

# neon_sizes_2016_2021 = readRDS(file = "data/derived_data/fish_dw-wrangled.rds") %>% 
#   filter(year >= 2016 & year <= 2021)
# 
# neon_sizes_2016_2021 = readRDS(file = "data/derived_data/macro_dw-wrangled.rds") %>% 
#   filter(year >= 2016 & year <= 2021)

# compile model
stan_spectra_single_models = stan_model("models/stan_spectra_singlesample.stan")


# make data and fit model ---------------------------------------------------------

dat_group = neon_sizes_2016_2021 %>% 
  filter(sample_id == 5)

stan_data = list(N = nrow(dat_group),
                             mat_s = dat_group$mat_s,
                             gpp_s = dat_group$log_gpp_s,
                             year = dat_group$year_int,
                             site = dat_group$site_int,
                             sample = dat_group$sample_int,
                             n_years = length(unique(dat_group$year_int)),
                             n_sites = length(unique(dat_group$site_int)),
                             n_samples = length(unique(dat_group$sample_int)),
                             counts = dat_group$no_m2,
                             x = dat_group$dw,
                             xmin = dat_group$xmin,
                             xmax = dat_group$xmax)

fit <- sampling(object = stan_spectra_single_models,
                data = stan_data,
                iter = 1000,
                chains = 2,
                open_progress = F,
                verbose = F)


fit
