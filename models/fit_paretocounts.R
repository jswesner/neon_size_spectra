source("models/paretocounts.R")
library(tidyverse)

dat_all = readRDS("data/derived_data/dat_all.rds") %>% 
  mutate(year = as.integer(year)) %>% 
  ungroup

# get raw means and sd to backtransform later
mean_sd = dat_all %>% distinct(site_id, log_om, log_gpp, temp_mean) %>% 
  pivot_longer(cols = -site_id) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(name = case_when(name == "log_gpp" ~ "log_gpp_s",
                          name == "log_om" ~ "log_om_s",
                          TRUE ~ "mat_s"))


# fit model and save
fit_pareto = brm(dw | vreal(no_m2, xmin, xmax) ~ mat_s*log_om_s*log_gpp_s + (1|year) + (1|site_id) +
             (1|sample_id), 
           data = dat_all,
           stanvars = stanvars,
           family = paretocounts(),
           chains = 3, iter = 2000,
           prior = c(prior(normal(-1.3, 0.1), class = "Intercept"),
                     prior(normal(0, 0.5), class = "b"),
                     prior(exponential(2), class = "sd")),
           file = "models/fit_pareto.rds")

