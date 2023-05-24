library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas

# refit without the sites with very high GPP (e.g., SYCA at 12,000 and XXXX at 7000)
# Then compare with the main model that includes these sites to assess influence of large GPP values.

# get data
# neon_sizes_2016_2021 = readRDS(file = "data/derived_data/fish_inverts_dw-allyears.rds") %>%
#   filter(year >= 2016 & year <= 2021) %>%
#   filter(!is.na(log_om_s)) %>%
#   filter(!is.na(log_gpp_s)) %>%
#   filter(!is.na(mat_s)) %>%
#   group_by(sample_id) %>% mutate(sample_int=cur_group_id())%>%
#   group_by(year) %>% mutate(year_int = cur_group_id()) %>%
#   group_by(site_id) %>% mutate(site_int=cur_group_id())
# 
# dat_all = neon_sizes_2016_2021 %>% mutate(temp_mean = mean,
#                                       temp_sd = sd)
# 
# saveRDS(dat_all, file = "data/derived_data/dat_all.rds")
dat_all = readRDS("data/derived_data/dat_all.rds")

mean_temp = mean(unique(dat_all$temp_mean))
sd_temp = sd(unique(dat_all$temp_mean))

fishinvertmod = readRDS("models/stan_gppxtempxom2023-05-12.rds")
fishinvertmod_noout = readRDS("models/stan_gppxtempxom_nooutliers2023-05-16.rds")

coefs_regular = summarise_draws(fishinvermod) %>% mutate(model = "main model")
coefs_wide = summarise_draws(fishinvertmod_noout) %>% mutate(model = "exclude gpp outliers")
