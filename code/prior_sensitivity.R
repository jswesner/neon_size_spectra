library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
library(posterior)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas

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

# load models
temp_mod = readRDS("models/stan_temp2023-05-14.rds")
temp_mod_wide = readRDS("models/stan_temp_wide2023-05-15.rds")

coefs_regular = summarise_draws(temp_mod) %>% mutate(model = "regular priors")
coefs_wide = summarise_draws(temp_mod_wide) %>% mutate(model = "wide priors")

bind_rows(coefs_regular, coefs_wide) %>% 
  filter(variable != "lp__") %>% 
  filter(!grepl("alpha", variable)) %>% 
  ggplot(aes(x = reorder(variable, median), y = median, ymin = q5, ymax = q95)) +
  geom_pointrange(aes(color = model),
                  position = position_dodge(width = 0.2),
                  size = 0.2) + 
  # facet_wrap(~model) + 
  coord_flip() +
  NULL

bind_rows(coefs_regular, coefs_wide) %>% 
  filter(variable != "lp__") %>% 
  filter(!grepl("alpha", variable))
