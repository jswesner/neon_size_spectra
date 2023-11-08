library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
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
om_mod = readRDS("models/stan_om2023-05-13.rds")
temp_mod = readRDS("models/stan_temp2023-05-14.rds")
gpp_mod = readRDS("models/stan_gpp2023-05-14.rds")

# extract posteriors

post_lines_om = as_draws_df(om_mod) %>% as_tibble() %>% expand_grid(om = unique(dat_all$log_om_s)) %>% 
  mutate(lambda = a + beta_om*om,
         pred = om,
         model = "om")
post_lines_temp = as_draws_df(temp_mod) %>% as_tibble() %>% expand_grid(mat_s = unique(dat_all$mat_s)) %>% 
  mutate(lambda = a + beta_mat*mat_s,
         pred = mat_s,
         model = "temp")
post_lines_gpp = as_draws_df(gpp_mod) %>% as_tibble() %>% expand_grid(gpp_s = unique(dat_all$log_gpp_s)) %>% 
  mutate(lambda = a + beta_gpp*gpp_s,
         pred = gpp_s,
         model = "gpp")


bind_rows(post_lines_om, post_lines_temp, post_lines_gpp) %>% 
  group_by(model, pred) %>% 
  median_qi(lambda) %>% 
  ggplot(aes(x = pred, y = lambda)) + 
  facet_wrap(~model, scales = "free_x") +
  geom_line(aes(color = model)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = model), alpha = 0.4) + 
  ylim(-1.5, -1)




# samples -----------------------------------------------------------------

post_samples_om = get_sample_om_lambdas(om_mod, data = dat_all)
post_samples_temp = get_sample_mat_lambdas(temp_mod, data = dat_all)
post_samples_gpp = get_sample_gpp_lambdas(gpp_mod, data = dat_all)

bind_rows(post_samples_om,
          post_samples_temp,
          post_samples_gpp) %>% 
  group_by(model, pred, sample_int) %>% 
  median_qi(lambda) %>% 
  ggplot(aes(x = pred, y = lambda)) + 
  geom_pointrange(aes(ymin = .lower, ymax = .upper),
                  position = position_jitter(width = 0.1), 
                  size = 0.2) + 
  facet_wrap(~model, scales = "free_x")



bind_rows(post_samples_om,
          post_samples_temp,
          post_samples_gpp) %>% 
  group_by(model, pred, sample_int) %>% 
  median_qi(lambda) %>% 
  ggplot(aes(x = reorder(sample_int, lambda), y = lambda, ymin = .lower, ymax = .upper)) + 
  geom_pointrange(aes(color = model), position = position_dodge(width = 0.05),
                  shape = 21) + 
  coord_flip()


bind_rows(post_samples_om,
          post_samples_temp,
          post_samples_gpp) %>% 
  group_by(model, pred, sample_int, site_id) %>% 
  median_qi(lambda) %>% 
  ggplot(aes(x = reorder(site_id, lambda), y = lambda, ymin = .lower, ymax = .upper)) + 
  geom_pointrange(aes(color = model, group = interaction(model, sample_int)), position = position_dodge(width = 0.5),
                  shape = 21) + 
  coord_flip()

