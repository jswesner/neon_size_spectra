library(rstan)
library(tidyverse)
library(janitor)
library(tidybayes)
library(brms)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas

# 1) load models
fishinvertmod = readRDS("models/stan_gppxtempxom2023-04-08.rds")

# 2) data
dat = readRDS(file = "data/derived_data/fish_inverts_dw-allyears.rds") %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  filter(!is.na(log_om_s)) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  filter(!is.na(mat_s)) %>% 
  group_by(sample_id) %>% mutate(sample_int=cur_group_id())%>% 
  group_by(year) %>% mutate(year_int = cur_group_id()) %>% 
  group_by(site_id) %>% mutate(site_int=cur_group_id())

# 3) extract posteriors (decide which level to summarize (i.e., intercept, sites, sample_ids, etc.))
# posts = as_draws_df(fishinvertmod) %>% 
  # select(a, .draw) %>% 
  # rename(lambda = a)

# posts_sample_lambdas, file = "data/derived_data/posts_sample_lambdas.rds"


# 4) merge with raw data
posts_raw = posts_sample_lambdas %>% 
  filter(.draw <= 10) %>% 
  select(lambda, sample_int, .draw) %>% 
  right_join(dat %>% select(sample_int, dw, xmin, xmax, no_m2), multiple = "all")

# 5) sample posterior preds
posts_raw_preds = posts_raw %>% 
  mutate(u = runif(nrow(.), 0, 1)) %>% # uniform draw
  mutate(x = (u*xmax^(lambda+1) +  (1-u) * xmin^(lambda+1) ) ^ (1/(lambda+1))) %>% 
  mutate(data = "y_rep") %>% 
  bind_rows(dat %>% 
              mutate(data = "y_raw") %>% 
              mutate(.draw = 0,
                     x = dw)) %>% 
  rename(sim = x)

# 6) simulate y and y_new
sim_posts = posts_raw_preds %>% 
  group_by(.draw, sample_int) %>% 
  sample_n(10000, weight = no_m2, replace = T) %>% 
  select(sim, .draw, data, site_id, year, sample_int, xmin, xmax, no_m2)

# 7) pick a sample to plot
id = as.integer(runif(5, 1, length(unique(dat$sample_int))))

# 8 Make plots
# violin
sim_posts %>%
  filter(sample_int == id) %>%
  ggplot(aes(x = .draw, y = sim*no_m2, color = data, group = .draw)) + 
  geom_violin() +
  scale_y_log10() +
  facet_wrap(sample_int ~ site_id) +
  NULL

# density
# violin
sim_posts %>%
  filter(sample_int == id) %>% 
  ggplot(aes(x = sim*no_m2, color = data, group = .draw)) + 
  geom_density() +
  scale_x_log10() +
  facet_wrap(sample_int ~ site_id) +
  NULL

