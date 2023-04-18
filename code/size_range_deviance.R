
dat_invert = readRDS(file = "data/derived_data/macro_dw-wrangled.rds") %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  filter(!is.na(log_om_s)) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  filter(!is.na(mat_s)) %>% 
  group_by(sample_id) %>% mutate(sample_int=cur_group_id())%>% 
  group_by(year) %>% mutate(year_int = cur_group_id()) %>% 
  group_by(site_id) %>% mutate(site_int=cur_group_id()) %>% 
  mutate(temp_mean = mean, 
         temp_sd = sd)

dat_fish = readRDS(file = "data/derived_data/fish_dw-wrangled.rds") %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  filter(!is.na(log_om_s)) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  filter(!is.na(mat_s)) %>% 
  group_by(sample_id) %>% mutate(sample_int=cur_group_id())%>% 
  group_by(year) %>% mutate(year_int = cur_group_id()) %>% 
  group_by(site_id) %>% mutate(site_int=cur_group_id()) %>% 
  mutate(temp_mean = mean, 
         temp_sd = sd)


invert_range = dat_invert %>% 
  group_by(sample_int) %>% 
  add_tally() %>%
  group_by(sample_int) %>% 
  mutate(sample_xmin = min(dw),
         sample_xmax = max(dw),
         gm = exp(mean(log(dw)))) %>% 
  ungroup %>% 
  distinct(xmin, xmax, site_id, n, sample_xmin, sample_xmax, sample_int, gm) %>% 
  mutate(pwr = log(xmax/xmin),
         pwr_sample = log(sample_xmax/sample_xmin),
         range = xmax/xmin,
         animal_type = "inverts",
         order_min = floor(log10(xmin)),
         order_max = floor(log10(xmax)),
         order_range = order_max - order_min)

fish_range = dat_fish %>% 
  group_by(sample_int) %>% 
  add_tally() %>%
  group_by(sample_int) %>% 
  mutate(sample_xmin = min(dw),
         sample_xmax = max(dw),
         gm = exp(mean(log(dw)))) %>% 
  ungroup %>% 
  distinct(xmin, xmax, site_id, n, sample_xmin, sample_xmax, sample_int, gm) %>% 
  mutate(pwr = log(xmax/xmin),
         pwr_sample = log(sample_xmax/sample_xmin),
         range = xmax/xmin,
         animal_type = "fish",
         order_min = floor(log10(xmin)),
         order_max = floor(log10(xmax)),
         order_range = order_max - order_min)



all_range = dat %>% 
  group_by(sample_int) %>% 
  add_tally() %>%
  group_by(sample_int) %>% 
  mutate(sample_xmin = min(dw),
         sample_xmax = max(dw),
         gm = exp(mean(log(dw)))) %>% 
  ungroup %>% 
  distinct(xmin, xmax, site_id, n, sample_xmin, sample_xmax, sample_int, gm) %>% 
  mutate(pwr = log(xmax/xmin),
         pwr_sample = log(sample_xmax/sample_xmin),
         range = xmax/xmin,
         animal_type = "inverts + fish",
         order_min = floor(log10(xmin)),
         order_max = floor(log10(xmax)),
         order_range = order_max - order_min)


  

posts_sample_lambdas_fishinverts = readRDS(file = "data/derived_data/posts_sample_lambdas.rds")

posts_medians_fish = posts_sample_lambdas_fish %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)

preds_to_match_fish = as_draws_df(fishmod) %>% 
  expand_grid(distinct(posts_medians_fish, mat_s, log_gpp_s, log_om_s)) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s) %>% 
  median_qi(lambda) %>% 
  rename(lambda_predict = lambda,
         lower_predict = .lower,
         upper_predict = .upper) %>% 
  select(mat_s, contains("predict"))


posts_medians_inverts = posts_sample_lambdas_inverts %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)

preds_to_match_inverts = as_draws_df(invertmod) %>% 
  expand_grid(distinct(posts_medians_inverts, mat_s, log_gpp_s, log_om_s)) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s) %>% 
  median_qi(lambda) %>% 
  rename(lambda_predict = lambda,
         lower_predict = .lower,
         upper_predict = .upper) %>% 
  select(mat_s, contains("predict"))


posts_medians_all = posts_sample_lambdas %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)

preds_to_match_all = as_draws_df(fishinvertmod) %>% 
  expand_grid(distinct(posts_medians_all, mat_s, log_gpp_s, log_om_s)) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s) %>% 
  median_qi(lambda) %>% 
  rename(lambda_predict = lambda,
         lower_predict = .lower,
         upper_predict = .upper) %>% 
  select(mat_s, contains("predict"))

posts_medians_fish %>% 
  left_join(preds_to_match_fish) %>% 
  mutate(deviance = lambda_predict - lambda) %>% 
  left_join(fish_range) %>% 
  bind_rows(posts_medians_inverts %>% 
              left_join(preds_to_match_inverts) %>% 
              mutate(deviance = lambda_predict - lambda) %>% 
              left_join(invert_range),
            posts_medians_all %>% 
              left_join(preds_to_match_all) %>% 
              mutate(deviance = lambda_predict - lambda) %>% 
              left_join(all_range)) %>% 
  # filter(animal_type == "inverts + fish") %>%
  ggplot(aes(x = pwr, y = abs(deviance), color = animal_type)) +
  geom_point() + 
  # geom_smooth() +
  geom_hline(aes(yintercept = 0)) +
  scale_x_log10()



