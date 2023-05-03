library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
library(ggridges)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas

# get future data

neon_sizes_future = readRDS(file = "data/derived_data/fish_inverts_dw-allyears.rds") %>%
  filter(year > 2021) %>%
  filter(!is.na(log_om_s)) %>%
  filter(!is.na(log_gpp_s)) %>%
  filter(!is.na(mat_s)) %>%
  group_by(sample_id) %>% mutate(sample_int=cur_group_id())%>%
  group_by(year) %>% mutate(year_int = cur_group_id()) %>%
  group_by(site_id) %>% mutate(site_int=cur_group_id())

# get previous data (2016 to 2021)
dat_all = readRDS("data/derived_data/dat_all.rds")

mean_temp = mean(unique(dat_all$temp_mean))
sd_temp = sd(unique(dat_all$temp_mean))

# load models
fishinvertmod = readRDS("models/stan_gppxtempxom2023-04-27.rds")

# extract posteriors
posts_sample_lambdas = get_sample_lambdas(fishinvertmod, data = dat)

site_future_preds = as_draws_df(fishinvertmod) %>% 
  pivot_longer(contains('alpha_raw_site'), 
               names_to = "site_int", 
               values_to = "site_offset") %>% 
  mutate(site_int = parse_number(site_int)) %>% 
  left_join(dat_all %>% ungroup %>% distinct(site_int, 
                                             mat_s, log_gpp_s, site_id, temp_mean,
                                             gpp, log_gpp, log_om_s, log_om, mean_om)) %>%
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s + 
           sigma_site*site_offset + 
           rnorm(nrow(.), 0, sigma_sample) + rnorm(nrow(.), 0, sigma_year)) %>% 
  group_by(site_id) %>% 
  mutate(median = median(lambda))


# fit single future samples
fit_model = stan_model("models/stan_spectra_singlesample_future.stan")

sim_data = neon_sizes_future %>% 
  group_by(sample_int) %>% 
  group_split()

posts_single = list()

# fit
for (i in 1:length(sim_data)) {
  stan_dat <- list(x = sim_data[[i]]$dw,
                   N = nrow(sim_data[[i]]),
                   counts = sim_data[[i]]$no_m2,
                   xmax = sim_data[[i]]$xmax,
                   xmin = sim_data[[i]]$xmin)
  
  fit <- sampling(object = fit_model,
                  data = stan_dat,
                  iter = 1000,
                  chains = 2,
                  open_progress = F,
                  verbose = F)
  
  posts_single[[i]] = as_draws_df(fit) %>% 
    mutate(site_id = unique(sim_data[[i]]$site_id),
           year = unique(sim_data[[i]]$year),
           season = unique(sim_data[[i]]$season),
           sample_int = unique(sim_data[[i]]$sample_int),
           sample_size = nrow(sim_data[[i]]))
  
}

future_lambdas = bind_rows(posts_single)

saveRDS(future_lambdas, file = "models/posteriors/future_lambdas.rds")

future_lambdas_summary = future_lambdas %>% 
  group_by(site_id, sample_int, season, sample_size) %>% 
  median_qi(lambda) %>% 
  left_join(site_future_preds %>% ungroup %>% distinct(site_id, median))

isd_future_predictions = site_future_preds %>% 
  ggplot(aes(x = lambda, y = reorder(site_id, median))) + 
  geom_density_ridges(scale = 0.85, quantile_lines = T,
                      quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975)) +
  # stat_dist_halfeyh() + 
  geom_pointrange(aes(x = lambda, xmin = .lower, xmax = .upper), 
                  data = future_lambdas_summary,
                  shape = 21) + 
  # scale_size(range=c(0.05, 0.8)) + 
  theme_default() + 
  labs(y = "NEON Stream Site",
       x = "\u03BB") + 
  coord_cartesian(xlim = c(-1.4, NA)) + 
  theme(text = element_text(size = 11))

ggview::ggview(isd_future_predictions, width = 4, height = 5)
saveRDS(isd_future_predictions, file = "plots/isd_future_predictions.rds")  
ggsave(isd_future_predictions, width = 4, height = 5,
       file = "plots/isd_future_predictions.jpg", dpi = 500)












