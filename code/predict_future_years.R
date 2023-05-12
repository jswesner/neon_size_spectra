library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
library(ggridges)
library(patchwork)
library(rstan)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas


# isd predictions ---------------------------------------------------------

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
posts_sample_lambdas = get_sample_lambdas(fishinvertmod, data = dat_all)

posts_sample_lambdas_years = get_sample_lambdas(fishinvertmod, data = dat_all,
                                                grp1_contains = "site",
                                                grp2_contains = "year") 

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
  mutate(median = median(lambda),
         year = 2022)

site_future_pred_summary = site_future_preds %>% 
  group_by(site_id) %>% 
  median_qi(lambda)

posts_sample_lambdas_years = get_sample_lambdas(fishinvertmod, data = dat_all,
                                                grp1_contains = "site",
                                                grp2_contains = "year") %>% 
  left_join(site_future_preds %>% distinct(site_id, median)) 

# fit single future samples
# fit_model = stan_model("models/stan_spectra_singlesample_future.stan")
# 
# sim_data = neon_sizes_future %>% 
#   group_by(sample_int) %>% 
#   group_split()
# 
# posts_single = list()
# 
# # fit
# for (i in 1:length(sim_data)) {
#   stan_dat <- list(x = sim_data[[i]]$dw,
#                    N = nrow(sim_data[[i]]),
#                    counts = sim_data[[i]]$no_m2,
#                    xmax = sim_data[[i]]$xmax,
#                    xmin = sim_data[[i]]$xmin)
#   
#   fit <- sampling(object = fit_model,
#                   data = stan_dat,
#                   iter = 1000,
#                   chains = 2,
#                   open_progress = F,
#                   verbose = F)
#   
#   posts_single[[i]] = as_draws_df(fit) %>% 
#     mutate(site_id = unique(sim_data[[i]]$site_id),
#            year = unique(sim_data[[i]]$year),
#            season = unique(sim_data[[i]]$season),
#            sample_int = unique(sim_data[[i]]$sample_int),
#            sample_size = nrow(sim_data[[i]]))
#   
# }
# 
# future_lambdas = bind_rows(posts_single)
# 
# saveRDS(future_lambdas, file = "models/posteriors/future_lambdas.rds")

future_lambdas = readRDS(file = "models/posteriors/future_lambdas.rds")

future_lambdas_summary = future_lambdas %>% 
  group_by(site_id, sample_int, season, sample_size) %>% 
  # median_qi(lambda) %>% 
  left_join(site_future_preds %>% ungroup %>% distinct(site_id, median)) %>% 
  group_by(site_id) %>% 
  left_join(site_future_pred_summary %>% distinct(site_id, .lower, .upper)) %>% 
  mutate(median2022 = median(lambda)) %>% 
  mutate(within = case_when(median2022 <= .upper | median2022 >= .lower ~ "yes", TRUE ~ "no"))


isd_future_predictions = posts_sample_lambdas_years %>% 
  bind_rows(site_future_preds) %>%
  mutate(year = as.factor(year)) %>% 
  mutate(yearfill = case_when(year == "2022" ~ "2022 Prediction", TRUE ~ "2016 through 2021")) %>% 
  ggplot(aes(x = lambda, y = reorder(site_id, median))) +
  geom_density_ridges(data = . %>% filter(yearfill == "2016 through 2021"),
                      aes(group = interaction(year, site_id)),
                      fill = NA,
                      color = "grey50", 
                      scale = 1) +
  geom_density_ridges(data = . %>% filter(yearfill != "2016 through 2021"),
                      aes(group = interaction(year, site_id)), 
                      scale = .6, quantile_lines = T,
                      quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975)) + 
  stat_pointinterval(aes(x = lambda, color = within), 
                     data = future_lambdas_summary,
                     size = 0.8) + 
  theme_default() + 
  scale_color_colorblind() +
  labs(y = "NEON Stream Site",
       x = "\u03BB") + 
  coord_cartesian(xlim = c(-1.6, NA)) + 
  theme(text = element_text(size = 11),
        legend.title = element_blank()) +
  guides(color = "none")

# isd_future_predictions = site_future_preds %>% 
#   ggplot(aes(x = lambda, y = reorder(site_id, median))) + 
#   geom_density_ridges(scale = 0.85, quantile_lines = T,
#                       quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975)) +
#   # stat_dist_halfeyh() + 
#   geom_pointrange(aes(x = lambda, xmin = .lower, xmax = .upper), 
#                   data = future_lambdas_summary,
#                   shape = 21) + 
#   # scale_size(range=c(0.05, 0.8)) + 
#   theme_default() + 
#   labs(y = "NEON Stream Site",
#        x = "\u03BB") + 
#   coord_cartesian(xlim = c(-1.4, NA)) + 
#   theme(text = element_text(size = 11))

ggview::ggview(isd_future_predictions, width = 4, height = 5)
saveRDS(isd_future_predictions, file = "plots/isd_future_predictions.rds")  
ggsave(isd_future_predictions, width = 4, height = 5,
       file = "plots/isd_future_predictions.jpg", dpi = 500)



# biomass predictions -----------------------------------------------------

community_mass_brm = readRDS("models/community_mass_brm.rds")

d = readRDS("data/derived_data/fish_inverts_dw-allyears.rds") %>% 
  filter(year >= 2016 & year <= 2021) %>% 
  group_by(sample_id, site_id, log_gpp_s, log_om_s, mat_s, year, season, mean) %>% 
  mutate(dw_m2 = dw*no_m2) %>% 
  summarize(total_g_dwm2 = sum(dw_m2)/1000) %>% 
  ungroup() %>% 
  mutate(total_g_dwm2_s = total_g_dwm2/mean(total_g_dwm2),
         log_total_g = log(total_g_dwm2),
         log_total_g_s = log_total_g/mean(log_total_g)) 


biomass_preds = as_draws_df(community_mass_brm) %>% 
  ungroup() %>% 
  select(starts_with("b_"), starts_with("sd_")) %>% 
  clean_names %>%
  expand_grid(d %>% ungroup %>% distinct(log_om_s, log_gpp_s, mat_s, site_id) %>% filter(!is.na(log_om_s))) %>% 
  glimpse() %>% 
  mutate(log_total_g_s = b_intercept + b_log_gpp_s*log_gpp_s + b_log_om_s*log_om_s + b_mat_s*mat_s +
           b_log_gpp_s_log_om_s*log_gpp_s*log_om_s + b_log_gpp_s_mat_s*log_gpp_s*mat_s + b_log_om_s_mat_s*log_om_s*mat_s +
           b_log_gpp_s_log_om_s_mat_s*log_om_s*log_gpp_s*mat_s + 
           rnorm(nrow(.), 0, sd_year_intercept) + rnorm(nrow(.), 0, sd_season_intercept)) %>% 
  mutate(log_total_g = log_total_g_s*mean(d$log_total_g)) %>% 
  left_join(site_future_preds %>% distinct(site_id, median)) %>% 
  mutate(year = 2022)

biomass_preds_summary = biomass_preds %>% 
  group_by(site_id) %>% 
  median_qi(log_total_g) %>% 
  select(site_id, .lower, .upper, log_total_g) %>% 
  rename(median2022 = log_total_g)

community_mass_future = readRDS("data/derived_data/fish_inverts_dw-allyears.rds") %>% 
  filter(year > 2021) %>% 
  group_by(sample_id, site_id, log_gpp_s, log_om_s, mat_s, year, season, mean) %>% 
  mutate(dw_m2 = dw*no_m2) %>% 
  summarize(total_g_dwm2 = sum(dw_m2)/1000) %>% 
  ungroup() %>% 
  mutate(total_g_dwm2_s = total_g_dwm2/mean(total_g_dwm2),
         log_total_g = log(total_g_dwm2),
         log_total_g_s = log_total_g/mean(log_total_g)) %>% 
  # mutate(mat = (mat_s*sd_water) + mean_water) %>% 
  left_join(site_future_preds %>% distinct(site_id, median)) %>% 
  left_join(biomass_preds_summary) %>% 
  mutate(within = case_when(log_total_g <= .upper | log_total_g >= .lower ~ "yes", TRUE ~ "no"))

biomass_preds_year = as_draws_df(community_mass_brm) %>% 
  ungroup() %>% 
  select(starts_with("b_"), starts_with("sd_"), starts_with("r_year")) %>% 
  clean_names %>%
  expand_grid(d %>% ungroup %>% distinct(log_om_s, log_gpp_s, mat_s, site_id) %>%
                filter(!is.na(log_om_s))) %>% 
  pivot_longer(cols = starts_with("r_year")) %>% 
  mutate(log_total_g_s = b_intercept + b_log_gpp_s*log_gpp_s + b_log_om_s*log_om_s + b_mat_s*mat_s +
           b_log_gpp_s_log_om_s*log_gpp_s*log_om_s + b_log_gpp_s_mat_s*log_gpp_s*mat_s + b_log_om_s_mat_s*log_om_s*mat_s +
           b_log_gpp_s_log_om_s_mat_s*log_om_s*log_gpp_s*mat_s + 
           # rnorm(nrow(.), 0, sd_year_intercept) +  
           rnorm(nrow(.), 0, sd_season_intercept) + 
           value) %>% 
  mutate(log_total_g = log_total_g_s*mean(d$log_total_g)) %>% 
  left_join(site_future_preds %>% distinct(site_id, median)) %>% 
  mutate(year = parse_number(name))

community_mass_future_preds = biomass_preds_year %>% 
  bind_rows(biomass_preds) %>%
  mutate(year = as.factor(year)) %>% 
  mutate(yearfill = case_when(year == "2022" ~ "2022 Prediction", TRUE ~ "2016 through 2021")) %>% 
  ggplot(aes(x = log_total_g, y = reorder(site_id, median))) +
  geom_density_ridges(data = . %>% filter(yearfill == "2016 through 2021"),
                      aes(group = interaction(year, site_id)),
                      fill = NA,
                      color = "grey50", 
                      scale = 1) +
  geom_density_ridges(data = . %>% filter(yearfill != "2016 through 2021"),
                      aes(group = interaction(year, site_id)), 
                      scale = .6, quantile_lines = T,
                      quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975)) + 
  stat_pointinterval(aes(x = log_total_g, color = within), 
                     data = community_mass_future,
                     size = 0.8) + 
  theme_default() + 
  scale_color_colorblind() +
  labs(y = "NEON Stream Site",
       x = "ln(Total Community Mass gDM)") + 
  coord_cartesian(xlim = c(-2, 5)) + 
  theme(text = element_text(size = 11),
        legend.title = element_blank()) +
  guides(color = "none")

# community_mass_future_preds = biomass_preds %>% 
#   ggplot(aes(x = log_total_g, y = reorder(site_id, median))) +
#   geom_density_ridges(scale = 0.85, quantile_lines = T,
#                       quantiles = c(0.025, 0.1, 0.5, 0.9, 0.975)) +
#   geom_point(aes(x = log_total_g), 
#              data = community_mass_future,
#              shape = 21) + 
#   theme_default() + 
#   labs(y = "NEON Stream Site",
#        x = "ln(Total Community Mass gDM)") + 
#   coord_cartesian(xlim = c(-2, 5)) +
#   theme(text = element_text(size = 11))

saveRDS(community_mass_future_preds, file = "plots/community_mass_future_preds.rds")  
ggsave(community_mass_future_preds, width = 4, height = 5,
       file = "plots/community_mass_future_preds.jpg", dpi = 500)



isd_mass_future_plot = (isd_future_predictions + labs(subtitle = "a) ISD")) + 
  (community_mass_future_preds + theme(axis.text.y = element_blank(),
                                       axis.title.y = element_blank()) + 
     labs(subtitle = "b) Total Community Mass"))

saveRDS(isd_mass_future_plot, file = "plots/ms_plots/isd_mass_future_plot.rds")  
ggsave(isd_mass_future_plot, width = 6.5, height = 5,
       file = "plots/ms_plots/isd_mass_future_plot.jpg", dpi = 500)



