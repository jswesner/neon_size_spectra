library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas


# isd -----------------------------------------------------------
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
# dat = neon_sizes_2016_2021 %>% mutate(temp_mean = mean, 
#                                       temp_sd = sd)
# 
# saveRDS(dat, file = "data/derived_data/dat_all.rds")
dat_all = readRDS("data/derived_data/dat_all.rds")

mean_temp = mean(unique(dat_all$temp_mean))
sd_temp = sd(unique(dat_all$temp_mean))

# fancy facets
facet_gpp = readRDS(file = "plots/facet_gpp.rds")
facet_om = readRDS(file = "plots/facet_om.rds")

# load models
fishinvertmod = readRDS("models/stan_gppxtempxom2023-04-27.rds")

# extract posteriors
posts_sample_lambdas = get_sample_lambdas(fishinvertmod, data = dat_all)

qlog_om_s = quantile(unique(dat_all$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(dat_all$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))


post_lines_heat = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  filter(.draw <= 200) %>% 
  expand_grid(mat_s = seq(min(dat_all$mat_s), max(dat_all$mat_s), length.out = 30)) %>% 
  expand_grid(log_gpp_s = seq(min(dat_all$log_gpp_s), max(dat_all$log_gpp_s), length.out = 30)) %>%
  # expand_grid(log_om_s = seq(min(dat_all$log_om_s), max(dat_all$log_om_s), length.out = 30)) %>%
  expand_grid(qlog_om_s) %>%
  # expand_grid(qlog_gpp_s) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s, log_om_s, quantile_om) %>% 
  median_qi(lambda) %>%
  mutate(temp_mean = (mat_s*sd_temp) + mean_temp)  %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>%
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM"),
         log_gpp = (log_gpp_s*sd(unique(dat_all$log_gpp))) + mean(unique(dat_all$log_gpp))) %>% 
  # left_join(facet_gpp) %>% 
  left_join(facet_om)
  # mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  # mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP"))

(isd_heat_plot = post_lines_heat %>% 
  ggplot(aes(x = temp_mean, y = log_gpp)) +
  geom_tile(aes(fill = lambda)) +
  facet_wrap(~facet_om, labeller = "label_parsed") +
  scale_fill_viridis_c(option = 'C', direction = -1, na.value="white",
                       limits = c(-1.5, -1.0),
                       breaks = c(-1.5, -1.25, -1)) +
  geom_point(data = dat_all %>% ungroup %>% distinct(mat_s, log_gpp) %>% 
               mutate(temp_mean = (mat_s*sd_water) + mean_water), col = 'green', alpha = 0.6, size = 2) +
  theme_default() +
  labs(fill = "\u03bb",
       x = "Mean Annual Temperature (\u00b0C)",
       y = expression(paste("GPP ln(",gC/m ^ 2/yr,")")),
       subtitle = "a) ISD")+
    theme(legend.key.height= unit(0.4, 'cm'),
          legend.key.width= unit(0.4, 'cm')))

# community mass ----------------------------------------------------------
d = readRDS("data/derived_data/fish_inverts_dw-allyears.rds") %>% 
  filter(year >= 2016 & year <= 2021)

water = d %>% ungroup() %>% distinct(site_id, mean)

mean_water = mean(water$mean)
sd_water = sd(water$mean)

community_mass = d %>% 
  group_by(sample_id, site_id, log_gpp_s, log_om_s, mat_s, year, season, mean) %>% 
  mutate(dw_m2 = dw*no_m2) %>% 
  summarize(total_g_dwm2 = sum(dw_m2)/1000) %>% 
  ungroup() %>% 
  mutate(total_g_dwm2_s = total_g_dwm2/mean(total_g_dwm2),
         log_total_g = log(total_g_dwm2),
         log_total_g_s = log_total_g/mean(log_total_g)) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water)

community_mass_brm = readRDS("models/community_mass_brm.rds")

qlog_om_s = quantile(unique(d$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(d$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))

qmat_s = quantile(unique(d$mat_s), probs = c(0.25, 0.5, 0.75), na.rm = T)

posts_mass = tibble(mat_s = seq(min(dat_all$mat_s), max(dat_all$mat_s), length.out = 30)) %>% 
  expand_grid(log_gpp_s = seq(min(dat_all$log_gpp_s), max(dat_all$log_gpp_s), length.out = 30)) %>%
  # expand_grid(log_om_s = seq(min(dat_all$log_om_s), max(dat_all$log_om_s), length.out = 30)) %>%
  expand_grid(qlog_om_s) %>%
  # expand_grid(qlog_gpp_s) %>%  
  add_epred_draws(community_mass_brm, re_formula = NA) %>% 
  mutate(mean = (mat_s*sd_water) + mean_water) %>% 
  group_by(mat_s, mean, log_gpp_s, quantile_om) %>% 
  median_qi(.epred) %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>%
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM"),
         log_gpp = (log_gpp_s*sd(unique(dat_all$log_gpp))) + mean(unique(dat_all$log_gpp)))

(
mass_heat_plot = posts_mass %>% 
  ggplot(aes(x = mean, y = log_gpp_s)) +
  geom_tile(aes(fill = .epred)) +
  facet_wrap(~quantile_om) +
  scale_fill_viridis_c(option = 'C', direction = -1, na.value="white",
                       breaks = c(0.5, 1.5, 2.5)) +
  geom_point(data = d %>% ungroup %>% distinct(mat_s, log_gpp_s) %>% 
               mutate(mean = (mat_s*sd_water) + mean_water), 
             col = 'green', alpha = 0.6, size = 2) +
  theme_default() +
  labs(fill = bquote('ln('*gDM/m^'2'*")"),
       x = "Mean Annual Temperature (\u00b0C)",
       y = expression(paste("GPP ln(",gC/m ^ 2/yr,")")),
       subtitle = "b) Standing Stock Biomass") +
  theme(legend.key.height= unit(0.4, 'cm'),
        legend.key.width= unit(0.4, 'cm'))
)



library(patchwork)

a_heat = isd_heat_plot + theme(axis.title.x = element_blank(),
                      legend.text = element_text(size = 9))

b_heat = mass_heat_plot + theme(strip.text = element_blank(),
                                legend.text = element_text(size = 9))

heat_map_plot = a_heat/b_heat

ggview::ggview(heat_map_plot, width = 6.5, height = 4.5, units = "in")
ggsave(heat_map_plot, file = "plots/heat_map_plot.jpg",width = 6.5, height = 4.5, units = "in",
       dpi = 500)
saveRDS(heat_map_plot, file = "plots/heat_map_plot.rds")
saveRDS(a_heat, file = "plots/a_heat.rds")
saveRDS(b_heat, file = "plots/b_heat.rds")
