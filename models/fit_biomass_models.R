library(brms)
library(tidyverse)
library(tidybayes)
library(here)
library(hydroTSM)
library(ggthemes)
library(scales)
library(janitor)
library(ggview)

# get data -------------------------
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
         mean_log_total_g = mean(log_total_g),
         log_total_g_s = log_total_g/mean(log_total_g)) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water)

# fish and inverts separate
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

dat_fishinvert = dat_invert %>% mutate(animal_type = "inverts") %>% 
  bind_rows(dat_fish %>% mutate(animal_type = "fish")) %>% 
  group_by(animal_type) %>% 
  mutate(mean_dw = mean(dw),
         log_dw = log(dw),
         log_dwm2 = log(dw*no_m2)) %>%
  mutate(mean_log_dw = mean(log_dw),
         log_dw_c = log_dw - mean(log_dw),
         dw_s = dw/mean(dw),
         mean_log_dwm2 = mean(log_dwm2),
         log_dwm2_c = log_dwm2 - mean_log_dwm2,
         temp_mean = (mat_s*sd_water) + mean_water)

saveRDS(dat_fishinvert, file = "data/derived_data/dat_fishinvert.rds")



# total biomass -----------------------------------------------------------
community_mass_brm = brm(log_total_g_s ~ log_gpp_s*log_om_s*mat_s + (1|year) + (1|season),
                         family = gaussian(),
                         data = community_mass,
                         prior = c(prior(normal(1, 2), class = "Intercept"),
                                   prior(normal(0, 1), class = "b"),
                                   prior(exponential(1), class = "sd")),
                         file = "models/community_mass_brm.rds",
                         file_refit = "on_change")

community_mass_brm = readRDS("models/community_mass_brm.rds")

coefs_mass = tidy_draws(community_mass_brm) %>% 
  select(starts_with("b_"), .draw) %>% 
  pivot_longer(cols = -.draw) %>% 
  filter(name != "b_Intercept") %>% 
  group_by(name) %>% 
  mutate(coefficient = str_sub(name, 3, 50),
         coefficient = paste0("\u03b2", coefficient),
         coefficient = str_replace(coefficient, "log_", ""),
         coefficient = str_replace(coefficient, "log_", ""),
         coefficient = str_replace(coefficient, "log_", ""),
         coefficient = str_replace(coefficient, "_s", ""),
         coefficient = str_replace(coefficient, "_s", ""),
         coefficient = str_replace(coefficient, "_s", ""),
         coefficient = str_replace(coefficient, "gpp:om:mat", "om:mat:gpp"),
         order = str_length(coefficient))


coefs_mass %>% 
  group_by(name) %>% 
  summarize(prob_pos = sum(value>0)/4000)

coefs_mass %>% 
  ggplot(aes(y = value, x = reorder(coefficient, -order))) + 
  stat_pointinterval() +
  coord_flip() +
  geom_hline(aes(yintercept = 0)) + 
  theme_default() +
  labs(x = "Model coefficients",
       y = "Value")

qlog_om_s = quantile(unique(d$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(d$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))

qmat_s = quantile(unique(d$mat_s), probs = c(0.25, 0.5, 0.75), na.rm = T)


posts = tibble(mat_s = unique(d$mat_s)) %>% 
  expand_grid(qlog_gpp_s) %>% 
  expand_grid(qlog_om_s) %>% 
  add_epred_draws(community_mass_brm, re_formula = NA) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water)

saveRDS(posts, file = "models/posteriors/posts_mass.rds")

community_mass_univariate_plot = posts %>% 
  filter(quantile_gpp == "Median GPP") %>% 
  filter(quantile_om == "Median OM") %>% 
  group_by(mat_s, log_om_s, log_gpp_s, mat) %>% 
  mutate(.epred = .epred*mean(community_mass$log_total_g)) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mat, y = .epred)) +
  geom_line() + 
  geom_point(data = community_mass, aes(y = log_total_g), size = 0.5) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.3) + 
  theme_default() + 
  labs(y = bquote('Standing Stock Biomass: ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  # scale_y_log10() +
  NULL

ggview::ggview(community_mass_univariate_plot, width = 5, height = 5, units = "in")
ggsave(community_mass_univariate_plot, width = 5, height = 5, units = "in",
       file = "plots/community_mass_univariate_plot.jpg", dpi = 500)
saveRDS(community_mass_univariate_plot, file = "plots/community_mass_univariate_plot.rds")


community_mass_plot = posts %>% 
  # filter(quantile_om == "Median OM") %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, mat) %>% 
  mutate(.epred = .epred*mean(community_mass$log_total_g)) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mat, y = .epred, fill = quantile_om)) +
  geom_line(aes(color = quantile_om))  +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  facet_grid(quantile_om~quantile_gpp) +
  # scale_y_log10() + 
  theme_default() + 
  labs(y = bquote('Standing Stock Biomass: ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  # scale_y_log10() +
  NULL


ggsave(community_mass_plot, file = "plots/community_mass_plot.jpg", width = 6, height = 6, units = "in")

community_mass_plot_byom = posts  %>%
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  filter(quantile_gpp == "Median GPP") %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, mat) %>% 
  mutate(.epred = .epred*mean(community_mass$log_total_g)) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mat, y = .epred)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  facet_grid(~quantile_om) +
  theme_default() +
  # scale_y_log10() + 
  labs(y = bquote('Standing Stock Biomass: ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  # scale_y_log10() +
  NULL

saveRDS(community_mass_plot_byom, file = "plots/community_mass_plot_byom.rds")
ggsave(community_mass_plot_byom, file = "plots/community_mass_plot_byom.jpg", width = 5.5, height = 2, units = "in")



# Polynomial biomass ------------------------------------------------------

community_mass_brm = readRDS("models/community_mass_brm.rds")
community_polynomial_brm = update(community_mass_brm,
                                  formula = . ~ log_gpp_s*log_om_s*mat_s +
                                    I(mat_s^2) + (1|year) + (1|season))

WAIC(community_mass_brm, community_polynomial_brm)



# vary time windows -------------------------------------------------------

temp_season_year = readRDS(file = "data/derived_data/raw_stream_temperatures_formodel.rds") %>%
  filter(surfWaterTempMean <= 33) %>% 
  mutate(season = case_when(
    month %in% c(1,2,3) ~ 'winter',
    month %in% c(4,5,6) ~ 'spring',
    month %in% c(7,8,9) ~ 'summer',
    month %in% c(10,11,12) ~ 'autumn' 
  )) %>% 
  group_by(season, siteID) %>% 
  summarize(mean_season_temp = mean(surfWaterTempMean)) %>% 
  clean_names()

community_mass %>% 
  mutate(season = case_when(season == "autumm" ~ "autumn",
                            TRUE ~ season)) %>% 
  left_join(temp_season_year) %>% 
  ggplot(aes(x = mean_season_temp, y = total_g_dwm2)) + 
  geom_point() + 
  scale_y_log10() +
  geom_smooth(method = "lm") +
  NULL



# fish and invert biomass ------------------------------------------------------------
fish_dw_wrangled = readRDS(file = "data/derived_data/fish_dw-wrangled.rds") %>% mutate(animal_type = "fish") %>% 
  filter(date >= as.Date("2016-10-15") & date <= as.Date("2021-06-15")) 

inverts_dw_wrangled = readRDS(file = "data/derived_data/macro_dw-wrangled.rds") %>% mutate(animal_type = "inverts") %>% 
  filter(date >= as.Date("2016-10-15") & date <= as.Date("2021-06-15")) 

gpp = readRDS("data/derived_data/gpp_means.rds") %>% clean_names() %>% 
  rename(gpp = mean, gpp_sd = sd) %>% 
  mutate(log_gpp = log(gpp),
         log_gpp_s = scale(log_gpp, center = T, scale = T),
         log_gpp_s = as.numeric(log_gpp_s),
         gpp_s = (gpp - mean(gpp))/sd(gpp)) 

mat = readRDS("data/derived_data/temperature_mean-annual.rds") %>% 
  mutate(mat_s = (mean - mean(mean))/sd(mean)) %>% clean_names()

cpom = readRDS(here("data/derived_data/cpom_means.rds")) %>% 
  mutate(om_s = (mean - mean(mean, na.rm = T))/sd(mean, na.rm = T)) %>% 
  clean_names() %>% 
  rename(mean_om = mean,
         sd_om = sd) %>% 
  mutate(log_om = log(mean_om),
         log_om_s = scale(log_om),
         log_om_s = as.numeric(om_s))

fish_inverts = bind_rows(fish_dw_wrangled, inverts_dw_wrangled) %>% 
  group_by(dw, site_id, sample_id, macro_julian, animal_type) %>%
  summarize(no_m2 = sum(no_m2)) %>%    # tally by size, regardless of taxa
  ungroup() %>% 
  mutate(xmin = min(dw)) %>% 
  group_by(site_id) %>% 
  mutate(xmax = max(dw)) %>% 
  ungroup %>% 
  left_join(gpp) %>% 
  left_join(mat) %>% 
  left_join(cpom) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  mutate(date = as_date(macro_julian),
         year = year(date),
         yday = yday(date),
         season = time2season(date, out.fmt = "seasons")) %>% 
  mutate(sample_int = as.integer(as.factor(sample_id)),
         year_int = as.integer(as.factor(year)),
         site_int = as.integer(as.factor(site_id)),
         season_int = as.integer(as.factor(season))) 


fish_invert_mass = fish_inverts %>% 
  group_by(sample_id, site_id, log_gpp_s, log_om_s, mat_s, year, season, animal_type, mean) %>% 
  mutate(dw_m2 = dw*no_m2,
         dw_m2 = case_when(is.na(dw_m2) ~ 0, TRUE ~ dw_m2)) %>%  # some sites don't have fish. this replaces NA's from that with true zeros 
  summarize(total_g_dwm2 = sum(dw_m2)/1000) %>% 
  ungroup() %>% 
  rename(mat = mean) %>% 
  mutate(total_g_dwm2_s = total_g_dwm2/mean(total_g_dwm2),
         log_total_g = log(total_g_dwm2),
         log_total_g_s = log_total_g/mean(log_total_g))


fish_invert_mass_brm = brm(log_total_g ~ log_gpp_s*log_om_s*mat_s*animal_type + (1|year) + (1|season),
                         family = gaussian(),
                         data = fish_invert_mass,
                         prior = c(prior(normal(1, 5), class = "Intercept"),
                                   prior(normal(0, 1), class = "b"),
                                   prior(exponential(1), class = "sd")),
                         file = "models/fish_invert_mass_brm.rds",
                         file_refit = "on_change")

qlog_om_s = quantile(unique(fish_invert_mass$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(fish_invert_mass$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))

qmat_s = quantile(unique(fish_invert_mass$mat_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "mat_s", names_to = "quantile_mat") %>% 
  mutate(quantile_mat = c("Low Temp", "Median Temp", "High Temp"))


posts_fishinvert = tibble(mat_s = unique(fish_invert_mass$mat_s)) %>% 
  expand_grid(qlog_gpp_s) %>% 
  expand_grid(qlog_om_s) %>% 
  expand_grid(animal_type = unique(fish_invert_mass$animal_type)) %>% 
  add_epred_draws(fish_invert_mass_brm, re_formula = NA) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water)


fish_invert_mass_plot = posts_fishinvert %>% 
  filter(quantile_gpp == "Median GPP") %>% 
  filter(quantile_om == "Median OM") %>% 
  group_by(mat_s, log_om_s, log_gpp_s, animal_type, mat) %>% 
  mutate(.epred = .epred) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = (mat_s*sd_water) + mean_water, y = .epred, fill = animal_type)) +
  geom_line() +
  geom_point(data = fish_invert_mass, aes(y = log_total_g, color = animal_type),
             size = 0.2) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) +
  # scale_y_log10() + 
  theme_default() +
  scale_color_colorblind(guide = guide_legend(reverse = T)) + 
  scale_fill_colorblind(guide = guide_legend(reverse = T)) +
  labs(y = bquote('Total Biomass: ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  theme(legend.title = element_blank()) +
  NULL

ggview(fish_invert_mass_plot, width =6,  height = 5, units = "in")
ggsave(fish_invert_mass_plot, file = "plots/fish_inver_mass_plot.jpg", width = 6, height = 5, units = "in", 
       dpi = 500)
saveRDS(fish_invert_mass_plot, file = "plots/fish_invet_mass_plot.rds")


fish_invert_mass_plot_panels = posts_fishinvert %>% 
  # filter(quantile_om == "Median OM") %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, animal_type) %>% 
  mutate(.epred = .epred) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = (mat_s*sd_water) + mean_water, y = .epred, fill = animal_type)) +
  geom_line()  +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  facet_grid(quantile_om~quantile_gpp) +
  # scale_y_log10() + 
  theme_default() +
  scale_color_colorblind(guide = guide_legend(reverse = T)) + 
  scale_fill_colorblind(guide = guide_legend(reverse = T))+
  labs(y = bquote('Total Biomass: ln('~g/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  theme(legend.title = element_blank()) +
  NULL

saveRDS(fish_invert_mass_plot_panels, file = "plots/fish_invert_mass_plot_panels.rds")
ggsave(fish_invert_mass_plot_panels, file = "plots/fish_invert_mass_plot_panels.jpg", width = 6, height = 5, units = "in")

# this back transforms the natural log .epred. Probably there is a better way
proportion_mass = posts_fishinvert %>% 
  # filter(quantile_om == "Median OM") %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  group_by(mat, log_gpp_s, log_om_s, .draw) %>% 
  mutate(total_mass = sum(exp(.epred)),
         proportion = exp(.epred)/total_mass)


proportion_mass %>% 
  # filter(animal_type == "fish") %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, animal_type) %>% 
  median_qi(proportion) %>% 
  ggplot(aes(x = (mat_s*sd_water) + mean_water, y = proportion, fill = animal_type)) +
  geom_line()  +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  facet_grid(quantile_om~quantile_gpp) +
  # scale_y_log10() + 
  theme_default() +
  scale_color_colorblind() + 
  scale_fill_colorblind() +
  labs(y = "Proportion of Community Mass (modeled)", 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  theme(legend.title = element_blank()) +
  NULL


fish_invert_mass_plot_byom = posts_fishinvert %>% 
  filter(quantile_gpp == "Median GPP") %>%
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  # mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  # mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, animal_type) %>% 
  mutate(.epred = .epred) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mat_s, y = .epred, fill = animal_type)) +
  geom_line()  +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  facet_grid(~quantile_om) +
  # scale_y_log10() + 
  theme_default() +
  scale_color_colorblind() + 
  scale_fill_colorblind() +
  labs(y = bquote('Total Biomass: ln('~g/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  theme(legend.title = element_blank()) +
  NULL

saveRDS(fish_invert_mass_plot_byom, file = "plots/fish_invert_mass_plot_byom.rds")
ggsave(fish_invert_mass_plot_byom, file = "plots/fish_invert_mass_plot_byom.jpg", width = 6, height = 2, units = "in")




# individual mass ---------------------------------------

# get data

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


dat_fishinvert = dat_invert %>% mutate(animal_type = "inverts") %>% 
  bind_rows(dat_fish %>% mutate(animal_type = "fish")) %>% 
  group_by(animal_type) %>% 
  mutate(log_dw = log(dw),
         mean_log_dw = mean(log_dw),
         sd_log_dw = sd(log_dw),
         log_dw_c = log_dw - mean_log_dw,
         log_dw_s = log_dw_c/sd_log_dw,
         log_dwm2 = log(dw*no_m2),
         mean_log_dwm2 = mean(log_dwm2),
         sd_log_dwm2 = sd(log_dwm2),
         log_dwm2_c = log_dwm2 - mean_log_dwm2,
         log_dwm2_s = log_dwm2_c/sd_log_dwm2,
         temp_mean = (mat_s*sd_water) + mean_water)

saveRDS(dat_fishinvert, file = "data/derived_data/dat_fishinvert.rds")

# individual size -------------------------------------------------
dat_fishinvert = readRDS(file = "data/derived_data/dat_fishinvert.rds")

qlog_om_s = quantile(unique(dat_fishinvert$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(dat_fishinvert$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))

qmat_s = quantile(unique(dat_fishinvert$mat_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "mat_s", names_to = "quantile_mat") %>% 
  mutate(quantile_mat = c("Low Temp", "Median Temp", "High Temp"))

ind_fish_invert_sizedensity = brm(bf(log_dwm2_s ~ mat_s*animal_type*log_gpp_s*log_om_s + (1|year) + 
                                (1|season)), 
                           data = dat_fishinvert, 
                           family = gaussian(),
                           prior = c(prior(normal(0, 3), class = "Intercept"),
                                     prior(normal(0, 1), class = "b")),
                           iter = 1000, chains = 2,
                           file_refit = "on_change",
                           file = "models/ind_fish_invert_sizedensity_gaussian.rds")

ind_fish_invert_sizedensity = readRDS("models/ind_fish_invert_sizedensity_gaussian.rds")

uncenter = dat_fishinvert %>% ungroup %>% distinct(animal_type, mean_log_dw, sd_log_dw)

ind_mass_posts = tibble(mat_s = seq(min(ind_fish_invert_sizedensity$data$mat_s),
                   max(ind_fish_invert_sizedensity$data$mat_s),
                   length.out = 20)) %>% 
  expand_grid(animal_type = unique(ind_fish_invert_sizedensity$data$animal_type)) %>%
  expand_grid(qlog_om_s) %>% 
  expand_grid(qlog_gpp_s) %>% 
  add_epred_draws(ind_fish_invert_sizedensity, re_formula = NA) %>% 
  mutate(temp_mean = (mat_s*sd_water) + mean_water) %>% 
  left_join(uncenter) 

ind_mass_summary = ind_mass_posts %>% 
  filter(grepl("edian", quantile_om)) %>% 
  filter(grepl("edian", quantile_gpp)) %>% 
  group_by(mat_s, animal_type, temp_mean) %>% 
  median_qi(.epred)

plot_indmass_trend = ind_mass_summary %>% 
  ggplot(aes(x = temp_mean, y = .epred, group = animal_type)) +
  geom_point(data = dat_fishinvert, aes(y = log_dwm2_s, color = animal_type),
             size = 0.1) + 
  geom_line() + 
  geom_ribbon(aes(fill = animal_type, ymin = .lower, ymax = .upper), alpha = 0.7) +
  scale_color_colorblind() + 
  scale_fill_colorblind() + 
  theme_default() + 
  theme(legend.title = element_blank()) + 
  labs(y = expression("log mgDM/" ~ m^2 ~ "(standardized)"),
       x = "Mean Annual Water Temperature (\u00b0C)")

saveRDS(plot_indmass_trend, file = "plots/plot_indmass_trend.rds")
ggsave(plot_indmass_trend, file = "plots/plot_indmass_trend.jpg", width = 6, height = 4,
       dpi = 500)


# ind_all
dat_all = readRDS(file = "data/derived_data/dat_all.rds") %>%
  ungroup %>% 
  mutate(log_dwm2 = log(dw*no_m2),
         mean_log_dwm2 = mean(log_dwm2),
         sd_log_dwm2 = sd(log_dwm2),
         log_dwm2_c = log_dwm2 - mean_log_dwm2,
         log_dwm2_s = log_dwm2_c/sd_log_dwm2,
         mat = (mat_s*sd_water) + mean_water)

ind_allfish_invert_sizedensity = brm(bf(log_dwm2_s ~ mat_s*log_gpp_s*log_om_s + (1|year) +
                                       (1|season)),
                                  data = dat_all,
                                  family = gaussian(),
                                  prior = c(prior(normal(0, 3), class = "Intercept"),
                                            prior(normal(0, 1), class = "b")),
                                  iter = 1000, chains = 2,
                                  file_refit = "on_change",
                                  file = "models/ind_allfish_invert_sizedensity_gaussian.rds")

ind_allfish_invert_sizedensity = readRDS(file = "models/ind_allfish_invert_sizedensity_gaussian.rds")
posts_indall = conditional_effects(ind_allfish_invert_sizedensity, effects = "mat_s")

uncenter_all = dat_all_sampled %>% ungroup %>% distinct(mean_log_dw, sd_log_dw)

plot_indall = as_tibble(posts_indall$mat_s) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water) %>% 
  ggplot(aes(x = mat, y = estimate__)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower__, 
                  ymax = upper__),
              alpha = 0.5) + 
  theme_default() + 
  geom_point(data = dat_all, aes(y = log_dwm2_s), size = 0.2) + 
  labs(y = expression("log mgDM/" ~ m^2 ~ "(standardized)"),
       x = "Mean Annual Water Temperature (\u00b0C)") 

plot_indall

ggsave(plot_indall, file = "plots/plot_indall.jpg", width = 4, height = 4,
       dpi = 500)

library(patchwork)

all_posts_ind = as_tibble(posts_indall$mat_s) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water,
         model = "a)",
         animal_type = "a)") %>% 
  rename(.epred = estimate__,
         .lower = lower__,
         .upper = upper__) %>% 
  bind_rows(ind_mass_summary %>% mutate(model = "b)")) %>% 
  mutate(mat = (mat_s*sd_water) + mean_water)

all_ind_data = bind_rows(ind_allfish_invert_sizedensity$data %>% 
                           mutate(animal_type = "a)",
                                  model = "a)"),
          ind_fish_invert_sizedensity$data %>% mutate(model = "b)")) %>% 
  as_tibble() %>% 
  mutate(mat = (mat_s*sd_water) + mean_water) 

labels = all_posts_ind %>% 
  distinct(mat, animal_type, .epred) %>% 
  mutate(mat = round(mat,0)) %>% 
  filter(mat == 20) %>% 
  group_by(mat, animal_type) %>% 
  summarize(.epred = mean(.epred)) %>% 
  mutate(label = c("All individuals", 
                   "Fish only", 
                   "Invertebrates only"),
         model = c("a)", "b)", "b)"))


all_ind_plot = all_posts_ind %>% 
  ggplot(aes(x = mat, y = .epred, fill = animal_type)) + 
  geom_point(data = all_ind_data, (aes(y = log_dwm2_s, color = animal_type)), 
             size = 0.2, 
             shape = 21,
             alpha = 0.2) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.7) + 
  facet_wrap(~model) + 
  scale_color_manual(values = c("black", "black", "#E69F00")) +
  scale_fill_manual(values = c("black", "black", "#E69F00")) +
  theme_default() +  
  ggrepel::geom_label_repel(data = labels, 
                            aes(label = label),
                            nudge_y = c(2, -2, 2),
                            fill = c("grey70", "grey70", "#E69f00")) + 
  guides(fill = "none", 
         color = "none") +
  labs(y = expression("log mgDM/" ~ m^2 ~ "(standardized)"),
       x = "Mean Annual Water Temperature (\u00b0C)") + 
  theme(strip.text.x = element_text(hjust = 0))


saveRDS(all_ind_plot, file = "plots/all_ind_plot.rds")
ggsave(all_ind_plot, file = "plots/all_ind_plot.jpg", width = 6, height = 3, units = "in",
       dpi = 500)


# quantile regression ---------------------------------------

# fit model
dat_fishinvert = readRDS(file = "data/derived_data/dat_fishinvert.rds")

dat_fishinvert_quantile0.2 = brm(bf(log_dwm2_c ~ mat_s*animal_type + (1|year) + 
                                   (1|season), quantile = 0.2), 
                              data = dat_fishinvert, 
                              family = asym_laplace(),
                              prior = c(prior(normal(0, 1), class = "Intercept"),
                                        prior(normal(0, 1), class = "b"),
                                        prior(exponential(2), class = "sigma")),
                              iter = 1000, chains = 1,
                              file_refit = "on_change",
                              file = "models/dat_fishinvert_quantile0.2.rds")


dat_fishinvert_quantile0.8 = brm(bf(log_dwm2_c ~ mat_s*animal_type + (1|year) + 
                                      (1|season), quantile = 0.8), 
                                 data = dat_fishinvert, 
                                 family = asym_laplace(),
                                 prior = c(prior(normal(0, 1), class = "Intercept"),
                                           prior(normal(0, 1), class = "b"),
                                           prior(exponential(2), class = "sigma")),
                                 iter = 1000, chains = 1,
                                 file_refit = "on_change",
                                 file = "models/dat_fishinvert_quantile0.8.rds")

quant02 = plot(conditional_effects(dat_fishinvert_quantile0.2, effects = "mat_s:animal_type", dpar = "mu"), points = T)
quant08 = plot(conditional_effects(dat_fishinvert_quantile0.8, effects = "mat_s:animal_type", dpar = "mu"), points = T)

quant02_points = layer_data(quant02$`mat_s:animal_type`, 1) %>% mutate(quantile = 0.2)
quant02_lineribbon = layer_data(quant02$`mat_s:animal_type`, 2) %>% mutate(quantile = 0.2)
quant08_points = layer_data(quant08$`mat_s:animal_type`, 1) %>% mutate(quantile = 0.8)
quant08_lineribbon = layer_data(quant08$`mat_s:animal_type`, 2) %>% mutate(quantile = 0.8)


quant_points = bind_rows(quant02_points) %>% # only need one set of points since they're the same
  mutate(animal_type = case_when(group == 2 ~ "inverts",
                                 TRUE ~ "fish")) %>% 
  left_join(dat_fishinvert %>% distinct(mean_log_dwm2, animal_type))
quant_lines = bind_rows(quant02_lineribbon, quant08_lineribbon) %>% 
  mutate(animal_type = case_when(group == 2 ~ "inverts",
                                 TRUE ~ "fish")) %>% 
  left_join(dat_fishinvert %>% distinct(mean_log_dwm2, animal_type))


quantile_animaltype = quant_points %>% 
  ggplot(aes(x = x, y = y + mean_log_dwm2)) + 
  geom_point(aes(color = animal_type), size = 0.2) + 
  geom_line(data = quant_lines, aes(group = as.factor(interaction(group, quantile)))) +
  geom_ribbon(data = quant_lines, aes(fill = animal_type,
                                      ymin = ymin + mean_log_dwm2,
                                      ymax = ymax + mean_log_dwm2,
                                      group = as.factor(interaction(group, quantile))),
              alpha = 0.5) +
  scale_color_colorblind() + 
  scale_fill_colorblind() + 
  theme_default() + 
  theme(legend.title = element_blank()) +
  labs(y = expression("log mgDM/" ~ m^2),
       x = "Mean Annual Water Temperature (\u00b0C)") 

  
ggsave(quantile_animaltype, file = "plots/quantile_animaltype.jpg", width = 6, height = 4, dpi = 500)

# slopes quantile
quantile_slope_posts = bind_rows(as_draws_df(dat_fishinvert_quantile0.2) %>% mutate(quantile = "0.2"),
                                 as_draws_df(dat_fishinvert_quantile0.8) %>% mutate(quantile = "0.8")) %>% 
  mutate(fishslope = b_mat_s,
         invertslope = b_mat_s + `b_mat_s:animal_typeinverts`) %>% 
  select(contains("slope"), quantile) %>% 
  mutate(.draw = 1:nrow(.)) %>% 
  pivot_longer(cols = c(-quantile, -.draw), names_to = "animal_type", values_to = "slope")

quantile_slope_posts %>% 
  ggplot(aes(y = quantile, x = slope, fill = animal_type)) + 
  stat_halfeye()





# ccsr -----------------------------------------------------
d %>% 
  group_by(sample_int, mat_s) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_dw = mean(dw),
            gm_m2 = exp(mean(log(no_m2)))) %>% 
  ggplot(aes(x = gm_m2, y = gm_dw)) + 
  geom_point() +
  # facet_wrap(~animal_type, scales = "free_y") + 
  geom_smooth(method = "lm") + 
  scale_y_log10() + 
  scale_x_log10()


dat_means = d %>% 
  group_by(sample_int, mat_s, year, season, log_gpp_s, log_om_s) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_logdw = mean(log(dw)),
            gm_nom2 = exp(mean(log(no_m2))),
            mean_lognom2 = mean(log(no_m2))) %>% 
  ungroup %>% 
  mutate(mean_lognom2_c = scale(mean_lognom2, scale = F),
         mean_logdw_c = scale(mean_logdw, scale = F))


qlog_om_s = quantile(unique(dat_means$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(dat_means$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))

qmat_s = quantile(unique(dat_means$mat_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "mat_s", names_to = "quantile_mat") %>% 
  mutate(quantile_mat = c("Low Temp", "Median Temp", "High Temp"))



ccsr_brm = brm(mean_lognom2_c ~ mean_logdw_c*mat_s*log_gpp_s*log_om_s + (1|year) + (1|season),
               family = gaussian(),
               data = dat_means, 
               prior = c(prior(normal(-0.75, 0.2), class = "b"),
                         prior(normal(0, 10), class = "Intercept"),
                         prior(exponential(0.1), class = "sd")),
               file_refit = "on_change",
               file = "models/ccsr_brm.rds")

ccsr_posts = tibble(mean_logdw_c = seq(min(dat_means$mean_logdw_c), 
                                       max(dat_means$mean_logdw_c), 
                                       length.out = 20)) %>% 
  expand_grid(qmat_s, qlog_gpp_s, qlog_om_s) %>% 
  add_epred_draws(ccsr_brm, re_formula = NA)


plot_ccsr = ccsr_posts %>% 
  filter(quantile_gpp == "Median GPP") %>% 
  filter(quantile_om == "Median OM") %>% 
  mutate(quantile_mat = "Median Temp") %>% 
  # mutate(quantile_mat = fct_relevel(quantile_mat, "Low Temp", "Median Temp")) %>% 
  group_by(mean_logdw_c) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mean_logdw_c, y = .epred)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper),alpha = 0.2) +
  geom_point(data = dat_means, aes(y = mean_lognom2_c)) +
  theme_default() + 
  labs(y = "Abundance (mean log no per m2 centered)",
       x = "Biomass (mean log mgDM per m2 centered)") + 
  annotate("text", x = 2, y = 5, label = "Slope = -0.87 (95% CrI: -1 to -0.74)")

plot_ccsr


saveRDS(plot_ccsr, file = "plots/plot_ccsr.rds")
ggsave(plot_ccsr, file = "plots/plot_ccsr.jpg", width = 5, height = 5, units = "in", dpi = 500)

ccsr_slopes = tibble(mean_logdw_c = c(0, 1))  %>% 
  expand_grid(qlog_gpp_s, qlog_om_s) %>% 
  expand_grid(mat_s = seq(min(ccsr_brm$data$mat_s), max(ccsr_brm$data$mat_s), length = 20)) %>% 
  add_epred_draws(ccsr_brm, re_formula = NA) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = "mean_logdw_c", values_from = .epred) %>% 
  mutate(slope = `1` - `0`)

ccsr_slopes_summary = ccsr_slopes %>% 
  group_by(mat_s, quantile_gpp, quantile_om) %>% 
  median_qi(slope) %>% 
  mutate(temp_mean = (mat_s*sd_water) + mean_water) 


plot_ccsrslope_interaction = ccsr_slopes_summary %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  ggplot(aes(x = temp_mean, y = slope)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.4) +
  facet_grid(quantile_om ~ quantile_gpp) + 
  theme_default() +
  labs(y = "CCSR Slope",
       x = "Mean Annual Water Temperature (\u00b0C)")

saveRDS(plot_ccsrslope_interaction, file = "plots/plot_ccsrslope_interaction.rds")
ggsave(plot_ccsrslope_interaction, file = "plots/plot_ccsrslope_interaction.jpg", width = 6, height = 6, units = "in", dpi = 500)


plot_ccsr_interaction = ccsr_posts %>% 
  filter(quantile_gpp == "Median GPP") %>% 
  # filter(quantile_om == "Median OM") %>% 
  # mutate(quantile_mat = "Median Temp") %>% 
  # mutate(quantile_mat = fct_relevel(quantile_mat, "Low Temp", "Median Temp")) %>% 
  group_by(mean_logdw_c, quantile_om, quantile_gpp, quantile_mat) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mean_logdw_c, y = .epred)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper),alpha = 0.2) +
  # geom_point(data = dat_means, aes(y = mean_lognom2_c)) +
  theme_default() + 
  labs(y = "Abundance (mean log no per m2 centered)",
       x = "Biomass (mean log mgDM per m2 centered)") + 
  facet_grid(quantile_mat~quantile_om)

saveRDS(plot_ccsr_interaction, file = "plots/plot_ccsr_interaction.rds")
ggsave(plot_ccsr_interaction, file = "plots/plot_ccsr_interaction.jpg", width = 6, height = 6, units = "in", dpi = 500)




# ccsr fish and inverts separate ------------------------------------------

dat_means_fishinverts = dat_fishinvert %>% 
  group_by(sample_int, mat_s, year, season, log_gpp_s, log_om_s, animal_type) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_logdw = mean(log(dw)),
            gm_nom2 = exp(mean(log(no_m2))),
            mean_lognom2 = mean(log(no_m2))) %>% 
  group_by(animal_type) %>% 
  mutate(mean_lognom2_c = scale(mean_lognom2, scale = F),
         mean_logdw_c = scale(mean_logdw, scale = F))



ccsr_brm_animaltype = brm(mean_lognom2_c ~ mean_logdw_c*mat_s*log_gpp_s*log_om_s*animal_type + (1|year) + (1|season),
               family = gaussian(),
               data = dat_means_fishinverts, 
               prior = c(prior(normal(-0.75, 0.2), class = "b"),
                         prior(normal(0, 10), class = "Intercept"),
                         prior(exponential(0.1), class = "sd")),
               file_refit = "on_change",
               file = "models/ccsr_brm_animaltype.rds")

ccsr_animal_plot= plot(conditional_effects(ccsr_brm_animaltype, effects = "mean_logdw_c:animal_type"), points = T)

ggsave(ccsr_animal_plot$`mean_logdw_c:animal_type` + 
         scale_color_colorblind() + 
         scale_fill_colorblind() + 
         theme_default(), 
       file = "plots/ccsr_animal_plot.jpg", width = 6, height = 5)


ccsr_posts_animal = tibble(mean_logdw_c = seq(min(dat_means_fishinverts$mean_logdw_c), 
                          max(dat_means_fishinverts$mean_logdw_c),
                          length.out = 10)) %>% 
  expand_grid(qmat_s) %>% 
  expand_grid(qlog_gpp_s) %>%
  expand_grid(qlog_om_s) %>% 
  expand_grid(animal_type = unique(dat_means_fishinverts$animal_type)) %>% 
  add_epred_draws(ccsr_brm_animaltype, re_formula = NA) %>% 
  group_by(quantile_gpp, quantile_om, mean_logdw_c, quantile_mat, animal_type) %>% 
  median_qi(.epred) %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  mutate(quantile_mat = as.factor(quantile_mat)) %>% 
  mutate(quantile_mat = fct_relevel(quantile_mat, "Low Temp", "Median Temp"))


ccsr_animal_interaction = ccsr_posts_animal %>% 
  filter(grepl("Median", quantile_gpp)) %>% 
  ggplot(aes(x = mean_logdw_c, y = .epred)) + 
  geom_line(aes(color = animal_type, group = interaction(quantile_gpp, animal_type))) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = animal_type,
                  group = interaction(quantile_gpp, animal_type)),
  alpha = 0.4) +
  scale_color_colorblind() + 
  scale_fill_colorblind() + 
  theme_default() +
  theme(legend.title = element_blank()) +
  facet_grid(quantile_mat ~ quantile_om) + 
  labs(y = "Abundance (mean log no per m2 centered)",
       x = "Biomass (mean log mgDM per m2 centered)") 


ggsave(ccsr_animal_interaction, file = "plots/ccsr_animal_interaction.jpg",
       width = 7, height = 6, dpi = 500)



# ccsr animalslope interaction
ccsr_animalslopes = tibble(mean_logdw_c = c(0, 1)) %>% 
  expand_grid(mat_s = seq(min(ccsr_brm_animaltype$data$mat_s), 
                          max(ccsr_brm_animaltype$data$mat_s),
                          length.out = 20)) %>% 
  expand_grid(qlog_gpp_s) %>%
  expand_grid(qlog_om_s) %>% 
  expand_grid(animal_type = unique(dat_means_fishinverts$animal_type)) %>% 
  add_epred_draws(ccsr_brm_animaltype, re_formula = NA) %>%
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = "mean_logdw_c", values_from = .epred) %>% 
  mutate(slope = `1` - `0`)


ccsr_animalslopes_summary = ccsr_animalslopes %>% 
  group_by(quantile_gpp, quantile_om, mat_s, animal_type) %>% 
  median_qi(slope) %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  mutate(temp_mean = (mat_s*sd_water) + mean_water) 

plot_ccsranimalslopes_interaction = ccsr_animalslopes_summary %>% 
  ggplot(aes(x = temp_mean, y = slope, fill = animal_type)) + 
  geom_line(aes(color = animal_type)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.4) +
  facet_grid(quantile_om ~ quantile_gpp) + 
  theme_default() +
  scale_color_colorblind() + 
  scale_fill_colorblind() +
  theme(legend.title = element_blank()) +
  labs(y = "CCSR Slope",
       x = "Mean Annual Water Temperature (\u00b0C)")

saveRDS(plot_ccsranimalslopes_interaction, file = "plots/plot_ccsranimalslopes_interaction.rds")
ggsave(plot_ccsranimalslopes_interaction, file = "plots/plot_ccsranimalslopes_interaction.jpg", width = 6, height = 6, units = "in", dpi = 500)
