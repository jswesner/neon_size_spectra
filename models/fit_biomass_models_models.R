library(brms)
library(tidyverse)
library(tidybayes)
library(here)
library(hydroTSM)
library(ggthemes)

# get data
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
         dw_s = dw/mean(dw))

# fish plus inverts
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


# total biomass -----------------------------------------------------------




community_mass_brm = brm(log_total_g_s ~ log_gpp_s*log_om_s*mat_s + (1|year) + (1|season),
                         family = gaussian(),
                         data = community_mass,
                         prior = c(prior(normal(1, 2), class = "Intercept"),
                                   prior(normal(0, 1), class = "b"),
                                   prior(exponential(1), class = "sd")),
                         file = "models/community_mass_brm.rds",
                         file_refit = "on_change")

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
  labs(y = bquote('Total Biomass: ln('*g/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") +
  # scale_y_log10() +
  NULL

ggview::ggview(community_mass_univariate_plot, width = 5, height = 5, units = "in")
ggsave(community_mass_univariate_plot, width = 5, height = 5, units = "in",
       file = "plots/community_mass_univariate_plot.jpg", dpi = 500)



community_mass_plot = posts %>% 
  # filter(quantile_om == "Median OM") %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, mat) %>% 
  mutate(.epred = .epred*mean(community_mass$log_total_g)) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mat, y = .epred)) +
  geom_line()  +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) + 
  facet_grid(quantile_om~quantile_gpp) +
  # scale_y_log10() + 
  labs(y = bquote('Total Biomass: ln('*g/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") + 
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
  labs(y = bquote('Total Biomass: ln('*g/m^'2'*")"), 
       x = "Mean Annual Water Temperature (\u00b0C)") + 
  NULL

saveRDS(community_mass_plot_byom, file = "plots/community_mass_plot_byom.rds")
ggsave(community_mass_plot_byom, file = "plots/community_mass_plot_byom.jpg", width = 5.5, height = 2, units = "in")




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
  scale_color_colorblind() + 
  scale_fill_colorblind() +
  labs(y = bquote('Total Biomass: ln('*g/m^'2'*")"), 
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
  scale_color_colorblind() + 
  scale_fill_colorblind() +
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
  labs(y = bquote('Total Biomass: ln('~g/m^'2'*")"), 
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




# model with gamma --------------------------------------------------------


fish_invert_mass_gamma_brm = brm(total_g_dwm2_s ~ log_gpp_s*log_om_s*mat_s*animal_type + (1|year) + (1|season) + (1|sample_id),
                                 family = Gamma(link = "log"),
                                 data = fish_invert_mass,
                                 prior = c(prior(normal(0, 2), class = "Intercept"),
                                           prior(normal(0, 1), class = "b"),
                                           prior(exponential(1), class = "sd")),
                                 file = "models/fish_invert_mass_gamma_brm.rds",
                                 file_refit = "on_change", 
                                 cores = 4)


ids = fish_invert_mass_gamma_brm$data %>% distinct(sample_id, log_om_s, log_gpp_s, mat_s, animal_type, year, season)

predictions_ids = predict(fish_invert_mass_gamma_brm, summary = F) %>% 
  t() %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  cbind(ids)  %>% 
  pivot_longer(cols = starts_with("V"), names_to = ".draw", values_to = "value") %>% 
  mutate(.draw = parse_number(.draw))

proportion_posts = predictions_ids %>% 
  mutate(value = exp(value)) %>% 
  pivot_wider(names_from = animal_type, values_from = value) %>% 
  mutate(total = fish + inverts) %>% 
  mutate(fish = fish/total,
         inverts = inverts/total) %>% 
  pivot_longer(cols = c(fish, inverts), names_to = "animal_type", 
               values_to = "proportion")


proportion_posts %>% 
  group_by(sample_id, mat_s, log_gpp_s, log_om_s, animal_type) %>% 
  median_qi(proportion) %>% 
  filter(animal_type == "fish") %>% 
  ggplot(aes(x = mat_s, y = proportion)) + 
  # geom_pointrange(aes(ymin = .lower, ymax = .upper)) +
  ylim(0, 1) +
  geom_point() + 
  geom_smooth(method = "lm")



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
  mutate(mean_dw = mean(dw),
         dw_s = dw/mean(dw))


# fit model

ind_fish_invert_size = brm(bf(dw_s ~ mat_s*animal_type + (1|year) + 
                                (1|season)), 
                           data = dat_fishinvert, 
                           family = Gamma(link = "log"),
                           prior = c(prior(normal(0, 3), class = "Intercept"),
                                     prior(normal(0, 1), class = "b")),
                           iter = 1000, chains = 2,
                           file = "models/ind_fish_invert_size.rds")

posts_ind_size = ind_fish_invert_size$data %>% 
  distinct(mat_s, animal_type) %>% 
  add_epred_draws(ind_fish_invert_size, re_formula = NA) %>% 
  left_join(dat_fishinvert %>% ungroup %>% distinct(animal_type, mean_dw)) %>% 
  mutate(raw_epred = .epred*mean_dw)

posts_ind_size %>%
  group_by(animal_type, mat_s) %>% 
  median_qi(y = .epred) %>% 
  ggplot(aes(x = mat_s, y = y, fill = animal_type)) + 
  geom_line(aes(color = animal_type)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.4) + 
  scale_y_log10() +
  geom_point(data = dat_fishinvert, aes(y = dw_s, 
                                        color = animal_type), size = 0.2,
             position = position_jitter(width = 0.01)) + 
  facet_wrap(~animal_type, scale = "free_y") +
  geom_boxplot(data = dat_fishinvert, aes(y = dw_s, group = mat_s), 
                                          outlier.shape = NA)




ind_conds = plot(conditional_effects(ind_fish_invert_size, effects = "mat_s:animal_type"), points = T)

ind_conds$`mat_s:animal_type` + 
  scale_y_log10()



# individual mass quantile regression ---------------------------------------

# fit model

dat_fishinvert_quantile0.2 = brm(bf(dw_s ~ mat_s*animal_type + (1|year) + 
                                   (1|season), quantile = 0.2), 
                              data = dat_fishinvert, 
                              family = asym_laplace(),
                              iter = 1000, chains = 2,
                              file = "models/dat_fishinvert_quantile0.2.rds")

dat_fishinvert_quantile0.8 = brm(bf(dw_s ~ mat_s*animal_type + (1|year) + 
                                      (1|season), quantile = 0.2), 
                                 data = dat_fishinvert, 
                                 family = asym_laplace(),
                                 iter = 1000, chains = 2,
                                 file = "models/dat_fishinvert_quantile0.8.rds")


post_quantile0.2 = dat_fishinvert_quantile0.2$data %>% 
  distinct(mat_s, animal_type) %>% 
  add_epred_draws(dat_fishinvert_quantile0.2, dpar = "mu", re_formula = NA) %>% 
  mutate(quantile = "0.2")

post_quantile0.8 = dat_fishinvert_quantile0.8$data %>% 
  distinct(mat_s, animal_type) %>% 
  add_epred_draws(dat_fishinvert_quantile0.8, dpar = "mu", re_formula = NA) %>% 
  mutate(quantile = "0.8")

post_quantiles = bind_rows(post_quantile0.2, post_quantile0.8) %>% 
  left_join(dat_fishinvert %>% ungroup %>% distinct(animal_type, mean_dw)) %>% 
  mutate(dw = mu*mean_dw)



post_quantiles %>% 
  group_by(mat_s, animal_type, quantile) %>% 
  median_qi(dw) %>% 
  ggplot(aes(x = mat_s, y = dw, fill = quantile)) + 
  geom_line(aes(group = quantile)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, group = interaction(animal_type, quantile)),
              alpha = 0.2) + 
  facet_wrap(~animal_type, scales = "free_y")


# percent change in mass
percent_change = post_quantiles %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration, -mu, -.epred) %>% 
  group_by(animal_type, quantile) %>% 
  filter(mat_s == min(mat_s) | mat_s == max(mat_s)) %>% 
  mutate(mat_s = round(mat_s, 1)) %>% 
  pivot_wider(names_from = mat_s, values_from = dw) %>% 
  mutate(change = (`2` - `-1.6`)/`-1.6`) 

percent_change %>% 
  ggplot(aes(x = change, y = animal_type, fill = quantile)) + 
  ggridges::geom_density_ridges() + 
  coord_cartesian(xlim = c(-2, 2))


# ccsr -----------------------------------------------------

dat_fishinvert %>% 
  group_by(sample_int, animal_type, mat_s) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_dw = mean(dw)) %>% 
  ggplot(aes(x = mat_s, y = gm_dw)) + 
  geom_point() +
  facet_wrap(~animal_type, scales = "free_y") + 
  geom_smooth(method = "lm") + 
  scale_y_log10()


dat_fishinvert %>% 
  group_by(sample_int, mat_s) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_dw = mean(dw)) %>% 
  ggplot(aes(x = mat_s, y = gm_dw)) + 
  geom_point() +
  # facet_wrap(~animal_type, scales = "free_y") + 
  geom_smooth(method = "lm") + 
  scale_y_log10()

dat_fishinvert %>% 
  group_by(sample_int, mat_s) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_dw = mean(log(dw)),
            gm_nom2 = exp(mean(log(no_m2))),
            mean_nom2 = mean(log(no_m2))) %>% 
  ggplot(aes(x = gm_nom2, y = gm_dw)) + 
  geom_point() +
  # facet_wrap(~animal_type, scales = "free") +
  geom_smooth(method = "lm") + 
  scale_y_log10() +
  scale_x_log10() +
  NULL


dat_means = dat_fishinvert %>% 
  group_by(sample_int, mat_s, year, season, log_gpp_s, log_om_s) %>% 
  summarize(gm_dw = exp(mean(log(dw))),
            median_dw = median(dw),
            mean_logdw = mean(log(dw)),
            gm_nom2 = exp(mean(log(no_m2))),
            mean_lognom2 = mean(log(no_m2))) %>% 
  ungroup %>% 
  mutate(mean_lognom2_c = scale(mean_lognom2, scale = F),
         mean_logdw_c = scale(mean_logdw, scale = F))



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


ccsr_posts %>% 
  filter(quantile_gpp == "Median GPP") %>% 
  filter(quantile_om == "Median OM") %>% 
  mutate(quantile_mat = as.factor(quantile_mat)) %>% 
  mutate(quantile_mat = fct_relevel(quantile_mat, "Low Temp", "Median Temp")) %>% 
  group_by(mean_logdw_c, mat_s, quantile_mat) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mean_logdw_c, y = .epred)) + 
  geom_line(aes(color = quantile_mat)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = quantile_mat),alpha = 0.2)


ccsr_slopes = tibble(mean_logdw_c = c(0, 1))  %>% 
  expand_grid(qmat_s, qlog_gpp_s, qlog_om_s) %>% 
  add_epred_draws(ccsr_brm, re_formula = NA) %>% 
  ungroup() %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = "mean_logdw_c", values_from = .epred) %>% 
  mutate(slope = `1` - `0`)

ccsr_slopes_summary = ccsr_slopes %>% 
  group_by(quantile_mat, quantile_gpp, quantile_om) %>% 
  median_qi(slope)

ccsr_slopes_summary %>% 
  ggplot(aes(x = slope, xmin = .lower, xmax = .upper,
             y = reorder(interaction(quantile_gpp, quantile_om, quantile_mat), slope))) + 
  geom_pointrange() + 
  # facet_wrap(~quantile_mat) + 
  geom_vline(xintercept = c(-0.75, -1))



