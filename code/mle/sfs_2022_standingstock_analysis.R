library(brms)
library(tidyverse)
library(tidybayes)
library(janitor)
library(ggridges)
library(sizeSpectra)
library(scales)
library(ggthemes)
library(ggdark)

# mle estimates from January Grand Junction meeting. Includes fish and macroinvertebrates
mle_output <- readRDS("results/mle_b_estimate_fish_macro.RDS")
# load temp estimates mean annual water temperatures
mat_posts <- readRDS("code/temperature/posteriors/mat_posts.rds") %>% clean_names

# data to analyze mle as a function of temperature
mle_mat <- mle_output %>% left_join(mat_posts)

#load raw data (fish + macros)
dat_mg <- readRDS("data/derived_data/macro_fish_dw.rds") %>% 
  mutate(mgdm_perm2 = dw*no_m2) %>% 
  group_by(animal_type, site_id, year_month) %>% 
  summarize(mgdm_perm2 = sum(mgdm_perm2),
            gdm_perm2 = mgdm_perm2/1000) %>% 
  left_join(mle_mat %>% distinct(mat_site, site_id, sdat_site))
  
dat_mg %>% 
  ggplot(aes(x = mat_site, y = gdm_perm2)) + 
  geom_point(aes(color = animal_type)) + 
  scale_y_log10()

# Fit model ---------------------------------------------------------------

brm_standingstock_sfs <- brm(gdm_perm2 ~ mat_site + (1|site_id) + (1 + mat_site|animal_type),
                             data = dat_mg,
                             family = Gamma(link = "log"),
                             prior = c(prior(normal(0, 2), class = "Intercept"),
                                       prior(normal(0, 0.1), class = "b"),
                                       prior(exponential(1), class = "sd"),
                                       prior(exponential(1), class = "shape")),
                             file = "code/mle/results/brm_standingstock_sfs.rds", 
                             file_refit = "on_change")




# check model
pp_check(brm_standingstock_sfs) + scale_x_log10()

# conditional plot
plot(conditional_effects(brm_standingstock_sfs, re_formula = NULL), points = T)

# extract posteriors and summarize slope
posts <- as_draws_df(brm_standingstock_sfs) %>% as_tibble() %>% clean_names() 

# slopes by animal type and overall
posts_by_type <- posts %>% select(!contains("site_id")) %>% 
  pivot_longer(cols = c(r_animal_type_macroinvertebrates_mat_site, r_animal_type_fish_mat_site)) %>% 
  mutate(animal_type_slope = b_mat_site + value) %>% 
  select(b_mat_site, draw, name, value) %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  pivot_longer(cols = -draw) 

posts_by_type %>% 
  group_by(name) %>% 
  median_qi(value)

posts_by_type %>% 
  group_by(name) %>% 
  summarize(prob_positive = sum(value <0)/nrow(.))


# site specific predictions
post_preds <- brm_standingstock_sfs$data %>% 
  distinct(site_id, mat_site, animal_type) %>% 
  add_predicted_draws(brm_standingstock_sfs, re_formula = NULL)

# plot ridges
g_dm_plot_dark <- post_preds %>% 
  ggplot(aes(x = .prediction, fill = animal_type, y = reorder(site_id, mat_site))) + 
  geom_point(data = brm_standingstock_sfs$data, aes(x = gdm_perm2), shape = "|") +
  geom_density_ridges(alpha = 0.5) + 
  # scale_fill_colorblind() +
  scale_x_log10(limits = c(1e-04, NA), labels = comma) + 
  dark_theme_classic() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 30)) +
  labs(y = "Site",
       x = expression("Grams dry mass/m"^2))
  

g_dm_plot_light <- post_preds %>% 
  ggplot(aes(x = .prediction, fill = animal_type, y = reorder(site_id, mat_site))) + 
  geom_point(data = brm_standingstock_sfs$data, aes(x = gdm_perm2), shape = "|") +
  geom_density_ridges(alpha = 0.5) + 
  # scale_fill_colorblind() +
  scale_x_log10(limits = c(1e-04, NA), labels = comma) + 
  theme_classic() + 
  theme(legend.position = "top",
        legend.title = element_blank(),
        text = element_text(size = 30)) +
  labs(y = "Site",
       x = expression("Grams dry mass/m"^2))


ggsave(g_dm_plot_light, file = "plots/g_dm_plot_light.jpg", dpi = 400, width = 9, height = 9)
ggsave(g_dm_plot_dark, file = "plots/g_dm_plot_dark.jpg", dpi = 400, width = 9, height = 9)


# plot regression

# conditional plot spaghetti
post_preds <- brm_standingstock_sfs$data %>% 
  distinct(mat_site) %>% 
  add_epred_draws(brm_standingstock_sfs, re_formula = NA)
  
library(viridis)

g_dm_regression_plot_dark <- post_preds %>% 
  filter(.draw <= 1000) %>% 
  ggplot(aes(x = mat_site, y = .epred)) +
  geom_line(aes(group = .draw), color = "white", alpha = 0.3) +  
  geom_point(data = brm_standingstock_sfs$data, aes(y = gdm_perm2, fill = mat_site),
             size = 3, shape = 21) + 
  guides(fill = "none") +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                labels = c("0.01", "0.1", "1", "10","100", "1000", "10,000"),
                limits = c(0.001, 10000)) +
  dark_theme_classic() +
  scale_fill_viridis(option = "B") +
  labs(y = expression("Grams dry mass/m"^2),
       x = "Mean Annual Temp (deg C)") + 
  theme(text = element_text(size = 38, family = "serif"))

g_dm_regression_plot_light <- post_preds %>% 
  filter(.draw <= 1000) %>% 
  ggplot(aes(x = mat_site, y = .epred)) +
  geom_line(aes(group = .draw), color = "black", alpha = 0.1) +  
  geom_point(data = brm_standingstock_sfs$data, aes(y = gdm_perm2, fill = mat_site),
             size = 3, shape = 21) + 
  guides(fill = "none") +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                labels = c("0.01", "0.1", "1", "10","100", "1000", "10,000"),
                limits = c(0.001, 10000)) +
  theme_default() +
  scale_fill_viridis(option = "B") +
  labs(y = expression("Grams dry mass/m"^2),
       x = "Mean Annual Temp (deg C)") + 
  theme(text = element_text(size = 38))


ggsave(g_dm_regression_plot_dark, file = "plots/g_dm_regression_plot_dark.jpg", dpi = 400, width = 10, height = 9)
ggsave(g_dm_regression_plot_light, file = "plots/g_dm_regression_plot_light.jpg", dpi = 400, width = 10, height = 9)


# conditional plot spaghetti by group
post_preds_type <- brm_standingstock_sfs$data %>% 
  distinct(mat_site, animal_type) %>%
  mutate(site_id = NA) %>% 
  add_epred_draws(brm_standingstock_sfs, re_formula = NULL)

g_dm_data_three <- dat_mg %>% 
  group_by(mat_site, site_id, year_month) %>% 
  summarize(gdm_perm2 = mean(gdm_perm2)) %>% 
  mutate(animal_type = "fish + macroinvertebrates") %>% 
  bind_rows(brm_standingstock_sfs$data)

gm_dm_three_regression_plot_dark <- post_preds_type %>% 
  bind_rows(post_preds %>% mutate(animal_type = "fish + macroinvertebrates")) %>% 
  mutate(animal_type = fct_relevel(animal_type, "fish", "macroinvertebrates")) %>% 
  filter(.draw <= 1000) %>% 
  ggplot(aes(x = mat_site, y = .epred)) + 
  geom_line(aes(group = .draw), color = "white", alpha = 0.03) + 
  geom_point(data = g_dm_data_three, aes(y = gdm_perm2, fill = mat_site),
             size = 3, shape = 21) + 
  guides(fill = "none") +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                labels = c("0.01", "0.1", "1", "10","100", "1000", "10,000"),
                limits = c(0.001, 10000)) +
  dark_theme_classic() +
  scale_fill_viridis(option = "B") +
  labs(y = expression("Grams dry mass/m"^2),
       x = "Mean Annual Temp (deg C)") + 
  facet_wrap(~animal_type, ncol = 1) +
  theme(text = element_text(size = 30, family = "serif"),
        axis.text.y = element_text(size = 20, family = "serif"),
        axis.text.x  =element_text(size = 20, family = "serif"))

gm_dm_three_regression_plot_light <- post_preds_type %>% 
  bind_rows(post_preds %>% mutate(animal_type = "fish + macroinvertebrates")) %>% 
  mutate(animal_type = fct_relevel(animal_type, "fish", "macroinvertebrates")) %>% 
  filter(.draw <= 1000) %>% 
  ggplot(aes(x = mat_site, y = .epred)) + 
  geom_line(aes(group = .draw), color = "black", alpha = 0.03) + 
  geom_point(data = g_dm_data_three, aes(y = gdm_perm2, fill = mat_site),
             size = 3, shape = 21) + 
  guides(fill = "none") +
  scale_y_log10(breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000),
                labels = c("0.01", "0.1", "1", "10","100", "1000", "10,000"),
                limits = c(0.001, 10000)) +
  theme_default() +
  scale_fill_viridis(option = "B") +
  labs(y = expression("Grams dry mass/m"^2),
       x = "Mean Annual Temp (deg C)") + 
  facet_wrap(~animal_type, ncol = 1) +
  theme(text = element_text(size = 30, family = "serif"),
        axis.text.y = element_text(size = 20, family = "serif"),
        axis.text.x  =element_text(size = 20, family = "serif"))


ggsave(gm_dm_three_regression_plot_dark, file = "plots/g_dm_three_regression_plot_dark.jpg", dpi = 400, width = 7, height = 11)
ggsave(gm_dm_three_regression_plot_light, file = "plots/g_dm_three_regression_plot_light.jpg", dpi = 400, width = 7, height = 11)




