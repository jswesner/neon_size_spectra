library(isdbayes)
library(tidybayes)
library(brms)
library(tidyverse)
library(ggthemes)

# load models
fit_temp = readRDS("models/fit_temp.rds")
fit_om = readRDS("models/fit_om.rds")
fit_gpp = readRDS("models/fit_gpp.rds")
fit_temp_om = readRDS("models/fit_temp_om.rds")
fit_temp_gpp = readRDS("models/fit_temp_gpp.rds")
fit_om_gpp = readRDS("models/fit_om_gpp.rds")
fit_temp_om_gpp = readRDS("models/fit_temp_om_gpp.rds")

fit_temp$preds = "temp" 
fit_om$preds = "om"
fit_gpp$preds = "gpp"
fit_temp_om$preds = "temp*om" 
fit_temp_gpp$preds = "temp*gpp"
fit_om_gpp$preds = "om*gpp"
fit_temp_om_gpp$preds = "temp*om*gpp"


# get individual lambdas ----------------------------------------
get_lambdas = function(model = NULL){
  fit_temp_om_gpp$data %>% 
    distinct(sample_id, mat_s, log_gpp_s, log_om_s, year, site_id, xmin, xmax) %>%
    mutate(no_m2 = 1) %>% 
    add_epred_draws(model, re_formula = NULL) %>% 
    mutate(formula = model$preds)
}

all_mods = list(fit_temp, 
                fit_om, 
                fit_gpp, 
                fit_temp_om, 
                fit_temp_gpp,
                fit_om_gpp,
                fit_temp_om_gpp)

all_lambda_posts = lapply(all_mods, FUN = get_lambdas) %>% bind_rows()

all_lambda_posts %>% 
  ggplot(aes(x = mat_s, y = .epred)) + 
  stat_pointinterval(aes(group = sample_id), size = 0.7) +
  facet_wrap(~formula)


# regressions ------------------------------

om_uni = fit_om$data %>% 
  distinct(log_om_s, xmin, xmax) %>% 
  mutate(no_m2 = 1) %>% 
  add_epred_draws(fit_om, re_formula = NA) %>% 
  ggplot(aes(x = log_om_s, y = .epred)) + 
  stat_lineribbon(.width = 0.95, fill = "grey50") + 
  geom_pointrange(data = all_lambda_posts %>% filter(formula == "om") %>% 
                    group_by(sample_id, log_om_s) %>% 
                    median_qi(.epred), 
                  aes(ymin = .lower, ymax = .upper),
                  size = 0.2)

gpp_uni = fit_gpp$data %>% 
  distinct(log_gpp_s, xmin, xmax) %>% 
  mutate(no_m2 = 1) %>% 
  add_epred_draws(fit_gpp, re_formula = NA) %>% 
  ggplot(aes(x = log_gpp_s, y = .epred)) + 
  stat_lineribbon(.width = 0.95, fill = "grey50") + 
  geom_pointrange(data = all_lambda_posts %>% filter(formula == "gpp") %>% 
                       group_by(sample_id, log_gpp_s) %>% 
                       median_qi(.epred), 
                  aes(ymin = .lower, ymax = .upper),
                  size = 0.2)

temp_uni = fit_temp$data %>% 
  distinct(mat_s, xmin, xmax) %>% 
  mutate(no_m2 = 1) %>% 
  add_epred_draws(fit_temp, re_formula = NA) %>% 
  ggplot(aes(x = mat_s, y = .epred)) + 
  stat_lineribbon(.width = 0.95, fill = "grey50") + 
  geom_pointrange(data = all_lambda_posts %>% filter(formula == "temp") %>% 
                    group_by(sample_id, mat_s) %>% 
                    median_qi(.epred), 
                  aes(ymin = .lower, ymax = .upper),
                  size = 0.2)


library(patchwork)
temp_uni + om_uni + gpp_uni


# plot parameters ---------------------------------------------------------

get_draws_with_preds = function(model = NA){
  as_draws_df(model) %>% 
    mutate(preds = model$preds)
}

all_draws = bind_rows(lapply(all_mods, get_draws_with_preds)) %>% 
  select(starts_with(c("b_", ".draw", "preds"))) %>%
  pivot_longer(cols = starts_with("b_")) %>% 
  filter(!is.na(value)) %>% 
  mutate(temp = case_when(grepl("mat_s", name) ~ "temperature",
                          TRUE ~ "other")) %>% 
  mutate(preds = paste0("~ ", preds, " ..."),
         preds = case_when(preds == "~ temp ..." ~ "a) ~temp ...",
                           preds == "~ gpp ..." ~ "c) ~gpp ...",
                           preds == "~ om ..." ~ "b) ~om ...",
                           preds == "~ temp*gpp ..." ~ "d) ~temp*gpp ...",
                           preds == "~ temp*om ..." ~ "e) ~temp*om ...",
                           preds == "~ om*gpp ..." ~ "f) ~om*gpp ...",
                           TRUE ~ "g) ~temp*om*gpp ...")) %>% 
  mutate(name_length = str_length(name)) %>% 
  mutate(name = str_replace(name, "mat", "temp"),
         name = str_replace(name, "b_", ""))


parameter_plot = all_draws  %>% 
  filter(name != "Intercept") %>% 
  ggplot(aes(x = reorder(name, -name_length), y = value, color = temp)) + 
  stat_pointinterval(position = position_dodge(width = 0.4)) +
  geom_hline(yintercept = 0) +
  scale_color_colorblind() + 
  theme_default() +
  labs(x = "Parameter",
       y = "Parameter Value") +
  coord_flip() +
  facet_wrap(~preds, ncol = 3) +
  guides(color = "none") +
  scale_y_continuous(breaks = c(-0.04, -0.02, 0, 0.02, 0.04)) +
  NULL

ggview::ggview(parameter_plot, width = 6.5, height = 7)
ggsave(parameter_plot, file = "plots/ms_plots/parameter_plot.jpg", 
       width = 6.5, height = 7, units = "in", dpi = 600 )
  


# plot inverts and fish separately ----------------------------------------
# ISD
isd_by_temp_fish = readRDS(file = "plots/isd_by_temp-fishonly.rds")
isd_by_temp_inverts = readRDS(file = "plots/isd_by_temp-invertsonly.rds") 
isd_by_temp = readRDS(file = "plots/isd_by_temp.rds")
fish_invert_mass = readRDS("models/fish_invert_mass_brm.rds")
mean_temp = mean(unique(dat$temp_mean))
sd_temp = sd(unique(dat$temp_mean))

all_plots = list(isd_by_temp,
                 isd_by_temp_inverts,
                 isd_by_temp_fish)

# get dots
temp_data_list = NULL
for(i in 1:length(temp_data)){
  temp_data_list[[i]] = all_plots[[i]]$data %>% 
    mutate(name = paste0(letters[i + 3], ")"))
  all_plot_data = bind_rows(temp_data_list) %>% 
    mutate(name = case_when(name == "d)" ~ "d) fish + inverts",
                            name == "e)" ~ "e) inverts",
                            TRUE ~ "f) fish"))
}

# get lines
temp_lines_list = NULL
for(i in 1:length(temp_data)){
  temp_data_list[[i]] = all_plots[[i]]$layers[[2]]$data %>% 
    mutate(name = paste0(letters[i + 3], ")"))
  all_plot_lines = bind_rows(temp_data_list) %>% 
    mutate(name = case_when(name == "d)" ~ "d) fish + inverts",
                            name == "e)" ~ "e) inverts",
                            TRUE ~ "f) fish"))
}

all_lambda_plots = all_plot_lines %>% 
  ggplot(aes(x = raw_temp, y = lambda, ymin = .lower, ymax = .upper)) + 
  geom_line() + 
  geom_ribbon(alpha = 0.4) +
  facet_wrap(~name) + 
  geom_pointrange(data = all_plot_data, size = 0.1, linewidth = 0.2, 
                  alpha = 0.1) + 
  theme_default() +
  labs(y = paste0("\u03bb", " (ISD exponent)"),
       x = "Mean Annual Temperature (°C)") +
  theme(strip.text = element_text(hjust = 0))


# standing stock mass
fish_mass_brm = readRDS(file = "models/fish_mass_brm.rds")
invert_mass_brm = readRDS(file = "models/invert_mass_brm.rds")
community_mass_brm = readRDS("models/community_mass_brm.rds")

fish_mass_brm$facet = "c) fish"
invert_mass_brm$facet = "b) inverts"
community_mass_brm$facet = "a) fish + inverts"

get_mass_draws = function(model = NULL){
  tibble(mat_s = seq(min(model$data$mat_s), max(model$data$mat_s), length.out = 20)) %>% 
    mutate(log_om_s = 0,
           log_gpp_s = 0) %>% 
    add_epred_draws(model, re_formula = NA) %>% 
    mutate(name = model$facet,
           raw_temp = (mat_s*sd_temp)+mean_temp)
}

get_mass_dots = function(model = NULL){
  model$data %>% 
    mutate(name = model$facet,
           raw_temp = (mat_s*sd_temp)+mean_temp)
}

all_mass_lines = lapply(list(fish_mass_brm, invert_mass_brm, community_mass_brm), get_mass_draws) %>% bind_rows()
all_mass_dots = lapply(list(fish_mass_brm, invert_mass_brm, community_mass_brm), get_mass_dots) %>% bind_rows()

all_mass_plots = all_mass_lines %>% 
  ggplot(aes(x = raw_temp, y = .epred)) +
  stat_lineribbon(.width = c(0.95), alpha = 0.3) + 
  geom_point(data = all_mass_dots, aes(y = log_total_g), size = 0.2) +
  facet_wrap(~name) +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  theme_default() +
  labs(y = bquote('Total Biomass: ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Temperature (\u00b0C)")+
  guides(color = "none",
         fill = "none") +
  theme(strip.text = element_text(hjust = 0))
  


# combine

fish_invert_plot = plot_grid(all_mass_plots,
                             all_lambda_plots, ncol = 1)

ggview::ggview(fish_invert_plot, width = 6.5, height = 6)
ggsave(fish_invert_plot, file = "plots/ms_plots/fish_invert_plot.jpg", width = 6.5, height = 6, dpi = 600)
