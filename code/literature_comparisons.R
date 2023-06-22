library(tidyverse)
library(brms)
library(tidybayes)
# Literature Figure Comparisons -------------------------------------------
theme_set(theme_default())

fit_pareto = readRDS("models/fit_pareto.rds")
source("code/sandbox/paretocounts.R")

dat_all = readRDS("data/derived_data/dat_all.rds")
mean_temp = mean(unique(dat_all$mean))
sd_temp = sd(unique(dat_all$mean))

# literature comparison

lit <- read_csv("data/temp_summaries_table.csv") %>% 
        filter(Include == "Y") %>% 
  mutate(size_magnitude = round(log10(xmax) - log10(xmin),0),
         size_scale = size_magnitude*scale_km) %>% 
  group_by(Author, organisms, b_diff) %>% 
  mutate(id = cur_group_id())

mod_best <- readRDS("models/fit_pareto.rds")
mod_summary = summary(mod_best)
conds = tibble(mat_s = seq(min(fit_pareto$data$mat_s), max(fit_pareto$data$mat_s), length.out = 20)) %>% 
  mutate(log_gpp_s = 0,
         log_om_s = 0,
         no_m2 = 1,
         xmin = 0.0035, 
         xmax = 200000) %>% 
  add_epred_draws(fit_pareto, re_formula = NA) %>% 
  group_by(mat_s) %>% 
  median_qi(.epred)



# plot slopes (scale-independent)
conds_scaled = conds %>% 
  mutate(value = .epred - mean(.epred),
         .lower = .lower - mean(.epred),
         .upper = .upper - mean(.epred),
         Driver = "Temperature",
        x = mat_s,
        id = "Gjoni et al. 2023",
        group = "This Study") %>% 
  mutate(x_raw = (mat_s*sd_temp) + mean_temp)

lit_plot_scaled = lit %>% 
  filter(Driver == "Temperature") %>% 
  filter(Author != "Gjoni et al. 2023") %>% 
  mutate(low = 0 - 0.5*direction,
         high = 0 + 0.5*direction,
         group = "Literature Estimates") %>% 
  pivot_longer(cols = c(low, high)) %>% 
  mutate(x = case_when(name == "low" ~ min(conds$mat_s), TRUE ~ max(conds$mat_s))) %>% 
  ggplot(aes(x = x, y = value)) + 
  geom_line(aes(group = id), alpha = 0.5) +
  # facet_wrap(~Driver) + 
  geom_line(data = conds_scaled) +
  geom_ribbon(data = conds_scaled,
              aes(ymin = .lower,
                  ymax = .upper),
              alpha = 0.7,
              fill = "orange") +
  ylim(-0.4, 0.4) +
  labs(y = "Change in \u03bb (scaled)",
       x = "Temperature (scaled)")

lit_plot_scaled_noribbon = lit %>% 
  filter(Driver == "Temperature") %>% 
  filter(Author != "Gjoni et al. 2023") %>% 
  mutate(low = 0 - 0.5*direction,
         high = 0 + 0.5*direction,
         group = "Literature Estimates") %>% 
  pivot_longer(cols = c(low, high)) %>% 
  mutate(x = case_when(name == "low" ~ min(conds$mat_s), TRUE ~ max(conds$mat_s))) %>% 
  ggplot(aes(x = x, y = value)) + 
  geom_line(aes(group = id), alpha = 0.5) +
  # facet_wrap(~Driver) + 
  # geom_line(data = conds_scaled) +
  # geom_ribbon(data = conds_scaled,
  #             aes(ymin = .lower,
  #                 ymax = .upper),
  #             alpha = 0.7,
  #             fill = "orange") +
  ylim(-0.4, 0.4) +
  labs(y = "\u03bb (scaled)",
       x = "Temperature (scaled)")

lit_plot_unscaled = lit %>% 
  filter(Driver == "Temperature") %>% 
  filter(Author != "Gjoni et al. 2023") %>% 
  mutate(low = 0 - 0.5*direction,
         high = 0 + 0.5*direction,
         group = "Literature Estimates") %>% 
  pivot_longer(cols = c(low, high)) %>% 
  mutate(x = case_when(name == "low" ~ temp_low, TRUE ~ temp_high)) %>% 
  ggplot(aes(x = x, y = value)) + 
  geom_line(aes(group = id), alpha = 0.5) +
  # facet_wrap(~Driver) + 
  geom_line(data = conds_scaled, aes(x = x_raw)) +
  geom_ribbon(data = conds_scaled,
              aes(ymin = .lower,
                  ymax = .upper,
                  x = x_raw),
              alpha = 0.7,
              fill = "orange") +
  labs(y = "\u03bb (scaled)",
       x = "Temperature (\u00b0C)")



lit_plot_otherdrivers = lit %>% 
  # filter(Driver == "Temperature") %>%
  filter(Author != "Gjoni et al. 2023") %>% 
  mutate(low = 0 - 0.5*direction,
         high = 0 + 0.5*direction,
         group = "Literature Estimates") %>% 
  pivot_longer(cols = c(low, high)) %>% 
  ungroup() %>% 
  mutate(x = case_when(name == "low" ~ min(conds$mat_s), TRUE ~ max(conds$mat_s)),
         Driver = fct_relevel(Driver, "Temperature")) %>% 
  ggplot(aes(x = x, y = value)) + 
  geom_line(aes(group = id,
                color = Driver), alpha = 0.5) +
  # facet_wrap(~Driver) + 
  ggthemes::scale_color_colorblind() +
  geom_line(data = conds_scaled) +
  geom_ribbon(data = conds_scaled,
              aes(ymin = .lower,
                  ymax = .upper),
              alpha = 0.7,
              fill = "orange") +
  ylim(-0.4, 0.4) +
  labs(y = "\u03bb (scaled)",
       x = "Temperature (scaled)")


lit_plot_boxplot = lit %>% 
  # mutate(Driver = fct_relevel(Driver, "Temperature")) %>% 
  ggplot(aes(x = reorder(Driver, b_diff),
             y = b_diff)) +
  geom_boxplot(aes(group = Driver, fill = reorder(Driver, b_diff)), width = 0.2) +
  geom_point() +
  ggthemes::scale_fill_colorblind() +
  theme_default() +
  labs(x = "Stressor",
       y = "Absolute change in \u03bb")



ggsave(lit_plot_scaled, file = "plots/lit_plot_scaled.jpg", 
       width = 5, height = 5)

ggsave(lit_plot_scaled_noribbon, file = "plots/lit_plot_scaled_noribbon.jpg", 
       width = 5, height = 5)

ggsave(lit_plot_unscaled, file = "plots/lit_plot_unscaled.jpg", 
       width = 5, height = 5)

ggsave(lit_plot_otherdrivers + guides(color = "none"), file = "plots/lit_plot_otherdrivers.jpg", 
       width = 5, height = 5)

ggsave(lit_plot_boxplot+ guides(color = "none",
                                fill = "none"), file = "plots/lit_plot_boxplot.jpg", 
       width = 5, height = 5)

