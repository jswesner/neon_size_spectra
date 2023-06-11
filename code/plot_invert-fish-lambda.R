library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas

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
# dat_all = neon_sizes_2016_2021 %>% mutate(temp_mean = mean,
#                                       temp_sd = sd)
# 
# saveRDS(dat_all, file = "data/derived_data/dat_all.rds")
dat_all = readRDS("data/derived_data/dat_all.rds") %>% 
  mutate(year = as.integer(year)) %>% 
  ungroup

# get raw means and sd to backtransform later
mean_sd = dat_all %>% distinct(site_id, log_om, log_gpp, temp_mean) %>% 
  pivot_longer(cols = -site_id) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(name = case_when(name == "log_gpp" ~ "log_gpp_s",
                          name == "log_om" ~ "log_om_s",
                          TRUE ~ "mat_s"))

# load models
fit_pareto = readRDS("models/fit_pareto.rds")


# plot conditionals -------------------------------------------------------

# extract posteriors
# lines
conds = conditional_effects(fit_pareto)

post_lines = as_tibble(conds$mat_s %>% mutate(name = "mat_s", x = mat_s)) %>% 
  bind_rows(as_tibble(conds$log_gpp_s %>% mutate(name = "log_gpp_s", x = log_gpp_s)),
            as_tibble(conds$log_om_s) %>% mutate(name = "log_om_s", x = log_om_s)) %>% 
  left_join(mean_sd) %>% 
  mutate(x_raw = (x*sd) + mean,
         x_raw = case_when(name == "mat_s" ~ x_raw,
                           TRUE ~ exp(x_raw)))
# samples
post_sample_lambdas = dat_all %>% 
  distinct(sample_id, .keep_all = T) %>%
  as_tibble() %>% 
  select(-dw) %>% 
  add_epred_draws(fit_pareto, re_formula = NULL)

post_sample_lambdas_summary = post_sample_lambdas %>%
  group_by(sample_id) %>% 
  median_qi(.epred) %>% 
  left_join(dat_all %>% ungroup %>% distinct(sample_id, mat_s, log_gpp_s,
                                             log_om_s, site_id)) %>% 
  pivot_longer(cols = c(mat_s, log_gpp_s, log_om_s), values_to = "x") %>% 
  left_join(mean_sd) %>% 
  mutate(x_raw = (x*sd) + mean,
         x_raw = case_when(name == "mat_s" ~ x_raw,
                           TRUE ~ exp(x_raw)))


# plot --------------------------------------------------------------------
plot_lambda_panels = function(pred = "mat_s"){
  post_lines %>% 
    filter(name == pred) %>% 
    ggplot(aes(x = x_raw, y = estimate__)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.5) +
    coord_cartesian(ylim = c(-2, -1)) + 
    geom_pointrange(data = post_sample_lambdas_summary %>% filter(name == pred), aes(y = .epred, 
                                                                                     ymin = .lower,
                                                                                     ymax = .upper),
                    size = 0.1, 
                    shape = "o") + 
    theme_default() + 
    labs(y = "\u03bb (ISD exponent)")
}

a = plot_lambda_panels(pred = "mat_s") + labs(x = "Mean Annual Temperature (\u00b0C)",
                                              subtitle = "a) Temperature")
b = plot_lambda_panels(pred = "log_gpp_s") + labs(x = expression(paste("GPP ",gC/m ^ 2/yr,"")),
                                                  subtitle = "b) Gross Primary Production") + 
  scale_x_log10()
c = plot_lambda_panels(pred = "log_om_s") + labs(x = expression(paste("OM ",gAFDM/m ^ 2,"")),
                                                 subtitle = "c) Allochthonous OM") + 
  scale_x_log10()

library(patchwork)
marginal_three_panel = a + b + c
ggview::ggview(marginal_three_panel,
               width = 7.5, height = 2.5, dpi = 400)

ggsave(marginal_three_panel, file = "plots/marginal_three_panel.jpg",
       width = 7.5, height = 2.5, dpi = 400)

# plot interaction --------------------------------------------------------
# extract posteriors
# lines
# 1) make quantiles
quantile_gpp = fit_pareto$data %>% distinct(log_gpp_s) %>%
  reframe(log_gpp_s = quantile(log_gpp_s, c(0.25, 0.5, 0.75))) 

quantile_om = fit_pareto$data %>% distinct(log_om_s) %>%
  reframe(log_om_s = quantile(log_om_s, c(0.25, 0.5, 0.75))) 

quantile_temp = fit_pareto$data %>% distinct(mat_s) %>%
  reframe(mat_s = quantile(mat_s, c(0.25, 0.5, 0.75))) 

cond_facets = tibble(gpp = c("gpp25", "gpp50", "gpp75")) %>% 
  expand_grid(om = c("om25", "om50", "om75")) %>% 
  unite(gpp, om, col = "cond__", sep = "_")

# 2) get fits
conditions = quantile_gpp %>% expand_grid(quantile_om) %>% 
  mutate(cond__ = cond_facets$cond__)

cond_interactions = conditional_effects(fit_pareto, effects = "mat_s", conditions = conditions)

cond_to_plot = as_tibble(cond_interactions$mat_s) %>% 
  separate(cond__, into = c("gpp", "om")) %>% 
  mutate(mat = (mat_s*6.14) + 10.8)

# 3) plot
interaction_plot = cond_to_plot %>% 
  ggplot(aes(x = mat, y = estimate__)) + 
  geom_line() +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.3) +
  facet_grid(om ~ gpp) + 
  ylim(-1.4, -1.1) + 
  theme_default() + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)") + 
  theme(text = element_text(size = 18))

ggview::ggview(interaction_plot, width = 6.5, height = 6.5)
ggsave(interaction_plot, width = 6.5, height = 6.5, dpi = 400, file = "plots/interaction_plot.jpg")


