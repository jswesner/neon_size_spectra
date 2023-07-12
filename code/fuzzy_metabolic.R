library(tidyverse)
library(ggridges)
library(ggthemes)
library(brms)
library(tidybayes)
source("code/sandbox/paretocounts.R")
theme_set(theme_default())

# total biomass -----------------------------------------------------------


E = -0.65 # From West, Brown, Enquist - "WBE", units are eV (electron volts)
k = 8.62*10^-5 # Boltzmann's constant
temp_c = 15
temp_k = temp_c + 273.15   # convert celsius to kelvin by adding 273.15 to the temp in celsius
M = 2 # mean body size (I think, via Brown et al. 2004)
r0 = 100 # resource supply rate (perhaps like GPP g/c/m2/y?)

theme_set(brms::theme_default())

Btot = r0*exp(E/(k*temp_k))*M^0.25

temps = c(1, 10, 20, 30)
M = c(0.01, 0.1, 1, 10)
r0 = c(0.1, 1, 10, 100, 1000)

tibble(E = -0.65, # From West, Brown, Enquist - "WBE", units are eV (electron volts)
       k = 8.62*10^-5, # Boltzmann's constant
       temp_c = 15,
       temp_k = temp_c + 273.15,   # convert celsius to kelvin by adding 273.15 to the temp in celsius
       M = 2, # mean body size (I think, via Brown et al. 2004)
       r0 = 100)



btot_sims = tibble(E = -0.65, 
       k = 8.62*10^-5) %>% 
  expand_grid(temp_c = temps,
              M = M,
              r0 = r0) %>% 
  mutate(temp_k = temp_c + 273.15,
         Btot = r0*exp(E/(k*temp_k))*(M^0.25),
         inverse_temp = 1/(k*temp_k))
       

btot_sims %>% 
  ggplot(aes(x = inverse_temp, y = Btot)) + 
  geom_point() +
  geom_line(aes(group = M)) +
  facet_wrap(~r0) +
  scale_y_log10()

dat_all = readRDS("data/derived_data/dat_all.rds")

mte_mass_preds = dat_all %>% 
  group_by(site_id, temp_mean, gpp, mean_om) %>% 
  reframe(M = exp(mean(log(dw)))) %>% 
  mutate(E = -0.65, 
         k = 8.62*10^-5,
         temp_c = temp_mean,
         r0 = gpp + mean_om) %>% 
  expand_grid(rep = 1:100) %>% 
  mutate(scaling_r = rnorm(nrow(.), 0.25, 0.01),
         E_r = rnorm(nrow(.), -0.65, 0.05)) %>% 
  mutate(temp_k = temp_c + 273.15,
         Btot_r = r0*exp(E_r/(k*temp_k))*(M^scaling_r),
         Btot = r0*exp(E/(k*temp_k))*(M^0.25),
         inverse_temp = 1/(k*temp_k))


mte_mass_preds %>% 
  ggplot(aes(x = site_id, y = Btot*1e9)) + 
  geom_point() +
  scale_y_log10()

mte_mass_preds %>% 
  # filter(rep == 50) %>%
  pivot_longer(cols = c(Btot_r, Btot), names_to = "method", values_to = "Btot") %>% 
  ggplot(aes(x = inverse_temp, y = Btot*1e9, color = method)) + 
  geom_point() +
  scale_y_log10() +
  # theme_default() +
  # theme(axis.text = element_blank(),
        # axis.title = element_blank()) +
  facet_wrap(~method) +
  NULL



# isd ---------------------------------------------------------------------
n = 5000
lambda_sims = tibble(a = rbeta(n, 10, 50),
       b = rlnorm(n, log(10^5), 3), # from Gjoni unpublished 2023 experiment; Wesner museum data (for fish - 10^5 to 10^6) + Mehner (10^4) + Perkins et al. 2018 (10^2 to 10^3) + Brose et al. 2019 (> 10^6)
       scaling_75 = rlnorm(n, log(0.75), 0.2), # from Brown et al. with variance mimicking Glazier et al. 2005
       scaling_35 = rlnorm(n, log(0.38), 0.07), # from Gjoni unpublished 2023 experiment. Values are low, but are within the range reported for broad taxa (Glazier et al. 2022)
       loga = log10(a),
       logb = log10(b),
       loga_logb = loga/logb,
       subsidy_effect = rlnorm(n, log(0.35), 0.2)) %>% # from Perkins et al. 2018 Fig 3a and Hocking (0.2-0.3 effect)
  # filter(a > 0) %>% 
  mutate(lambda_nosubsidies_75 = (loga/logb) - scaling_75 - 1,
         lambda_subsidies_75 = lambda_nosubsidies_75 + subsidy_effect,
         lambda_nosubsidies_35 = (loga/logb) - scaling_35 - 1,
         lambda_subsidies_35 = lambda_nosubsidies_35 + subsidy_effect) %>% 
  pivot_longer(cols = contains("lambda")) %>% 
  group_by(name) %>% 
  mutate(median = median(value)) %>% 
  separate(name, into = c("measure", "subsidy", "scaling"), remove = F) %>% 
  mutate(scaling = case_when(scaling == 75 ~ "0.75 metabolic scaling",
                              TRUE ~ "0.5 metabolic scaling"),
         source = name,
         model = "Simulation")

# add to NEON isd ---------------------------------------------------------
# get raw means and sd to backtransform later
mean_sd = readRDS("data/derived_data/dat_all.rds") %>% 
  mutate(year = as.integer(year)) %>% 
  ungroup %>% distinct(site_id, log_om, log_gpp, temp_mean) %>% 
  pivot_longer(cols = -site_id) %>% 
  group_by(name) %>% 
  summarize(mean = mean(value),
            sd = sd(value)) %>% 
  mutate(name = case_when(name == "log_gpp" ~ "log_gpp_s",
                          name == "log_om" ~ "log_om_s",
                          TRUE ~ "mat_s"))

# load models
fit_pareto = readRDS("models/fit_pareto.rds") # NEON model
mesocosm_isd_2022 = readRDS("models/brm_fit_2022_mesocosms.rds") # 2022 mesocosm model

# get posteriors
neon_conds = conditional_effects(fit_pareto, effects = "mat_s")
neon_lines = neon_conds$mat_s %>% mutate(temp_mean = mean_sd[[3,2]],
                                         temp_sd = mean_sd[[3,3]]) %>% 
  mutate(x_raw = (mat_s * temp_sd) + temp_mean)

neon_intercept = as_draws_df(fit_pareto) %>% 
  mutate(source = "NEON (this study)",
         model = "Empirical",
         value = b_Intercept)

post_sample_lambdas = fit_pareto$data %>% 
  distinct(sample_id, year, site_id, mat_s, log_gpp_s, log_om_s, xmin, xmax) %>% 
  mutate(no_m2 = 1) %>%  # placeholder
  add_epred_draws(fit_pareto, re_formula = NULL) %>% 
  mutate(source = "NEON (this study)",
         model = "Empirical")
  
posts_2022 = as_draws_df(mesocosm_isd_2022) %>% 
  mutate(source = "Stream Mesocosms (2022)",
             model = "Empirical",
         value = b_Intercept) # 2022 mesocosm posteriors

posts_2022_tanks = mesocosm_isd_2022$data %>% 
  distinct(xmin, xmax, gpp_max_s, mean_temp_s, sample) %>% 
  mutate(counts = 1) %>% 
  add_epred_draws(mesocosm_isd_2022) %>% 
  mutate(source = "Stream Mesocosms (2022)",
         model = "Empirical") # 2022 mesocosm posteriors

# plot

simulation_plot = lambda_sims %>% 
  mutate(name = case_when(name == "lambda_nosubsidies_75" ~ "Subsidies absent,\nExponent = 0.75",
                          name == "lambda_nosubsidies_35" ~ "Subsidies absent,\nExponent = 0.4",
                          name == "lambda_subsidies_75" ~ "Subsidies present,\nExponent = 0.75",
                          TRUE ~ "Subsidies present,\nExponent = 0.4")) %>% 
  ggplot(aes(x = value)) + 
  stat_binline(aes(y = reorder(name, -median)), bins = 100,
               scale = 1) +
  coord_cartesian(clip = "off") +
  geom_segment(data = bind_rows(posts_2022_tanks %>% 
                                bind_rows(post_sample_lambdas %>% 
                                            mutate(sample = sample_id)) %>% 
                                group_by(sample, source, model) %>%
                                median_qi(.epred)),
             aes(x = .epred, 
                 xend = .epred,
                 y = 0.5,
                 yend = .7,
                 color = source), alpha = 1,
             linewidth = 0.08) +
  scale_color_colorblind() +
  scale_x_continuous(limits = c(-2.5, -0.5)) +
  theme(axis.title.y = element_blank(),
        legend.position = "top",
        # text = element_text(size = 11)
        ) +
  labs(color = "",
       x = "\u03bb (ISD exponent)",
       subtitle = "e)") +
  guides(color = "none") +
  annotate(geom = "text", y = .6, x = -2, label = "Empirical estimates",
           size = 3) +
  annotate(geom = "text", y = 3.2, x = -1, label = "Simulations",
           size = 3) +
  geom_segment(aes(x = -1.1, y = 3.2, xend = -1.3, yend = 3.2),
               arrow = arrow(type = "closed",
                             length=unit(1, "mm")),
               lwd = 0.2) +
  geom_segment(aes(x = -1.7, y = 0.6, xend = -1.55, yend = 0.6),
               arrow = arrow(type = "closed",
                             length=unit(1, "mm")),
               lwd = 0.2) +
  NULL



ggview::ggview(simulation_plot, width = 6, height = 3.5)
ggsave(simulation_plot, width = 6, height = 3.5, units = "in", dpi = 500,
       file = "plots/ms_plots/simulation_plot.jpg")



simulation_three = lambda_sims %>% 
  filter(name != "lambda_nosubsidies_35") %>% 
  mutate(name = case_when(name == "lambda_nosubsidies_35" ~ "Subsidies absent,\nExponent = 0.4",
                          name == "lambda_nosubsidies_75" ~ "Subsidies absent,\nExponent = 0.75",
                          name == "lambda_subsidies_75" ~ "Subsidies present,\nExponent = 0.75",
                          TRUE ~ "Subsidies present,\nExponent = 0.4")) %>%
  ggplot(aes(x = value)) + 
  stat_binline(aes(y = reorder(name, -median)), bins = 100,
               scale = 1) +
  coord_cartesian(clip = "off") +
  geom_segment(data = bind_rows(posts_2022_tanks %>% 
                                  bind_rows(post_sample_lambdas %>% 
                                              mutate(sample = sample_id)) %>% 
                                  group_by(sample, source, model) %>%
                                  median_qi(.epred)),
               aes(x = .epred, 
                   xend = .epred,
                   y = 0.5,
                   yend = .7,
                   color = source), alpha = 1,
               linewidth = 0.08) +
  scale_color_colorblind() +
  scale_x_continuous(limits = c(-2.5, -0.5)) +
  theme(legend.position = "top",
        # text = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        # text = element_text(size = 11)
  ) +
  labs(color = "",
       x = "\u03bb (ISD exponent)",
       subtitle = "e)") +
  guides(color = "none") +
  # guides(color = "none") +
  # annotate(geom = "text", y = .6, x = -2, label = "Empirical estimates",
  #          size = 3) +
  # annotate(geom = "text", y = 3.2, x = -1, label = "Simulations",
  #          size = 3) +
  # geom_segment(aes(x = -1.1, y = 3.2, xend = -1.3, yend = 3.2),
  #              arrow = arrow(type = "closed",
  #                            length=unit(1, "mm")),
  #              lwd = 0.2) +
  # geom_segment(aes(x = -1.7, y = 0.6, xend = -1.55, yend = 0.6),
  #              arrow = arrow(type = "closed",
  #                            length=unit(1, "mm")),
  #              lwd = 0.2) +
  NULL


ggsave(simulation_three, file = "plots/simulation_three.jpg", width = 5, height = 5)



# metabolic plot ----------------------------------------------------------

brm_metab <- readRDS("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/NEON-2023-mesocosm-experiment/models/brm_metab.rds")
brm_metab_taxon <- read_csv("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/NEON-2023-mesocosm-experiment/data/metabolism.csv") %>% 
  mutate(treatment = case_when(treatment == "no heated_fish" ~ "b) ambient, +fish",
                               treatment == "no heated_no fish" ~ "a) ambient, -fish",
                               treatment == "heated_fish" ~ "d) heated, +fish",
                               TRUE ~ "c) heated, -fish"))
  
metab_lines = brm_metab$data %>% 
  distinct(fish, heat) %>% 
  expand_grid(log_dw_c = seq(min(brm_metab$data$log_dw_c),
                           max(brm_metab$data$log_dw_c),
                           length.out = 20)) %>% 
  add_epred_draws(brm_metab, re_formula = NA) %>% 
  group_by(log_dw_c, fish, heat) %>% 
  median_qi(.epred) %>% 
  mutate(treatment = paste0(heat, "_", fish)) %>% 
  mutate(treatment = case_when(treatment == "no heated_fish" ~ "b) ambient, +fish",
                               treatment == "no heated_no fish" ~ "a) ambient, -fish",
                               treatment == "heated_fish" ~ "d) heated, +fish",
                               TRUE ~ "c) heated, -fish"))

metab_values = brm_metab$data %>% 
  distinct(fish, heat) %>% 
  expand_grid(log_dw_c = c(0, 1)) %>% 
  add_epred_draws(brm_metab, re_formula = NA) %>% 
  ungroup %>% 
  select(-.row, -.chain, -.iteration) %>% 
  pivot_wider(names_from = log_dw_c, values_from = .epred) %>% 
  mutate(slope = `1`-`0`) %>%
  mutate(treatment = paste0(heat, "_", fish)) %>% 
  mutate(treatment = case_when(treatment == "no heated_fish" ~ "b) ambient, +fish",
                               treatment == "no heated_no fish" ~ "a) ambient, -fish",
                               treatment == "heated_fish" ~ "d) heated, +fish",
                               TRUE ~ "c) heated, -fish")) %>% 
  group_by(treatment) %>% 
  reframe(mean = round(mean(slope),1),
          sd = round(sd(slope),2)) %>% 
  mutate(log_dw_c = -1, .epred = 1.7,
         label = paste("slope = ", mean, "\u00b1", sd))
  
ref_label = tibble(treatment = metab_lines %>% distinct(treatment) %>%
                     filter(treatment == "a) ambient, -fish") %>% pull) %>% 
  mutate(log_dw_c = 3.2,
         .epred = 2.3,
         label = "0.75")

mean_log_dw = mean(brm_metab_taxon$log_dw)
mean_resp = mean(brm_metab_taxon$log_resp)

metab_regression = metab_lines %>% 
  ggplot(aes(x = log_dw_c + mean_log_dw, y = .epred + mean_resp)) +
  geom_line(aes(group = treatment)) +
  geom_ribbon(aes(ymin = .lower + mean_resp, ymax = .upper + mean_resp,
                  fill = treatment),alpha = 0.2) +
  geom_point(data = brm_metab_taxon %>% 
               mutate(taxon = str_to_sentence(taxon)),
             aes(y = log_resp, color = treatment), size = 0.2) +
  scale_color_viridis_d() +
  scale_fill_viridis_d() +
  labs(y = "log10 (Metabolic Rate)",
       x = "log10 (Individual Dry Mass)",
       shape = "Taxon",
       color = "Treatment",
       fill = "Treatment") +
  guides(fill = "none",
         color = "none") +
  geom_abline(slope = 0.75) +
  theme(strip.text = element_text(angle = 0, hjust = 0)) +
  geom_text(data = metab_values, aes(label = label),
            size = 2) +
  geom_label(data = ref_label, aes(label = label),
            size = 2) +
  facet_wrap(~treatment, nrow = 1)

metab_regression

library(patchwork)
library(cowplot)
two_panel_simulations = plot_grid(metab_regression, simulation_plot,
          ncol = 1, rel_heights = c(0.25, 0.7))

ggview::ggview(two_panel_simulations, width = 6.5, height = 8)
ggsave(two_panel_simulations, width = 6.5, height = 8,
       file = "plots/ms_plots/two_panel_simulations.jpg", units = "in",
       dpi = 500)



metab_plot_univariate = plot(conditional_effects(brm_metab, effects = "log_dw_c"))

metab_plot_univariate$log_dw_c$data %>% 
  ggplot(aes(x = log_dw_c)) + 
  geom_lineribbon(aes(y = estimate__, ymin = lower__, ymax = upper__), alpha = 0.5) +
  geom_point(data = brm_metab$data, aes(y = log_resp_c))




