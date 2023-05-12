library(tidyverse)
library(brms)
library(ggthemes)
library(patchwork)

### Plots with fish, inverts, and fish + inverts combined

# isd_by_temp -------------------------------------------------------------

isd_by_temp_fish = readRDS(file = "plots/isd_by_temp-fishonly.rds")
isd_by_temp_inverts = readRDS(file = "plots/isd_by_temp-invertsonly.rds")
isd_by_temp = readRDS(file = "plots/isd_by_temp.rds")

points_fish = layer_data(isd_by_temp_fish, 1) %>% mutate(animal_type = "fish")
points_inverts = layer_data(isd_by_temp_inverts, 1) %>% mutate(animal_type = "inverts")
points_all = layer_data(isd_by_temp, 1) %>% mutate(animal_type = "inverts + fish")

lines_fish = layer_data(isd_by_temp_fish, 2) %>% mutate(animal_type = "fish")
lines_inverts = layer_data(isd_by_temp_inverts, 2) %>% mutate(animal_type = "inverts")
lines_all = layer_data(isd_by_temp, 2) %>% mutate(animal_type = "inverts + fish")

ribbon_fish = layer_data(isd_by_temp_fish, 3) %>% mutate(animal_type = "fish")
ribbon_inverts = layer_data(isd_by_temp_inverts, 3) %>% mutate(animal_type = "inverts")
ribbon_all = layer_data(isd_by_temp, 3) %>% mutate(animal_type = "inverts + fish")

points = bind_rows(points_fish,
                   points_inverts,
                   points_all) %>% 
  mutate(panel = case_when(animal_type == "fish" ~ "c) Fish",
                           animal_type == "inverts" ~ "b) Invertebrates",
                           TRUE ~ "a) Inverts + Fish"))

lines = bind_rows(lines_fish,
                   lines_inverts,
                   lines_all) %>% 
  mutate(panel = case_when(animal_type == "fish" ~ "c) Fish",
                           animal_type == "inverts" ~ "b) Invertebrates",
                           TRUE ~ "a) Inverts + Fish"))

ribbons = bind_rows(ribbon_fish,
                    ribbon_inverts,
                    ribbon_all) %>% 
  mutate(panel = case_when(animal_type == "fish" ~ "c) Fish",
                           animal_type == "inverts" ~ "b) Invertebrates",
                           TRUE ~ "a) Inverts + Fish"))


# plot

# three column

(all_isd = ggplot(data = points, aes(x = x, y = y)) + 
  geom_pointrange(aes(ymin = ymin, ymax = ymax),
                  alpha = 0.2, size = 0.01,
                  linewidth = 0.1) + 
  geom_line(data = lines) + 
  geom_ribbon(data = ribbons, aes(ymin = ymin, ymax = ymax), alpha = 0.2) + 
  scale_color_colorblind() + 
  scale_fill_colorblind() + 
  facet_wrap(~panel, ncol = 3) +
  theme_default() + 
    theme(strip.text = element_text(hjust = 0)) +
  guides(color = "none",
         fill = "none") +
    coord_cartesian(ylim = c(-2, -0.4)) +
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)")
)

saveRDS(all_isd, file = "plots/all_isd.rds")
ggview(all_isd, width = 6.5, height = 2)
ggsave(all_isd, file = "plots/all_isd.jpg",dpi = 500,
       width = 6.5, height = 2)
ggsave(all_isd + facet_wrap(~panel, ncol = 3), file = "plots/all_isd_samescale.jpg",dpi = 500,
       width = 6.5, height = 2)


# total community only
total_community_isd = ggplot(data = points  %>% filter(animal_type == "inverts + fish"), aes(x = x, y = y)) + 
  geom_pointrange(aes(color = animal_type, ymin = ymin, ymax = ymax)) + 
  geom_line(data = lines  %>% filter(animal_type == "inverts + fish"), aes(color = animal_type)) + 
  geom_ribbon(data = ribbons  %>% filter(animal_type == "inverts + fish"), 
              aes(ymin = ymin, ymax = ymax, fill = animal_type), alpha = 0.2) + 
  scale_color_colorblind() + 
  scale_fill_colorblind() + 
  facet_wrap(~animal_type) +
  theme_default() + 
  guides(color = "none",
         fill = "none") +
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)")

# inverts and fish separately
inverts_fish_separate_isd = ggplot(data = points %>% filter(animal_type != "inverts + fish"), aes(x = x, y = y)) + 
  geom_pointrange(aes(color = animal_type, ymin = ymin, ymax = ymax), 
                  size = 0.2) + 
  geom_line(data = lines %>% filter(animal_type != "inverts + fish"), aes(color = animal_type)) + 
  geom_ribbon(data = ribbons %>% filter(animal_type != "inverts + fish"), aes(ymin = ymin, ymax = ymax, fill = animal_type), alpha = 0.2) + 
  scale_color_colorblind() + 
  scale_fill_colorblind() + 
  facet_wrap(~animal_type, ncol = 1) +
  theme_default() + 
  guides(color = "none",
         fill = "none") +
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)")


total_community_isd + inverts_fish_separate_isd


# isd tempxomxgpp ---------------------------------------------------------

isd_temp_by_gpp_fish = readRDS(file = "plots/isd_temp_by_gpp_om-fishonly.rds")
isd_temp_by_gpp_inverts = readRDS(file = "plots/isd_temp_by_gpp_om-invertsonly.rds")

plot_isd_fish_inverts_interaction = bind_rows(isd_temp_by_gpp_fish$data %>% mutate(animal_type = "Fish"),
          isd_temp_by_gpp_inverts$data %>% mutate(animal_type = "Invertebrates")) %>% 
  ggplot(aes(x = raw_temp, y = lambda)) + 
  geom_line(aes(color = animal_type)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, fill = animal_type), alpha = 0.2) + 
  scale_color_colorblind() + 
  scale_fill_colorblind() +
  facet_grid(quantile_gpp~quantile_om) +
  theme_default() +
  theme(legend.title = element_blank()) + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)") +
  NULL

ggsave(plot_isd_fish_inverts_interaction, file = "plots/plot_isd_fish_inverts_om-fishonly_interaction.jpg", width = 7, height = 6, units = "in")
saveRDS(plot_isd_fish_inverts_interaction, file = "plots/plot_isd_fish_inverts_om-fishonly_interaction.rds")





# simulate data -----------------------------------------------------------

dat_invert = readRDS(file = "data/derived_data/dat_invert.rds") %>% mutate(animal_type = "inverts")
dat_fish = readRDS(file = "data/derived_data/dat_fish.rds") %>% mutate(animal_type = "fish")
dat_all = bind_rows(dat_invert, dat_fish) 

n_samples = 1000
sims = dat_all %>% 
  group_by(sample_int) %>% 
  sample_n(n_samples, weight = no_m2, replace = T) %>% 
  arrange(-dw) %>% 
  mutate(rank_order = 1:n_samples)


set.seed(23222)
sample_ints = as.integer(runif(3, 1,max(sims$sample_int)))

sims %>% 
  filter(sample_int %in% sample_ints) %>% 
  ggplot(aes(x = dw, y = rank_order, color = animal_type)) + 
  geom_point(shape = 21, aes(size = dw)) + 
  scale_x_log10() + 
  scale_y_log10() +
  scale_color_colorblind() +
  facet_wrap(~sample_int) +
  theme_default() +
  NULL
 

pred_rank = 3 
sims %>% 
  filter(sample_int == 85) %>% 
  mutate(focal_x = unique(dw)[pred_rank],
         prey_focal_low = focal_x/1e3,
         prey_focal_high = focal_x/1e1,
         prey_for_focal_x = case_when(dw >= prey_focal_low &
                                     dw <= prey_focal_high ~ "PPMR",
                                     dw == focal_x ~ "PPMR",
                                   TRUE ~ "Not PPMR"),
         start_line = case_when(prey_for_focal_x == "PPMR" ~ dw),
         start_order = case_when(prey_for_focal_x == "PPMR" ~ rank_order),
         y_end = pred_rank) %>% 
  ggplot(aes(x = dw, y = rank_order, color = prey_for_focal_x)) + 
  geom_point(shape = 21, aes(size = dw)) + 
  geom_segment(aes(x = start_line, xend = focal_x,
                   y = start_order, yend = pred_rank),
               alpha = 0.2, linewidth = 0.01) + 
  scale_x_log10() +
  scale_y_log10() +
  # scale_alpha_manual(values = c(0.006, 1)) +
  scale_color_grey(start = 0.8, end = 0.2) +
  facet_wrap(~sample_int) +
  guides(size = "none") +
  theme_default() +
  theme(legend.title = element_blank()) +
  NULL


# isd and biomass ---------------------------------------------------------
# Fix quantiles
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

lowgpp <-  expression(paste(GPP[0.25]))
medgpp <- expression(paste(GPP[0.5]))
highgpp <- expression(paste(GPP[0.75]))
lowOM <- expression(paste(OM[0.25]))
medOM <- expression(paste(OM[0.5]))
highOM <- expression(paste(OM[0.75]))

variable_gpp = c(lowgpp, medgpp, highgpp) 
                    
variable_om = c(lowOM, medOM, highOM)

facet_gpp = tibble(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"),
                      facet_gpp = gl(3,1, labels = variable_gpp))

facet_om = tibble(quantile_om = c("Low OM", "Median OM", "High OM"),
                  facet_om = gl(3, 1, labels = variable_om))

saveRDS(facet_gpp, file = "plots/facet_gpp.rds")
saveRDS(facet_om, file = "plots/facet_om.rds")

# 1) load models or plots
community_mass_brm = readRDS("models/community_mass_brm.rds")
fishinvertmod = readRDS("models/stan_gppxtempxom2023-04-27.rds")
posts_mass = readRDS(file = "models/posteriors/posts_mass.rds") %>% left_join(facet_gpp) %>% left_join(facet_om)
post_lines = readRDS(file = "models/posteriors/post_lines.rds") %>% left_join(facet_gpp) %>% left_join(facet_om)
a_heat = readRDS(file = "plots/a_heat.rds")
b_heat = readRDS(file = "plots/b_heat.rds")

# 2) get coefs
coefs_isd_fishinvert = tidy_draws(fishinvertmod) %>% 
  select(starts_with("beta_")) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  # median_qi(value) %>% 
  mutate(coefficient = str_sub(name, 6, 20),
         coefficient = str_replace(coefficient, "_", ":"),
         coefficient = str_replace(coefficient, "_", ":"),
         coefficient = paste0("\u03b2", coefficient),
         order = str_length(coefficient),
         model = "ISD")

coefs_mass = tidy_draws(community_mass_brm) %>% 
  select(starts_with("b_")) %>% 
  pivot_longer(everything()) %>% 
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
         order = str_length(coefficient),
         model = "Standing Stock Biomass")

# 3) make coef plots
coefs_mass_plot = coefs_mass %>% 
  ggplot(aes(y = value, x = reorder(coefficient, -order))) + 
  stat_pointinterval() +
  coord_flip() +
  geom_hline(aes(yintercept = 0)) + 
  theme_default() +
  labs(x = "Model coefficients",
       y = "Value")

coefs_isd_plot =  coefs_isd_fishinvert %>% 
  ggplot(aes(y = value, x = reorder(coefficient, -order))) + 
  stat_pointinterval() +
  coord_flip(ylim = c(-0.1, 0.1)) +
  geom_hline(aes(yintercept = 0)) + 
  theme_default() +
  labs(x = "Model coefficients",
       y = "Value")

coefs_mass_isd = coefs_isd_plot + coefs_mass_plot + labs(x = "")


# 4) Get isd and mass regression plots
isd_by_temp = readRDS(file = "plots/isd_by_temp.rds")

community_mass_univariate_plot = readRDS(file = "plots/community_mass_univariate_plot.rds") +
  labs(y = bquote('ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Temperature (\u00b0C)")

# isd counterfactual plots
(isd_temp_by_gpp = post_lines %>% 
    ggplot(aes(x = raw_temp, y = lambda, fill = quantile_om)) + 
    geom_line(aes(group = as.factor(log_gpp_s), color = quantile_om)) + 
    geom_ribbon(aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
    facet_grid(facet_gpp ~ facet_om, labeller = "label_parsed") +
    theme_default() + 
    scale_color_colorblind() + 
    scale_fill_colorblind() +
    scale_x_continuous(breaks = c(4, 12, 20)) +
    scale_y_continuous(breaks = c(-1.5,-1.25, -1),
                       limits = c(-1.5, -1)) +
    guides(fill = "none",
           color = "none") + 
    labs(y = "\u03bb (ISD exponent)",
         x = "Mean Annual Temperature (\u00b0C)") 
  )

mean_mass = community_mass %>% ungroup %>% distinct(site_id, log_total_g) %>% summarize(mean_mass = mean(log_total_g))

community_mass_facet_plot = posts_mass %>% 
  # filter(quantile_om == "Median OM") %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP")) %>% 
  group_by(mat_s, log_om_s, log_gpp_s, quantile_om, quantile_gpp, mat, facet_om, facet_gpp) %>% 
  mutate(.epred = .epred*unique(community_mass$mean_log_total_g)) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = mat, y = .epred, fill = quantile_om)) +
  geom_line(aes(color = quantile_om))  +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) +
  facet_grid(facet_gpp ~ facet_om, labeller = "label_parsed") +
  # scale_y_log10() + 
  theme_default() + 
  scale_color_colorblind() + 
  scale_fill_colorblind() +
  guides(fill = "none",
         color = "none") +
  labs(y = bquote('ln('*gDM/m^'2'*")"), 
       x = "Mean Annual Temperature (\u00b0C)") +
  scale_x_continuous(breaks = c(4, 12, 20)) +
  scale_y_continuous(breaks = c(0, 3, 6)) +
  NULL


# 5) Combine plots
library(patchwork)
a = isd_by_temp + labs(subtitle = "a)")
b = community_mass_univariate_plot + labs(subtitle = "b)")
c = isd_temp_by_gpp + labs(subtitle = "c)")
d = community_mass_facet_plot + labs(subtitle = "d)")
e = a_heat + labs(subtitle = "e)") + theme(legend.position = "top")
f = b_heat + labs(subtitle = "f)") + theme(legend.position = "top")

isd_mass_fourpanel = (a + b)/(c + d)
ggview::ggview(isd_mass_fourpanel, width = 6.5, height = 6.5, units = "in")
saveRDS(isd_mass_fourpanel, file = "plots/ms_plots/isd_mass_fourpanel.rds")
ggsave(isd_mass_fourpanel, width = 6.5, height = 6.5, units = "in",
       file = "plots/ms_plots/isd_mass_fourpanel.jpg", dpi = 500)


