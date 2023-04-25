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
