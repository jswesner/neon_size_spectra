library(brms)
library(tidyverse)
library(tidybayes)
library(janitor)
library(ggridges)
library(sizeSpectra)
library(scales)
library(ggdark)
library(ggthemes)

# mle estimates from January Grand Junction meeting. Includes fish and macroinvertebrates
mle_output <- readRDS("results/mle_b_estimate_fish_macro.RDS")

# load temp estimates mean annual water temperatures
mat_posts <- readRDS("code/temperature/posteriors/mat_posts.rds") %>% clean_names

# data to analyze mle as a function of temperature
mle_mat <- mle_output %>% left_join(mat_posts)



# Fit model ---------------------------------------------------------------

brm_mle_sfs <- brm(b|mi(stdErr) ~ me(mat_site, sdat_site) + (1|site_id),
               data = mle_mat,
               family = gaussian(),
               file = "code/mle/results/brm_mle_sfs.rds", 
               file_refit = "on_change")




# check model
pp_check(brm_mle_sfs)

# conditional plot
plot(conditional_effects(brm_mle_sfs, re_formula = NA), points = T)

# extract posteriors and summarize slope
posts <- as_draws_df(brm_mle_sfs) %>% as_tibble()

posts %>% 
  median_qi(bsp_memat_sitesdat_site)

posts %>% 
  summarize(prob_negative = sum(bsp_memat_sitesdat_site <0)/nrow(.))


# site specific predictions
post_preds <- brm_mle_sfs$data %>% 
  distinct(site_id, mat_site, stdErr, sdat_site) %>% 
  add_predicted_draws(brm_mle_sfs, re_formula = NULL)

# plot ridges
library(viridis)
ridge_b_plot_light <- post_preds %>% 
  ggplot(aes(x = .prediction, fill = mat_site, y = reorder(site_id, mat_site))) + 
  geom_point(data = brm_mle_sfs$data, aes(x = b), shape = "|", color = "black") +
  geom_density_ridges(alpha = 0.7) + 
  scale_fill_viridis(option = "B") + 
  theme_default() + 
  labs(fill = "Temp (C)",
       y = "Site",
       x = expression(paste(italic("b"), "- Size Spectrum Exponent"))) + 
  theme(legend.position = "top",
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 24))
  
ridge_b_plot_dark <- post_preds %>% 
  ggplot(aes(x = .prediction, fill = mat_site, y = reorder(site_id, mat_site))) + 
  geom_point(data = brm_mle_sfs$data, aes(x = b), shape = "|", color = "black") +
  geom_density_ridges(alpha = 0.7) + 
  scale_fill_viridis(option = "B") + 
  dark_theme_classic() + 
  labs(fill = "Temp (C)",
       y = "Site",
       x = expression(paste(italic("b"), "- Size Spectrum Exponent"))) + 
  theme(legend.position = "top",
        axis.title = element_text(size = 30),
        axis.text = element_text(size = 24),
        text = element_text(family = "serif"))

ggsave(ridge_b_plot_light, file = "plots/ridge_b_plot_light.jpg", dpi = 400, width = 7, height = 11)
ggsave(ridge_b_plot_dark, file = "plots/ridge_b_plot_dark.jpg", dpi = 400, width = 7, height = 11)



# plot regression

# conditional plot
cond_data <- conditional_effects(brm_mle_sfs, re_formula = NA)

range(brm_mle_sfs$data$b)

cond_data$mat_site %>% 
  ggplot(aes(x = mat_site, y = estimate__)) + 
  geom_line() + 
  geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.2) + 
  geom_pointrange(data = brm_mle_sfs$data, 
                  aes(y = b, ymin = b - 2*stdErr, ymax = b + 2*stdErr),
                  size = 0.2, shape = 21, position = position_jitter(height = 0, width = 0.1)) + 
  theme_default() + 
  ylim(NA, -1)

# conditional plot spaghetti
post_preds <- posts %>% select(b_Intercept, bsp_memat_sitesdat_site, .draw) %>% 
  expand_grid(mat_site = seq(min(brm_mle_sfs$data$mat_site), max(brm_mle_sfs$data$mat_site), length.out = 30)) %>% 
  mutate(b = b_Intercept + bsp_memat_sitesdat_site*mat_site)
  
library(viridis)

regression_plot <- post_preds %>% 
  filter(.draw <= 500) %>% 
  ggplot(aes(x = mat_site, y = b)) + 
  geom_pointrange(data = brm_mle_sfs$data,
                  aes(fill = mat_site, ymin = b - 2*stdErr, ymax = b + 2*stdErr),
                  size = 0.7, shape = 21) + 
  geom_line(aes(group = .draw), alpha = 0.1, color = "black") + 
  ylim(NA, -1) +
  guides(fill = "none") +
  scale_fill_viridis(option = "B") +
  theme_default() + 
  labs(y = expression(paste(italic("b"), "- Size Spectrum Exponent")),
       x = "Mean Annual Temp (deg C)") + 
  theme(text = element_text(size = 38))

regression_plot_dark <- post_preds %>% 
  filter(.draw <= 500) %>% 
  ggplot(aes(x = mat_site, y = b)) + 
  geom_pointrange(data = brm_mle_sfs$data,
                  aes(fill = mat_site, ymin = b - 2*stdErr, ymax = b + 2*stdErr),
                  size = 0.7, shape = 21) + 
  geom_line(aes(group = .draw), alpha = 0.1) + 
  ylim(NA, -1) +
  guides(fill = "none") +
  dark_theme_classic() +
  scale_fill_viridis(option = "B") +
  labs(y = expression(paste(italic("b"), "- Size Spectrum Exponent")),
       x = "Mean Annual Temp (deg C)") + 
  theme(text = element_text(size = 38, family = "serif"))

ggsave(regression_plot, file = "plots/regression_plot.jpg", dpi = 400, width = 10, height = 9)
ggsave(regression_plot_dark, file = "plots/regression_plot_dark.jpg", dpi = 400, width = 10, height = 9)


# Generate data to plot via Edwards ------------------------------------------
#load raw data (fish + macros)
dat <- readRDS("data/derived_data/macro_fish_dw.rds") %>%
  mutate(Year = ID,
         Number = no_m2,
         bodyMass = dw) # different names that work with sizeSpectra package

# join with posteriors
dat_to_plot <- dat %>% ungroup() %>% 
  select(site_id, year_month, animal_type, bodyMass, Number) %>% 
  left_join(mle_mat) %>% 
  group_by(site_id, year_month) %>% 
  arrange(site_id, year_month, bodyMass) %>% 
  mutate(group = paste(site_id, year_month, sep = "_")) 

# get counts of organisms and cumulative counts
dat_MLE_counts = dat_to_plot %>% 
  select(bodyMass, Number, group) %>% ungroup() %>% 
  group_by(bodyMass, group) %>% 
  summarize(Count = sum(Number)) %>% 
  arrange(group, desc(bodyMass)) %>% 
  group_by(group) %>% 
  mutate(cumSum = cumsum(Count),
         cumProp = cumSum / sum(Count),
         length = ceiling(sum(Count))) 

# generate sequence of values to simulate over
dat_MLE_sim <- dat_MLE_counts %>% 
  group_by(group, length) %>% 
  summarize(min_cumProp = min(cumProp)) %>% 
  group_by(group) %>% 
  do(tibble(cumPropsim = seq(.$min_cumProp, 1, length = .$length/1))) # dividing by something reduces file size by limiting iterations, but check for accuracy

# Simulate cumulative proportion data to plot against MLE estimates
# make lists
dat_MLE_simlist <- dat_MLE_sim %>% group_by(group) %>% group_split() 
dat_MLE_countslist <- dat_MLE_counts %>% group_by(group) %>% group_split() 
mle_sim = list() # empty list to population

# simulate with for loop
for(i in 1:length(dat_MLE_simlist)){
  mle_sim[[i]] = dat_MLE_simlist[[i]] %>% as_tibble %>% 
    mutate(bodyMasssim = dat_MLE_countslist[[i]][findInterval(dat_MLE_simlist[[i]]$cumPropsim,
                                                 dat_MLE_countslist[[i]]$cumProp), ]$bodyMass)
}

# data to plot
mle_sim_tibble <- bind_rows(mle_sim) # dots to plot...very large file
rm(mle_sim)

saveRDS(mle_sim_tibble, file = "code/mle/data/mle_sim_tibble.rds")

mle_sim_tibble <- readRDS(file = "code/mle/data/mle_sim_tibble.rds")

# Generate MLE regression line to plot via Edwards ------------------------------------------
# get values to generate sequence to predict over
sim_PLBlist <- dat_to_plot %>% group_by(group, b, confMin, confMax, mat_site, sdat_site) %>% 
  summarize(xmin = min(bodyMass),
            xmax = max(bodyMass)) %>% 
  rename(Mean = b) %>% 
  pivot_longer(cols = c(Mean, confMin, confMax), values_to = "b") %>%
  group_by(group, b, mat_site, sdat_site, name, xmin, xmax) %>%  
  group_split()

x_PLB <- list()

# generate list of x-values to predict over
for(i in 1:length(sim_PLBlist)){
  x_PLB[[i]] = tibble(x_PLB = seq(sim_PLBlist[[i]]$xmin, sim_PLBlist[[i]]$xmax, length = 1000)) %>% 
    mutate(group = sim_PLBlist[[i]]$group,
           xmin = sim_PLBlist[[i]]$xmin,
           xmax = sim_PLBlist[[i]]$xmax,
           b = sim_PLBlist[[i]]$b,
           mat = sim_PLBlist[[i]]$mat_site,
           sdat_site = sim_PLBlist[[i]]$sdat_site)
}


# estimate predicted y-values from list of x-values (e.g., simulate prob x>= x)
y_PLB <- list()

for(i in 1:length(sim_PLBlist)){
  y_PLB[[i]] = tibble(y_PLB = 1 - pPLB(x = x_PLB[[i]]$x_PLB,
                                   b = unique(x_PLB[[i]]$b),
                                   xmin = unique(x_PLB[[i]]$xmin),
                                   xmax = unique(x_PLB[[i]]$xmax))) %>% 
    mutate(group = sim_PLBlist[[i]]$group,
           param = sim_PLBlist[[i]]$name,
           xmin = sim_PLBlist[[i]]$xmin,
           xmax = sim_PLBlist[[i]]$xmax,
           mat = sim_PLBlist[[i]]$mat_site,
           sdat_site = sim_PLBlist[[i]]$sdat_site,
           x = x_PLB[[i]]$x_PLB,
           id = 1:nrow(.))
}

y_PLB_tibble <- bind_rows(y_PLB) %>%
  pivot_wider(names_from = param, values_from = y_PLB) %>% 
  left_join(dat_to_plot %>% distinct(group, b))

# make plots
library(viridis)

# one site
y_PLB_tibble %>% 
  filter(group == "ARIK_2018_3") %>%
  ggplot(aes(x = x, group = group)) + 
  geom_point(data = mle_sim_tibble %>% 
               filter(group == "MAYF_2018_10"), aes(x = bodyMasssim, y = cumPropsim),
             shape = 21, size = 0.8) +
  geom_line(aes(y = Mean), alpha = 0.5) +
  geom_ribbon(aes(ymin = confMin, ymax = confMax), alpha = 0.2) +
  scale_y_log10() + 
  scale_x_log10() + 
  labs(x = "Individual dry mass (mg)", 
       color = expression(italic("b")),
       y = "Proportion of values \u2265 x") +
  theme_default() + 
  theme(text = element_text(size = 38)) +
  NULL


# all sites
all_mle_slopes <- y_PLB_tibble %>%
  ggplot(aes(x = x, group = group)) + 
  geom_line(aes(y = Mean), alpha = 0.5) +
  geom_ribbon(aes(ymin = confMin, ymax = confMax), alpha = 0.2) +
  scale_y_log10() + 
  scale_x_log10() + 
  labs(x = "Individual dry mass (mg)", 
       color = expression(italic("b")),
       y = "Proportion of values \u2265 x") +
  theme_default() + 
  theme(text = element_text(size = 38)) +
  NULL

ggsave(all_mle_slopes, file = "plots/all_mle_slopes.jpg", dpi = 600, width = 12, height = 11)





# Make map ---------------------------------------------------------------

library(maps)
library(ggrepel)

neon_latlong <- read_csv(file = "data/site_latlong.csv") %>% rename(site_id = site) %>% 
  left_join(mle_mat %>% distinct(site_id, mat_site))
states <- map_data("state")
world <- map_data("world")

map <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey50") +
  geom_polygon(data = states, aes(x = long, y = lat, group =group), color = "white", fill = "grey50") +
  coord_quickmap() +
  geom_label_repel(data = neon_latlong, aes(label = site_id, x = long, y = lat), size = 5) +
  geom_point(data = neon_latlong, aes(x = long, y = lat, fill = mat_site), size = 7,
             color = "black", shape = 21) +
  ylim(c(10,75))+
  xlim(c(-180,-50)) +
  theme_void() +
  labs(fill = "Mean Annual\nTemp (deg C)") +
  scale_fill_viridis() +
  theme(text = element_text(size = 24),
        legend.position = c(0.25,0.45))

ggsave(map, file = "plots/map.jpg", dpi = 600, width = 14, height = 14)


