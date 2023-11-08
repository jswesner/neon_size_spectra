library(tidyverse)
library(packcircles)
library(viridis)
library(isdbayes)
library(ggthemes)

# make pareto sizes
n = 8000
dw <- sort(rparetocounts(n = n, mu = -1.75))

# dot plots ---------------------------------------------------------------

# make uniform sizes
dw_flat = rep(1, n)
packing2 <- circleProgressiveLayout( rev(dw_flat) ) 
dat2 <- circleLayoutVertices(packing2)

(uniform_circle1 = 
    ggplot(data = dat2, aes(x, y)) + 
    geom_polygon(aes(group = id),
                 alpha = 0.9,
                 fill = "#e34a33",
                 colour = "black", show.legend = FALSE) +
    # scale_fill_distiller(palette = "YlOrRd") +
    # scale_fill_brewer(type = "", palette = 3) +
    # scale_alpha_continuous() +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
)


(uniform_circle = 
  ggplot(data = dat2, aes(x, y)) + 
    geom_polygon(aes(group = id, fill = -id),
                 alpha = 0.9,
                 colour = "black", show.legend = FALSE) +
    # scale_fill_distiller(palette = "YlOrRd") +
    # scale_fill_brewer(type = "", palette = 3) +
    # scale_alpha_continuous() +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
)

# make pareto sizes
dw <- sort(rparetocounts(n = n, mu = -1.75))

packing1 <- circleProgressiveLayout( rev(dw) ) 
dat1 <- circleLayoutVertices(packing1)

(pareto_circle = 
  ggplot(data = dat1, aes(x, y)) + 
    geom_polygon(aes(group = id, fill = -id),
                 alpha = 0.9,
                 colour = "black", show.legend = FALSE) +
  # scale_fill_distiller(palette = "YlOrRd") +
    # scale_fill_brewer(type = "", palette = 3) +
    # scale_alpha_continuous() +
  viridis::scale_fill_viridis() +
  coord_equal() +
  theme_void()
)

ggsave(uniform_circle, file = "plots/uniform_circle.jpg", width = 5, height = 5, units = "in", dpi = 400)
ggsave(pareto_circle, file = "plots/pareto_circle.jpg", width = 5, height = 5, units = "in", dpi = 400)



# food web plot -----------------------------------------------------------
trophic_dots = tibble(dw = dw) %>% 
  mutate(x_bins = cut( dw, breaks = c(1, 400, 800, 950, 1000))) %>% 
  group_by(x_bins) %>% 
  add_tally() %>% 
  separate(x_bins, into = c("lower", "upper"), sep = ",", remove = F) %>% 
  mutate(lower = parse_number(lower),
         upper = parse_number(upper)) %>% 
  ungroup %>% 
  mutate(ndot = runif(nrow(.), lower, upper)) %>% 
  group_by(x_bins) %>% 
  mutate(mindot = min(ndot),
         ndot = ndot - mindot,
         mass = sum(dw),
         ndot = ndot*mass,
         nbar = max(ndot)) %>% 
  ungroup %>% 
  mutate(trophic = case_when(upper <= 400 ~ "1",
                             upper <= 800 ~ "2",
                             upper <= 990 ~ "3",
                             TRUE ~ "4"))

trophic_bars = trophic_dots %>% 
  ungroup %>% 
  distinct(x_bins, nbar, trophic)

set.seed(20202)
trophic_plot = trophic_dots %>% 
  ggplot(aes(x = nbar, y = trophic)) + 
  geom_bar(data = trophic_bars, stat = "identity") +
  geom_jitter(aes(x = ndot,y = trophic, size = dw, fill = dw),
              shape = 21, width = 0, height = 0.4) +
  scale_size_continuous(range = c(0, 5)) +
  scale_fill_viridis() +
  theme_void() +
  labs(x = "Mass") +
  guides(fill = "none",
         size = "none") + 
  theme(axis.text.y = element_text(),
        axis.title.x = element_text())
  
ggsave(trophic_plot, file = "plots/trophic_plot.jpg", width = 4, height = 4, dpi = 300)

# isd plot --------------------------------------------

library(rstan)
library(tidyverse)
library(janitor)
library(tidybayes)
library(brms)
library(ggthemes)
source("code/sandbox/paretocounts.R")
theme_set(brms::theme_default())

# 1) load models
fishinvertmod = readRDS("models/fit_pareto.rds")

# 2) get data
dat = as_tibble(fishinvertmod$data)

# 3) extract posteriors 
posts_sample_lambdas_summary = dat %>% 
  distinct(sample_id, .keep_all = T) %>% 
  add_epred_draws(fishinvertmod, re_formula = NULL) %>% 
  rename(lambda = .epred) %>% 
  group_by(sample_id) %>% 
  median_qi(lambda)

# 4) merge posts and raw data
n_samples = 500

dat_resampled = dat %>% 
  group_by(sample_id) %>% 
  sample_n(n_samples, weight = no_m2, replace = T) %>% 
  # bind_rows(mins, maxs) %>% 
  select(site_id, year, sample_id, no_m2, dw, xmin, xmax) %>% 
  group_by(sample_id) %>% 
  mutate(sample_xmin = min(dw),
         sample_xmax = max(dw),
         data = "y_raw") %>% 
  arrange(sample_id, -dw) %>% 
  mutate(y_data = (1:n_samples - 1)/n_samples) %>%
  ungroup %>% 
  left_join(posts_sample_lambdas_summary) 

# isd's for each sample
dat_resampled_split = dat_resampled %>% 
  # group_by(sample_id) %>% 
  group_split(sample_id, lambda, .lower, .upper, site_id, year, xmin, xmax,
              sample_xmin, sample_xmax)

sample_sims = NULL

for(i in 1:length(dat_resampled_split)){
  
  min = min(dat_resampled_split[[i]]$xmin) 
  max = max(dat_resampled_split[[i]]$xmax)
  dw_seq = seq(min, max, length.out = n_samples)

  sample_sims[[i]] = dat_resampled_split[[i]] %>% select(-dw, -no_m2, -y_data) %>% distinct() %>% 
    expand_grid(x.PLB = dw_seq) %>% 
    mutate(y.PLB = (1 - (x.PLB^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1)))),
           ymin.PLB = (1 - (x.PLB^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1)))),
           ymax.PLB = (1 - (x.PLB^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1))))) 
}


isd_lines = bind_rows(sample_sims) %>%
  # filter(sample_id == 1) %>%
  ggplot(aes(x = x.PLB, y = y.PLB, group = sample_id, 
             color = lambda)) + 
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  # geom_point(data = dat_resampled %>%
  #              filter(sample_id == 1) , aes(x = dw, y = y_data))  +
  coord_cartesian(ylim = c(1e-04, 1)) +
  viridis::scale_color_viridis() +
  # facet_wrap(~site_id) +
  labs(y = "Number of values \u2265 x",
       x = "Individual dry mass (mg)",
       color = "\u03bb") +
  theme(text = element_text(size = 12),
        legend.position = c(0.8, 0.85),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.5,"line")) +
  NULL

isd_lines

ggsave(isd_lines, file = "plots/ms_plots/isd_lines.jpg", width = 5, height = 5,
       dpi = 500)

# deviance
n_samples = 80000

dat_resampled = dat %>% 
  group_by(sample_id) %>% 
  sample_n(n_samples, weight = no_m2, replace = T) %>% 
  # bind_rows(mins, maxs) %>% 
  select(site_id, year, sample_id, no_m2, dw, xmin, xmax) %>% 
  group_by(sample_id) %>% 
  mutate(sample_xmin = min(dw),
         sample_xmax = max(dw),
         data = "y_raw") %>% 
  arrange(sample_id, -dw) %>% 
  mutate(y_data = (1:n_samples - 1)/n_samples) %>%
  ungroup %>% 
  left_join(posts_sample_lambdas_summary) 

# isd's for each sample
dat_resampled_split = dat_resampled %>% 
  # group_by(sample_id) %>% 
  filter(sample_id == 1) %>% 
  group_split(sample_id, lambda, .lower, .upper, site_id, year, xmin, xmax,
              sample_xmin, sample_xmax)

sim_dw_yplb = NULL

for(i in 1:length(dat_resampled_split)){
  
  min = min(dat_resampled_split[[i]]$xmin) 
  max = max(dat_resampled_split[[i]]$xmax)
  dw_seq = dat_resampled_split[[i]]$dw
  
  sim_dw_yplb[[i]] = dat_resampled_split[[i]] %>% select(-dw, -no_m2, -y_data) %>% distinct() %>% 
    expand_grid(x.PLB = dw_seq) %>% 
    mutate(y.PLB = (1 - (x.PLB^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1)))),
           ymin.PLB = (1 - (x.PLB^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1)))),
           ymax.PLB = (1 - (x.PLB^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1))))) 
}


post_sims_tobind = bind_rows(sim_dw_yplb) %>% 
  filter(sample_id == 1) %>% 
  arrange(x.PLB) %>% 
  select(x.PLB, y.PLB) %>% 
  mutate(order = row_number())


dat_raw = dat_resampled %>% 
  filter(sample_id == 1) %>% 
  select(y_data, dw, sample_id) %>% 
  arrange(dw) %>% 
  mutate(order = row_number())

left_join(post_sims_tobind, dat_raw) %>% 
  mutate(deviance = y_data/y.PLB) %>%
  ggplot(aes(x = x.PLB, y = deviance)) + 
  geom_line() +
  geom_vline(xintercept = 0.16) +
  # scale_y_log10() +
  scale_x_log10()

left_join(post_sims_tobind, dat_raw) %>% 
  mutate(deviance = y_data/y.PLB) %>% 
  filter(deviance <= 1.1 & deviance >= 0.9)



# sample_specific isds ----------------------------------------------------


sample_sims_samplexmax = NULL

for(i in 1:length(dat_resampled_split)){
  
  min = min(dat_resampled_split[[i]]$sample_xmin) 
  max = max(dat_resampled_split[[i]]$sample_xmax)
  dw_seq = seq(min, max, length.out = n_samples)
  
  sample_sims_samplexmax[[i]] = dat_resampled_split[[i]] %>% select(-dw, -no_m2, -y_data) %>% distinct() %>% 
    expand_grid(x.PLB = dw_seq) %>% 
    mutate(y.PLB = (1 - (x.PLB^(lambda + 1) - (sample_xmin^(lambda+1)))/(sample_xmax^(lambda + 1) - (sample_xmin^(lambda+1)))),
           ymin.PLB = (1 - (x.PLB^(.lower + 1) - (sample_xmin^(.lower+1)))/(sample_xmax^(.lower + 1) - (sample_xmin^(.lower+1)))),
           ymax.PLB = (1 - (x.PLB^(.upper + 1) - (sample_xmin^(.upper+1)))/(sample_xmax^(.upper + 1) - (sample_xmin^(.upper+1))))) 
}


bind_rows(sample_sims_samplexmax) %>%
  # filter(site_id == "ARIK") %>%
  ggplot(aes(x = x.PLB, y = y.PLB, group = sample_id, 
             color = lambda)) + 
  geom_line() +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(data = dat_resampled, aes(x = dw, y = y_data),
             size = 0.2)  +
  coord_cartesian(ylim = c(1e-04, 1)) +
  viridis::scale_color_viridis() +
  # facet_wrap(~site_id) +
  labs(y = "Number of values \u2265 x",
       x = "Individual dry mass (mg)",
       color = "\u03bb") +
  theme(text = element_text(size = 12),
        legend.position = c(0.8, 0.85),
        legend.direction = "horizontal",
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.5,"line")) +
  facet_wrap(~sample_id) +
  guides(color = "none") +
  theme_void() +
  NULL



# isd and biomass ---------------------------------------------------------

mass_brm = readRDS("models/community_mass_brm.rds")
conditional_effects(mass_brm)

community_mass = readRDS(file = "data/derived_data/community_mass.rds")

posts_sample_lambdas_summary %>% 
  left_join(community_mass %>% distinct(sample_id, total_g_dwm2, year)) %>% 
  ggplot(aes(x = total_g_dwm2, y = lambda)) + 
  geom_pointrange(aes(ymin = .lower, ymax = .upper)) +
  scale_x_log10() + 
  geom_smooth(method = "lm") +
  facet_wrap(~year)

# fish mass plot
fish_dw_taxa <- readRDS("data/derived_data/fish_dw_taxa.rds") %>% mutate(animal_type = "fish") %>% 
  rename(taxon = taxon_id) %>% distinct(dw, taxon, animal_type)

invertebrate_taxonomy <- read_csv("data/derived_data/aqua-sync_data/invertebrate-taxonomy.csv") %>% 
  rename(taxon = scientificName) %>% distinct(taxon, order)

invertebrate_size_data <- read_csv("data/derived_data/aqua-sync_data/invertebrate-size-data.csv") %>% 
  left_join(invertebrate_taxonomy) %>% 
  mutate(animal_type = "inverts") %>% 
  rename(dw = body_mass) %>% 
  distinct(dw, order, animal_type) %>% 
  rename(taxon = order)

dat_size = bind_rows(invertebrate_size_data, fish_dw_taxa) %>% 
  group_by(taxon) %>% 
  mutate(max = max(dw))

taxon_size_plot = dat_size %>%
  ggplot(aes(x = dw, y = reorder(taxon, max), color = animal_type)) +
  geom_point(alpha = 0.4, size = 0.1) +
  scale_color_colorblind() +
  brms::theme_default() + 
  labs(color = "",
       x = "mgDM Individual",
       y = "Taxon (ranked by dw)") +
  theme(axis.text.y = element_blank(),
        text = element_text(size = 16)) + 
  guides(color = guide_legend(override.aes = list(size = 2, alpha = 1))) 

saveRDS(taxon_size_plot, file = "plots/ms_plots/taxon_size_plot.rds")  

