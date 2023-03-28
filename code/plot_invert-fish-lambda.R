library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
source("code/custom-functions/get_sample_lambdas.R") # automates wrangling of sample-specific posterior lambdas

# get data
neon_sizes_2016_2021 = readRDS(file = "data/derived_data/fish_inverts_dw-allyears.rds") %>% 
    filter(year >= 2016 & year <= 2021)

dat = neon_sizes_2016_2021 %>% mutate(temp_mean = mean, 
                                      temp_sd = sd)

mean_temp = mean(unique(dat$temp_mean))
sd_temp = sd(unique(dat$temp_mean))

# load models
fishinvertmod = readRDS("models/stan_gppxtemp2023-02-18.rds")

# extract posteriors
posts_sample_lambdas = get_sample_lambdas(fishinvertmod, data = dat)


# x = temp, y = isd, facet = gpp quantiles --------------------------------

posts_medians = posts_sample_lambdas %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)

# lambda regression
post_lines = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  expand_grid(mat_s = seq(min(dat$mat_s), max(dat$mat_s), length.out = 10)) %>% 
  expand_grid(log_gpp_s = quantile(unique(dat$log_gpp_s), probs = c(0.25, 0.5, 0.75))) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_gpp_mat*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s) %>% 
  median_qi(lambda) %>% 
  mutate(panel = case_when(log_gpp_s == min(log_gpp_s) ~ "a) Low GPP (q25)",
                           log_gpp_s == max(log_gpp_s) ~ "c) High GPP (q75)",
                           TRUE ~ "b) Median GPP (q50)"),
         raw_temp = (mat_s*sd_temp) + mean_temp)

# counterfactual plots
(isd_temp_by_gpp = post_lines %>% 
  ggplot(aes(x = raw_temp, y = lambda)) + 
  geom_line(aes(group = as.factor(log_gpp_s))) + 
  geom_ribbon(aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
  facet_wrap(~panel) + 
  ylim(-1.4, -1.1) + 
  theme_default() + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)"))

ggview::ggview(isd_temp_by_gpp, width = 6, height = 2.3, units = "in")
ggsave(isd_temp_by_gpp, file = "plots/isd_temp_by_gpp.jpg", width = 6, height = 2.3, units = "in")
saveRDS(isd_temp_by_gpp, file = "plots/isd_temp_by_gpp.rds")


# regression with samples 
(isd_by_temp = posts_medians %>% 
  ggplot(aes(x = raw_temp, y = lambda)) + 
  geom_pointrange(aes(ymin = .lower, ymax = .upper),
                  position = position_jitter(width = 0.02),
                  alpha = 0.7,
                  shape = 21, size = 0.1) +
  geom_line(data = post_lines %>% filter(log_gpp_s!= min(log_gpp_s) &
                                           log_gpp_s != max(log_gpp_s)), 
            aes(group = as.factor(log_gpp_s))) + 
  geom_ribbon(data = post_lines %>% filter(log_gpp_s!= min(log_gpp_s) &
                                             log_gpp_s != max(log_gpp_s)), 
              aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
  # ylim(-1.4, -1.1) + 
  theme_default() + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)"))

ggview::ggview(isd_by_temp, width = 4, height = 3, units = "in")
ggsave(isd_by_temp, file = "plots/isd_by_temp.jpg", width = 6, height = 3, units = "in")
saveRDS(isd_by_temp, file = "plots/isd_by_temp.rds")



# x = gpp, y = isd, facet = temp quantiles --------------------------------
posts_medians = posts_sample_lambdas %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s) %>% 
  median_qi(lambda)

# lambda regression
post_lines_xis_gpp = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  expand_grid(log_gpp_s = seq(min(dat$log_gpp_s), max(dat$log_gpp_s), length.out = 10)) %>% 
  expand_grid(mat_s = quantile(unique(dat$mat_s), probs = c(0.25, 0.5, 0.75))) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_gpp_mat*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s) %>% 
  median_qi(lambda) %>% 
  mutate(panel = case_when(mat_s == min(mat_s) ~ "a) Low temp (q25)",
                           mat_s == max(mat_s) ~ "c) High temp (q75)",
                           TRUE ~ "b) Median temp (q50)"),
         raw_temp = (mat_s*sd_temp) + mean_temp)

# counterfactual plots
(isd_gpp_by_temp = post_lines_xis_gpp %>% 
    ggplot(aes(x = log_gpp_s, y = lambda)) + 
    geom_line(aes(group = as.factor(mat_s))) + 
    geom_ribbon(aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
    facet_wrap(~panel) + 
    ylim(-1.4, -1.1) +
    theme_default() + 
    labs(y = "\u03bb (ISD exponent)",
         x = "Mean Annual Temperature (\u00b0C)"))

ggview::ggview(isd_gpp_by_temp, width = 6, height = 2.3, units = "in")
ggsave(isd_gpp_by_temp, file = "plots/isd_gpp_by_temp.jpg", width = 6, height = 2.3, units = "in")
saveRDS(isd_gpp_by_temp, file = "plots/isd_gpp_by_temp.rds")


# regression with samples 
posts_medians %>% 
  ggplot(aes(x = mat_s, y = lambda)) + 
  geom_pointrange(aes(ymin = .lower, ymax = .upper),
                  position = position_jitter(width = 0.02),
                  alpha = 0.7,
                  shape = 21, size = 0.4) +
  geom_line(data = post_lines %>% filter(log_gpp_s!= min(log_gpp_s) &
                                           log_gpp_s != max(log_gpp_s)), 
            aes(group = as.factor(log_gpp_s))) + 
  geom_ribbon(data = post_lines %>% filter(log_gpp_s!= min(log_gpp_s) &
                                             log_gpp_s != max(log_gpp_s)), 
              aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
  # ylim(-1.4, -1.1) + 
  theme_default() + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)")



# plot isd's --------------------------------------------------------------

# sample dw weighted by density
nsamples = 1000

dat_sims = dat %>% 
  # filter(sample_int == id) %>%
  left_join(posts_medians) %>% 
  group_by(sample_int) %>% 
  sample_n(nsamples, weight = no_m2, replace = T) %>% 
  select(dw, site_id, year, sample_int, xmin, xmax, no_m2, lambda, .lower, .upper) 

dat_toplot = dat_sims %>% 
  # filter(sample_int %in% c(id)) %>% 
  group_by(sample_int) %>% 
  arrange(desc(dw)) %>% 
  mutate(y_order = 1:nsamples) 

dat_split = dat_sims %>% 
  # filter(sample_int %in% c(id)) %>% 
  group_by(sample_int) %>% 
  group_split

xy.PLB = NULL
for(i in 1:length(dat_split)) {
  sample_int = unique(dat_split[[i]]$sample_int)
  site_id = unique(dat_split[[i]]$site_id)
  year = unique(dat_split[[i]]$year)
  xmin = min(dat_split[[i]]$dw)
  xmax = max(dat_split[[i]]$dw)
  
  lambda = unique(dat_split[[i]]$lambda)
  .lower = unique(dat_split[[i]]$.lower)
  .upper = unique(dat_split[[i]]$.upper)
  
  x.PLB = seq(min(dat_split[[i]]$dw),
              max(dat_split[[i]]$dw),
              length=nsamples) # x values to plot PLB
  
  y.PLB = (1 - (x.PLB^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1))))*nsamples
  ymin.PLB = (1 - (x.PLB^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1))))*nsamples
  ymax.PLB = (1 - (x.PLB^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1))))*nsamples
  
  xy.PLB[[i]] = tibble(dw = x.PLB, y_order = y.PLB,
                       ymin = ymin.PLB,
                       ymax = ymax.PLB,
                       xmax = xmax,
                       xmin = xmin) %>%
    mutate(sample_int = sample_int,
           site_id = site_id,
           year = year,
           lambda = lambda)
}

lines_toplot = bind_rows(xy.PLB)  %>% 
  # filter(sample_int <= 10) %>% 
  mutate(facet_name = paste(site_id, sample_int))

isd_per_sample_plot = dat_toplot %>% 
  mutate(facet_name = paste(site_id, sample_int)) %>% 
  ggplot(aes(x = dw, y = y_order, group = sample_int)) + 
  geom_point(shape = 21, size = 0.3, aes(color = site_id)) +
  geom_line(data = lines_toplot ) +
  geom_ribbon(data = lines_toplot , aes(ymin = ymin, ymax = ymax), alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~sample_int) +
  theme_default() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    axis.text = element_blank()) +
  labs(y = "Number of values \u2265 x",
       x = "Individual dry mass (mg)",
       color = "") +
  guides(color = "none") +
  coord_cartesian(ylim = c(limits = c(min(dat_toplot$y_order), NA)))

isd_per_sample_plot


