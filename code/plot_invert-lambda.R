library(brms)
library(tidyverse)
library(tidybayes)
library(janitor)


neon_sizes_2016_2021 = readRDS(file = "data/derived_data/macro_dw-wrangled.rds") %>% 
    filter(year >= 2016 & year <= 2021)

dat = neon_sizes_2016_2021

stan_data_interaction = list(N = nrow(dat),
                             mat_s = dat$mat_s,
                             gpp_s = dat$log_gpp_s,
                             year = dat$year_int,
                             site = dat$site_int,
                             sample = dat$sample_int,
                             n_years = length(unique(dat$year_int)),
                             n_sites = length(unique(dat$site_int)),
                             n_samples = length(unique(dat$sample_int)),
                             counts = dat$no_m2,
                             x = dat$dw,
                             xmin = dat$xmin,
                             xmax = dat$xmax) %>% 
  as_tibble()

invertmod = readRDS("models/stan_invertsonly_gppxtemp2023-02-18.rds")

ids = dat %>% mutate(id = paste(year_int, site_int, sample_int, sep = "_")) %>% 
  ungroup %>% distinct(id, mat_s, log_gpp_s, site_int, site_id, year_int, year)

posts_long = as_draws_df(invertmod) %>% as_tibble() %>% 
  mutate(iter = 1:nrow(.)) %>% 
  filter(iter <= 500) %>% 
  pivot_longer(cols = contains("alpha_raw_year"),
               names_to = "yeargroup",
               values_to = "year_offset") %>% 
  pivot_longer(cols = contains("alpha_raw_site"),
               names_to = "sitegroup", 
               values_to = "site_offset") %>% 
  pivot_longer(cols = contains("alpha_raw_sample"),
               names_to = "samplegroup",
               values_to = "sample_offset")

posts_long_parsed = posts_long %>% 
  mutate(site_int = parse_number(sitegroup),
         year_int = parse_number(yeargroup),
         sample_int = parse_number(samplegroup),
         id = paste(year_int, site_int, sample_int, sep = "_"))


posts_medians = posts_long_parsed %>% filter(id %in% ids$id) %>% 
  left_join(ids) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_gpp_mat*mat_s*log_gpp_s +
           sigma_year*year_offset + sigma_site*site_offset + sigma_sample*sample_offset) %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s) %>% 
  median_qi(lambda)

post_lines = as_draws_df(invertmod) %>% as_tibble() %>% 
  expand_grid(mat_s = unique(dat$mat_s)) %>% 
  expand_grid(log_gpp_s = quantile(unique(dat$log_gpp_s), probs = c(0.25, 0.5, 0.75))) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_gpp_mat*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s) %>% 
  median_qi(lambda)

post_lines %>% 
  ggplot(aes(x = mat_s, y = lambda)) + 
  # geom_pointrange(aes(ymin = .lower, ymax = .upper),
  #                 position = position_jitter(width = 0.02),
  #                 alpha = 0.7,
  #                 shape = 21, size = 0.1) + 
  geom_line(data = post_lines, aes(group = as.factor(log_gpp_s))) + 
  geom_ribbon(data = post_lines, aes(ymin = .lower,ymax = .upper,
                                     fill = as.factor(log_gpp_s)), alpha = 0.2) + 
  facet_wrap(~log_gpp_s)




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
  facet_wrap(~site_id) +
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

