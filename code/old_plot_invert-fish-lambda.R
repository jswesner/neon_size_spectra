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
dat_all = readRDS("data/derived_data/dat_all.rds")

mean_temp = mean(unique(dat_all$temp_mean))
sd_temp = sd(unique(dat_all$temp_mean))
mean_om = mean(unique(dat_all$log_om))
sd_om = sd(unique(dat_all$log_om))
mean_gpp = mean(unique(dat_all$log_gpp))
sd_gpp = sd(unique(dat_all$log_gpp))



# load models
fishinvertmod = readRDS("models/stan_gppxtempxom2023-05-12.rds")

# extract posteriors
posts_sample_lambdas = get_sample_lambdas(fishinvertmod, data = dat_all)
saveRDS(posts_sample_lambdas, file = "models/posteriors/posts_sample_lambdas.rds")


# x = temp, y = isd, facet = gpp and quantiles --------------------------------

qlog_om_s = quantile(unique(dat_all$log_om_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_om_s", names_to = "quantile_om") %>% 
  mutate(quantile_om = c("Low OM", "Median OM", "High OM"))

qlog_gpp_s = quantile(unique(dat_all$log_gpp_s), probs = c(0.25, 0.5, 0.75), na.rm = T) %>% 
  as_tibble() %>% 
  pivot_longer(cols = everything(), values_to = "log_gpp_s", names_to = "quantile_gpp") %>% 
  mutate(quantile_gpp = c("Low GPP", "Median GPP", "High GPP"))

# lambda regression
post_lines = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  expand_grid(mat_s = seq(min(dat_all$mat_s), max(dat_all$mat_s), length.out = 10)) %>% 
  expand_grid(qlog_gpp_s) %>% 
  expand_grid(qlog_om_s) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s, log_om_s, quantile_gpp, quantile_om) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)  %>% 
  mutate(quantile_om = as.factor(quantile_om)) %>% 
  mutate(quantile_om = fct_relevel(quantile_om, "Low OM", "Median OM")) %>% 
  mutate(quantile_gpp = as.factor(quantile_gpp)) %>% 
  mutate(quantile_gpp = fct_relevel(quantile_gpp, "Low GPP", "Median GPP"))

saveRDS(post_lines, file = "models/posteriors/post_lines.rds")

# counterfactual plots
(isd_temp_by_gpp = post_lines %>% 
  ggplot(aes(x = raw_temp, y = lambda)) + 
  geom_line(aes(group = as.factor(log_gpp_s))) + 
  geom_ribbon(aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
  facet_grid(quantile_gpp ~ quantile_om) + 
  ylim(-1.4, -1.1) + 
  theme_default() + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)"))

ggview::ggview(isd_temp_by_gpp, width = 6, height = 6, units = "in")
ggsave(isd_temp_by_gpp, file = "plots/isd_temp_by_gpp_om.jpg", width = 6, height = 6, units = "in")
saveRDS(isd_temp_by_gpp, file = "plots/isd_temp_by_gpp_om.rds")

(isd_temp_by_OM = post_lines %>% 
    filter(quantile_gpp == "Median GPP") %>% 
    ggplot(aes(x = raw_temp, y = lambda)) + 
    geom_line(aes(group = as.factor(log_gpp_s))) + 
    geom_ribbon(aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
    facet_grid(~ quantile_om) + 
    ylim(-1.4, -1.1) + 
    theme_default() + 
    labs(y = "\u03bb (ISD exponent)",
         x = "Mean Annual Temperature (\u00b0C)")
  )

ggview::ggview(isd_temp_by_OM, width = 5, height = 2, units = "in")
ggsave(isd_temp_by_OM, file = "plots/isd_temp_by_OM.jpg", width = 5, height = 2, units = "in")
saveRDS(isd_temp_by_OM, file = "plots/isd_temp_by_OM.rds")


# regression with samples -----------------------------------
posts_medians = posts_sample_lambdas %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)

saveRDS(posts_medians, file = "models/posteriors/posts_medians.rds")

post_lines_median = post_lines %>% 
  filter(grepl("edian", quantile_gpp)) %>% filter(grepl("edian", quantile_gpp)) %>% 
  filter(grepl("edian", quantile_om)) %>% filter(grepl("edian", quantile_om))

(isd_by_temp = posts_medians %>% 
  ggplot(aes(x = raw_temp, y = lambda)) + 
  geom_pointrange(aes(ymin = .lower, ymax = .upper),
                  position = position_jitter(width = 0.09),
                  alpha = 0.3,
                  shape = "O", size = 0.2) +
  # geom_point(shape = 21, size = 0.5) +
  geom_line(data = post_lines_median, 
            aes(group = as.factor(log_gpp_s))) + 
  geom_ribbon(data = post_lines_median, 
              aes(ymin = .lower,ymax = .upper), alpha = 0.2) + 
  ylim(-2, -1) +
  theme_default() + 
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)"))

ggview::ggview(isd_by_temp, width = 5, height = 5, units = "in")
ggsave(isd_by_temp, file = "plots/isd_by_temp.jpg", width = 5, height = 5, units = "in")
saveRDS(isd_by_temp, file = "plots/isd_by_temp.rds")

coefs_isd_fishinvert = tidy_draws(fishinvertmod) %>% 
  select(starts_with("beta_")) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  # median_qi(value) %>% 
  mutate(coefficient = str_sub(name, 6, 20),
         coefficient = str_replace(coefficient, "_", ":"),
         coefficient = str_replace(coefficient, "_", ":"),
         coefficient = paste0("\u03b2", coefficient),
         order = str_length(coefficient))
  
coefs_isd_fishinvert %>% 
  ggplot(aes(y = value, x = reorder(coefficient, -order))) + 
  stat_pointinterval() +
  coord_flip(ylim = c(-0.05, 0.05)) +
  geom_hline(aes(yintercept = 0)) + 
  theme_default() +
  labs(x = "Model coefficients",
       y = "Value")


# x = gpp, y = isd, facet = temp quantiles --------------------------------
posts_medians = posts_sample_lambdas %>% 
  group_by(year, site_id, sample_int, mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(raw_temp = (mat_s*sd_temp) + mean_temp)

# lambda regression
post_lines_xis_gpp = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  expand_grid(log_gpp_s = seq(min(dat_all$log_gpp_s), max(dat_all$log_gpp_s), length.out = 10)) %>% 
  expand_grid(mat_s = quantile(unique(dat_all$mat_s), probs = c(0.25, 0.5, 0.75))) %>% 
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

# three-panel fixed effects -----------------------------------------------

gpp_lines = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  # expand_grid(mat_s = seq(min(dat_invert$mat_s), max(dat_invert$mat_s), length.out = 10)) %>% 
  expand_grid(log_gpp_s = seq(min(dat_invert$log_gpp_s), max(dat_invert$log_gpp_s), length.out = 20)) %>% 
  expand_grid(mat_s = 0) %>% 
  expand_grid(log_om_s = 0) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(pred = "gpp",
         x = log_gpp_s) %>% 
  mutate(x_raw = exp((log_gpp_s*sd_gpp) + mean_gpp))

om_lines = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  # expand_grid(mat_s = seq(min(dat_invert$mat_s), max(dat_invert$mat_s), length.out = 10)) %>% 
  expand_grid(log_om_s = seq(min(dat_invert$log_om_s), max(dat_invert$log_om_s), length.out = 20)) %>% 
  expand_grid(mat_s = 0) %>% 
  expand_grid(log_gpp_s = 0) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(pred = "om",
         x = log_om_s) %>% 
  mutate(x_raw = exp((log_om_s*sd_om) + mean_om))

temp_lines = as_draws_df(fishinvertmod) %>% as_tibble() %>% 
  expand_grid(mat_s = seq(min(dat_all$mat_s), max(dat_all$mat_s), length.out = 20)) %>% 
  mutate(log_gpp_s = median(unique(dat_all$log_gpp_s)),
         log_om_s = median(unique(dat_all$log_om_s))) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*log_gpp_s + beta_om*log_om_s +
           beta_gpp_om*log_gpp_s*log_om_s + beta_gpp_mat*log_gpp_s*mat_s + beta_om_mat*log_om_s*mat_s +
           beta_om_mat_gpp*log_om_s*mat_s*log_gpp_s) %>% 
  group_by(mat_s, log_gpp_s, log_om_s) %>% 
  median_qi(lambda) %>% 
  mutate(x_raw = (mat_s*sd_temp) + mean_temp,
         pred = "temp",
         x = mat_s) 

lambda_samples = posts_sample_lambdas %>% 
  pivot_longer(cols = c("log_om_s", "log_gpp_s", "mat_s"), names_to = "pred",
               values_to = "x") %>%
  # pivot_longer(cols = c("mean_om", "gpp", "temp_mean"), names_to = "pred", 
               # values_to = "x") %>% 
  group_by(x, pred) %>% 
  mutate(pred = case_when(pred == "log_om_s" ~ "om",
                          pred == "log_gpp_s" ~ "gpp",
                          TRUE ~ "temp")) %>% 
  median_qi(lambda)

bind_rows(temp_lines, gpp_lines, om_lines) %>% 
  ggplot(aes(x = x, y = lambda)) + 
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.2) +
  facet_wrap(~pred, scales = "free_x") + 
  # scale_x_continuous(labels = x_raw) +
  # scale_x_log10() +
  geom_pointrange(data = lambda_samples, aes(ymin = .lower, ymax = .upper)) +
  NULL
  

# plot isd's --------------------------------------------------------------

# sample dw weighted by density
nsamples = 1000

dat_sims = dat_all %>% 
  # filter(sample_int == id) %>%
  left_join(posts_medians) %>% 
  group_by(sample_int) %>% 
  sample_n(nsamples, weight = no_m2, replace = T) %>% 
  select(dw, site_id, year, sample_int, xmin, xmax, no_m2, lambda, .lower, .upper) 

dat_toplot = dat_sims %>% 
  # filter(sample_int %in% c(id)) %>% 
  group_by(sample_int) %>% 
  arrange(sample_int, desc(dw)) %>% 
  mutate(y_order = 1:nsamples) %>% 
  left_join(dat_all %>% ungroup %>% distinct(site_id, mean, log_gpp_s, log_om_s))

dat_split = dat_sims %>% 
  # filter(sample_int %in% c(id)) %>% 
  group_by(sample_int) %>% 
  group_split

xy.PLB = NULL
for(i in 1:length(dat_split)) {
  sample_int = unique(dat_split[[i]]$sample_int)
  site_id = unique(dat_split[[i]]$site_id)
  year = unique(dat_split[[i]]$year)
  xmin = min(dat_split[[i]]$xmin)
  xmax = max(dat_split[[i]]$xmax)
  
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
  mutate(facet_name = paste(site_id, sample_int)) %>% 
  left_join(dat_all %>% ungroup %>% distinct(site_id, mean, log_gpp_s, log_om_s))


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
saveRDS(isd_per_sample_plot, file = "plots/isd_per_sample_plot.rds")
ggsave(isd_per_sample_plot, file = "plots/isd_per_sample_plot.jpg", 
       width = 6.5, height = 6.5, dpi = 400)

# isds range
sites = as.integer(runif(3, min(dat_toplot$sample_int), max(dat_toplot$sample_int)))

isd_ranges = dat_toplot %>% 
  filter(sample_int %in% sites) %>% 
  mutate(facet_name = paste("\u03bb", "=", round(lambda, 2))) %>%
  ggplot(aes(x = dw, y = y_order/1000, group = sample_int)) + 
  geom_point(shape = 21, size = 0.3, aes(color = site_id)) +
  geom_line(data = lines_toplot  %>% 
              mutate(facet_name = paste("\u03bb", "=", round(lambda, 2)))%>% 
              filter(sample_int %in% sites)) +
  geom_ribbon(data = lines_toplot  %>% 
                mutate(facet_name = paste("\u03bb", "=", round(lambda, 2))) %>% 
                filter(sample_int %in% sites), aes(ymin = ymin/1000, ymax = ymax/1000), alpha = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~facet_name) +
  annotate("text", label = site_id, x = 0.1, y = 1.1) +
  labs(y = "Proportion of values \u2265 x",
       x = "Individual dry mass (mg)",
       color = "") +
  theme_default() +
  guides(color = "none")

isd_ranges



#all isds
labels = lines_toplot %>% 
  filter(lambda == max(lambda, na.rm = T)|lambda == min(lambda, na.rm = T)) %>% 
  distinct(lambda, sample_int, mean, log_gpp_s, log_om_s) %>% 
  mutate(label = paste("\u03bb", "=", round(lambda, 2)),
         dw = c(1.21e-01, 0.5e05),
         y_order = 0.01)

isd_lines_onepanel = lines_toplot %>% 
  mutate(facet_name = paste(site_id, sample_int)) %>% 
  ggplot(aes(x = dw, y = y_order/1000, group = sample_int, fill = mean)) + 
  # geom_point(shape = 21, size = 0.3, aes(color = site_id)) +
  geom_ribbon(aes(ymin = ymin/1000, ymax = ymax/1000), alpha = 0.7) +
  geom_line(aes(color = mean), linewidth = 0.2) +
  scale_x_log10() +
  scale_y_log10() +
  # facet_wrap(~sample_int) +
  theme_default() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()) +
  labs(y = "Proportion of body sizes \u2265 x",
       x = "Individual dry mass (mg)",
       fill = "Mean Annual Temp \u00b0C") +
  guides(color = "none") +
  geom_text(data = labels, aes(label = label, y = y_order, x = dw), size = 2.5) +
  viridis::scale_fill_viridis() +
  coord_cartesian(ylim = c(0.001, 1)) +
  theme(legend.position = "top")

ggsave(isd_lines_onepanel, file = "plots/isd_lines_onepanel.jpg", width = 5.5, height = 5, units = "in", dpi = 500)
saveRDS(isd_lines_onepanel, file = "plots/isd_lines_onepanel.rds")


# residuals ---------------------------------------------------------------
# sample dw weighted by density
nsamples = 10000

dat_sims = dat_all %>% 
  # filter(sample_int == id) %>%
  left_join(posts_medians) %>% 
  group_by(sample_int) %>% 
  sample_n(nsamples, weight = no_m2, replace = T) %>% 
  select(dw, site_id, year, sample_int, xmin, xmax, no_m2, lambda, .lower, .upper) 

dat_toplot = dat_sims %>% 
  # filter(sample_int %in% c(id)) %>% 
  group_by(sample_int) %>% 
  arrange(sample_int, desc(dw)) %>% 
  mutate(y_order = (1:nsamples/nsamples)) %>% 
  left_join(dat_all %>% ungroup %>% distinct(site_id, mean, log_gpp_s, log_om_s)) %>% 
  group_by(sample_int) %>% 
  # filter(sample_int == 12) %>% 
  mutate(y.PLB = (1 - (dw^(lambda + 1) - 
                         (xmin^(lambda+1)))/(xmax^(lambda + 1) - 
                                               (xmin^(lambda+1)))),
         y.PLBlower = (1 - (dw^(.lower + 1) - 
                         (xmin^(.lower+1)))/(xmax^(.lower + 1) - 
                                               (xmin^(.lower+1)))),
         y.PLBupper = (1 - (dw^(.upper + 1) - 
                         (xmin^(.upper+1)))/(xmax^(.upper + 1) - 
                                               (xmin^(.upper+1))))) %>% 
  mutate(residual = y_order - y.PLB,
         residual_lower = y_order - y.PLBlower,
         residual_upper = y_order - y.PLBupper)


dat_toplot %>% 
  ggplot(aes(x = dw, y = residual)) + 
  # geom_point() +
  geom_line(aes(group = sample_int)) +
  geom_ribbon(aes(ymin = residual_lower, ymax = residual_upper, group = sample_int),
              alpha = 0.5) +
  # geom_point(aes(y = y.PLB)) + 
  scale_x_log10() +
  geom_hline(aes(yintercept = 0)) +
  facet_wrap(~site_id) +
  labs(subtitle = "The model underpredicts the frequency of small individuals and/noverpredicts the frequency of large individuals") +
  # xlim(1, nsamples) +
  # scale_y_log10() +
  NULL
