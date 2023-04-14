library(brms)
library(tidyverse)
library(tidybayes)
library(janitor)


# three-way interaction ---------------------------------------------------

# 1) load data
gpp = readRDS("data/derived_data/gpp_means.rds") %>% rename(mean_gpp = mean, sd_gpp = sd) %>% 
  mutate(gpp_s = scale(mean_gpp))
temp = readRDS("data/derived_data/temperature_mean-annual.rds") %>% 
  rename(mean_temp = mean,sd_temp = sd) %>% 
  mutate(mat_s = scale(mean_temp))
#om = readRDS("data/derived_data/organic_matter.rds)
om = tibble(om = rnorm(n = nrow(gpp), 0, 1)) %>% 
  mutate(om_s = scale(om))
rep_peryear = 2
nyears = 5

mean_temp = mean(temp$mean_temp)
sd_temp = sd(temp$mean_temp)

data = tibble(gpp_s = gpp$gpp_s,
              mat_s = temp$mat_s,
              om_s = om$om_s,
              mat = temp$mean_temp) %>% 
  mutate(site = row_number()) %>% 
  expand_grid(rep_peryear = 1:rep_peryear,
              year = 1:nyears) %>% 
  sample_n(145) %>%  # thin to roughly the number of NEON samples (since each site is not fully sampled in the dataset)
  mutate(sample = row_number()) 

# 2) simulate priors
nsims = 200
priors = tibble(iter = 1:nsims,
                a = rnorm(nsims, -1.3, 0.1),
                beta_mat = rnorm(nsims, 0, 0.5),
                beta_om = rnorm(nsims, 0, 0.5),
                beta_gpp = rnorm(nsims, 0, 0.5),
                beta_gpp_om = rnorm(nsims, 0, 0.5),
                beta_gpp_mat = rnorm(nsims, 0, 0.5),
                beta_om_mat = rnorm(nsims, 0, 0.5),
                beta_om_mat_gpp = rnorm(nsims, 0, 0.5))



# 3) combine data and priors. Simulate lambda
prior_lambdas = data %>% 
  expand_grid(priors) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*gpp_s + beta_om*om_s +
           beta_gpp_om*gpp_s*om_s + beta_gpp_mat*gpp_s*mat_s + beta_om_mat*om_s*mat_s +
           beta_om_mat_gpp*om_s*mat_s*gpp_s)


# 4) plot
hist(prior_lambdas$lambda)

# counterfactuals
gpp_s_25 = quantile(gpp$gpp_s, probs = 0.25)
gpp_s_50 = quantile(gpp$gpp_s, probs = 0.5)
gpp_s_75 = quantile(gpp$gpp_s, probs = 0.75)
om_s_25 = quantile(om$om_s, probs = 0.25)
om_s_50 = quantile(om$om_s, probs = 0.5)
om_s_75 = quantile(om$om_s, probs = 0.75)

prior_counterfactual = tibble(mat_s = seq(min(temp$mat_s), max(temp$mat_s), length.out = 20)) %>% 
  mutate(gpp_s_50 = gpp_s_50,
         # gpp_s_25 = gpp_s_25,
         # gpp_s_75 = gpp_s_75,
         om_s = om_s_50) %>% 
  pivot_longer(contains("gpp_s"), values_to = "gpp_s") %>% 
  expand_grid(priors) %>% 
  mutate(lambda = a + beta_mat*mat_s + beta_gpp*gpp_s + beta_om*om_s +
           beta_gpp_om*gpp_s*om_s + beta_gpp_mat*gpp_s*mat_s + beta_om_mat*om_s*mat_s +
           beta_om_mat_gpp*om_s*mat_s*gpp_s) %>% 
  mutate(mat = (mat_s*sd_temp) + mean_temp)

#load_posterior plot
isd_by_temp = readRDS(file = "plots/isd_by_temp.rds") +
  labs(subtitle = "b) Posterior") + 
  coord_cartesian(ylim = c(-3, 0))

# make prior plot
isd_by_temp_prior = prior_counterfactual %>% 
  ggplot(aes(x = mat, y = lambda, group = iter)) + 
  geom_line(alpha = 0.5) + 
  # facet_wrap(~gpp_s) +
  theme_default() +
  labs(y = "\u03bb (ISD exponent)",
       x = "Mean Annual Temperature (\u00b0C)",
       subtitle = "a) Prior") + 
  coord_cartesian(ylim = c(-3, 0))

# combine prior and posterior
library(patchwork)

prior_post_isd = isd_by_temp_prior + isd_by_temp
ggview::ggview(prior_post_isd, width = 6.5, height = 3.2)
ggsave(prior_post_isd, width = 6.5, height = 3.2, units = "in", dpi = 500,
       file = "plots/prior_post_isd.jpg")
