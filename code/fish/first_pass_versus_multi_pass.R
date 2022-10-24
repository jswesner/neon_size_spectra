library(FSA)
library(tidyverse)
library(janitor)
library(lubridate)
source("code/stream_site_id.R")
library(rjags)
library(coda)
library(nlist)


# frequentist estimates ---------------------------------------------------

stream_fish_abund = readRDS(file = "code/fish/stream_fish_abund.rds")

total_abund = stream_fish_abund %>% 
  select(site_id, date, reach, data, taxon_id) %>% 
  unnest(cols = data) %>% 
  mutate(run = rep(1:3, 1276)) %>% 
  group_by(site_id, date, reach, run) %>% 
  summarize(total_collected = sum(total_fish))

# data to estimate densities with
total_abund_fsa = total_abund %>% 
  ungroup() %>% 
  select(-run) %>% 
  nest_by(site_id, reach, date) %>%
  mutate(pop = lapply(data, function(fish) removal(fish, just.ests = T, method = "CarleStrub"))) %>% 
  unnest_wider(pop) %>% 
  clean_names() 



# bayesian estimates ------------------------------------------------------

# data
stream_fish_perm2 = readRDS(file = "data/derived_data/stream_fish_perm2.rds")

stream_fish_with_passes = stream_fish_perm2 %>% unnest(cols = c(data)) %>% 
  group_by(taxon_id, site_id, date, total_fish_perm2, area_m2) %>% 
  mutate(pass = 1:3) %>% 
  mutate(total_fish_perpass_perm2 = total_fish/area_m2)

stream_fish_with_passes %>% 
  filter(pass == 1) %>% 
  ggplot(aes(x = total_fish_perpass_perm2, y = total_fish_perm2, group = interaction(date, taxon_id))) + 
  geom_point() + 
  # scale_x_log10() + 
  # scale_y_log10() + 
  facet_wrap(~site_id, scales = "free") +
  geom_smooth(method = "lm") + 
  labs(x = "Number of fish in first pass (per square meter)",
       y = "3-pass removal population estimate (per square meter)",
       title = "Population estimates from all NEON fish sampling from 2016 to 2021",
       subtitle = "Blue lines = Linear regression for each species\nBlack lines = 1:1") + 
  geom_abline()



# jags model
# 1) make data
y = stream_fish_with_passes %>%
  ungroup() %>%
  mutate(site_no = as.integer(as.factor(site_id))) %>% 
  # filter(site_no ==4) %>% 
  select(site_id, pass, total_fish, date) %>%
  group_by(site_id) %>% 
  # filter(date == max(date)|
           # date == min(date)) %>%
  group_by(site_id, date, pass) %>% 
  summarize(total_fish = sum(total_fish)) %>% 
  ungroup() %>% 
  mutate(sample = paste0(site_id, date)) %>% 
  mutate(Nsites = as.integer(as.factor(sample)),
         Npass = as.integer(pass),
         y = total_fish) %>% 
  select(Nsites, Npass, y) %>% 
  pivot_wider(names_from = Npass, values_from = y) %>% 
  select(-Nsites) %>% 
  as.matrix()

y_firstpass_only = y[,1] %>% as.matrix() # same as above, but only the first pass

# 2) pass models to jags
model.fit <- jags.model(file="code/fish/models/test_data/test_multipass_jags.txt",
                        data = list('y' = y))

model.fit.first <- jags.model(file="code/fish/models/test_data/test_multipass_jags.txt",
                        data = list('y' = y_firstpass_only))

# 3) sample the posterior
n_post_all = jags.samples(model.fit, variable.names = c("q", "r", "n"), n.iter = 1000)
n_post_firstonly = jags.samples(model.fit.first, variable.names = c("q", "r", "n"), n.iter = 1000)

# 4) convert to mcmc.list
post_n = as.mcmc.list(n_post_all$r)
post_n_firstonly = as.mcmc.list(n_post_firstonly$r)

# 5) summarize means and CrI's
post_means = tidy(post_n, simplify = T)
post_means_firstonly = tidy(post_n_firstonly, simplify = T)

# 6) plot abundance estimates

combined_posts = post_means %>% mutate(model = "three_passes",
                                       site = 1:nrow(.)) %>% 
  bind_rows(post_means_firstonly %>% mutate(model = "first_pass_only",
                                  site = 1:nrow(.)))

library(ggthemes)
combined_posts %>% 
  ggplot(aes(x = reorder(site, -estimate), y = estimate, color = model)) + 
  geom_point(position = position_dodge(width = 0.8), aes(shape = model)) +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 position = position_dodge(width = 0.8)) +
  theme_classic() + 
  scale_color_colorblind()


# add estimates using the detection probabilities from 3-pass
post_q_means = tidy(as.mcmc.list(n_post_all$q), simplify = T)

catch_prob_ests = tibble(first_pass_n = y_firstpass_only,
       q = post_q_means$estimate[1],
       qupper = post_q_means$upper[1],
       qlower = post_q_means$lower[1]) %>% 
  mutate(estimate = first_pass_n/(1-q),
         upper = first_pass_n/(1-qlower),
         lower = first_pass_n/(1-qupper)) %>% 
  mutate(model = "catch_prob*first_pass",
         site = 1:nrow(.))

sites = stream_fish_with_passes %>%
  ungroup() %>%
  mutate(site_no = as.integer(as.factor(site_id))) %>% 
  # filter(site_no ==4) %>% 
  select(site_id, pass, total_fish, date) %>%
  group_by(site_id) %>% 
  # filter(date == max(date)|
  # date == min(date)) %>%
  group_by(site_id, date, pass) %>% 
  summarize(total_fish = sum(total_fish)) %>% 
  ungroup() %>% 
  mutate(sample = paste0(site_id, date)) %>% 
  mutate(Nsites = as.integer(as.factor(sample)),
         Npass = as.integer(pass),
         y = total_fish) %>% 
  distinct(sample, Nsites) %>% 
  rename(site = Nsites)

combined_posts %>% 
  bind_rows(catch_prob_ests) %>% 
  left_join(sites) %>% 
  mutate(site_id = str_sub(sample, 1, 4)) %>% 
  ggplot(aes(x = reorder(site, estimate), y = estimate, color = model)) + 
  geom_point(position = position_dodge(width = 0.8), aes(shape = model)) +
  geom_linerange(aes(ymin = lower, ymax = upper),
                 position = position_dodge(width = 0.8)) +
  theme_classic() + 
  scale_color_colorblind() +
  facet_wrap(~site_id, scales = "free") + 
  coord_flip() + 
  scale_y_log10()

saveRDS(combined_posts, file = "code/fish/models/test_data/combined_posts.rds")


# compare frequentist and bayes -------------------------------------------

freq_bayes = total_abund_fsa %>% mutate(sample = paste0(site_id, date)) %>% 
  left_join(sites) %>% 
  select(sample, site, no, no_lci, no_uci) %>% 
  rename(estimate = no,
         lower = no_lci,
         upper = no_uci) %>% 
  mutate(model = "frequentist") %>% 
  bind_rows(combined_posts) %>% 
  bind_rows(catch_prob_ests) %>% 
  left_join(sites) %>% 
  mutate(site_id = str_sub(sample, 1, 4))

freq_only = freq_bayes %>% 
  filter(model == "frequentist")

freq_only %>% 
  select(site, estimate) %>% 
  rename(freq = estimate) %>% 
  left_join(freq_bayes %>% 
              filter(model == "catch_prob*first_pass") %>% 
              select(site, estimate) %>% 
              rename(catch_probXfirstpass = estimate)) %>%
  # filter(freq<= 6e5) %>% 
  ggplot(aes(y = catch_probXfirstpass, x = freq)) + 
  geom_point(alpha = 0.6) +
  scale_y_log10(limits = c(NA, 1e6)) + 
  scale_x_log10(limits = c(NA, 1e6)) +
  geom_abline()
