library(ubms)
library(unmarked)
library(tidyverse)

# load data and check
fish <- readRDS("data/raw_data/fish.rds")
stream_fish_perm2 = readRDS(file = "data/derived_data/stream_fish_perm2.rds")

stream_fish_with_passes = stream_fish_perm2 %>% unnest(cols = c(data)) %>% 
  group_by(taxon_id, site_id, date, total_fish_perm2, area_m2) %>% 
  mutate(pass = 1:3) %>% 
  mutate(total_fish_perpass_perm2 = total_fish/area_m2)

ubms_data = stream_fish_with_passes %>% 
  group_by(site_id, date, pass, taxon_id) %>% 
  summarize(total = sum(total_fish)) %>%
  pivot_wider(names_from = pass, values_from = total) %>% 
  mutate(site_date = paste0(site_id, date)) %>% 
  ungroup() %>% 
  mutate(site_int = 1:nrow(.)) %>% 
  filter(site_int <= 300) %>% 
  mutate(site_int = as.factor(site_int))

sites = ubms_data %>% distinct(site_id, site_int, taxon_id)

ubms_data %>% 
  pivot_longer(cols = c(`1`, `2`, `3`)) %>% 
  ggplot(aes(x = name, y = value)) + 
  geom_jitter(width = 0.2, height = 0)


ubms_sites = ubms_data %>% select(site_int)

ubms_data_matrix = ubms_data %>% select(`1`, `2`, `3`) %>% as.matrix()

ovenFrame <- unmarkedFrameMPois(ubms_data_matrix,
                                siteCovs= data.frame(site_int = ubms_sites$site_int),
                                type = "removal")

fm1_species <- stan_multinomPois(~ site_int ~ site_int, ovenFrame,
                         chains = 1, iter = 300)



fm1_species = update(fm1_species, iter = 1000, chains = 4)
saveRDS(fm1_species, file = "code/fish/models/stan/fm1_species.rds")

library(tidybayes)
library(brms)
fm_draws = as_draws_df(fm1_species@stanfit)
# abundance
draws_long = fm_draws %>% as_tibble() %>% 
  select(contains("beta_state")) %>% 
  mutate(draws = 1:nrow(.)) %>% 
  pivot_longer(cols = -c(contains("Intercept"), "draws")) %>% 
  mutate(abundance = exp(`beta_state[(Intercept)]` + value)) %>% 
  mutate(site_int = as.integer(parse_number(name)))

bayes_freq = draws_long %>% 
  group_by(site_int) %>% 
  median_qi(abundance) %>%
  mutate(model = "3-pass, UBMS Package (Bayesian)") %>% 
  # bind_rows(freq_only %>% mutate(model = "3-pass, FSA package (Frequentist)") %>% 
  #             select(site, estimate, lower, upper, model, site) %>% 
  #             rename(abundance = estimate, 
  #                    .lower = lower,
  #                    .upper= upper,
  #                    site_int = site)) %>% 
  left_join(sites %>% mutate(site_int = as.integer(parse_number(as.character(site_int))))) 
  

bayes_freq %>% 
  ggplot(aes(y = reorder(taxon_id, abundance), color = model)) + 
  geom_point(aes(x = abundance)) + 
  geom_linerange(aes(xmin = .lower, xmax = .upper)) + 
  # scale_x_log10(limits = c(1e-1, 1e6)) + 
  facet_wrap(~site_id, scales = "free")


bayes_freq %>% 
  ggplot(aes(y = reorder(taxon_id, abundance), x = abundance, color = model)) + 
  geom_point(position = position_dodge(width = 0.7)) + 
  scale_x_log10()



# capture probability
pdraws_long = fm_draws %>% as_tibble() %>% 
  select(contains("beta_det")) %>% 
  mutate(draws = 1:nrow(.)) %>% 
  pivot_longer(cols = -c(contains("Intercept"), "draws")) %>% 
  mutate(p = inv_logit_scaled(`beta_det[(Intercept)]` + value)) %>% 
  mutate(site_int = as.integer(parse_number(name)))

pdraws_long %>% 
  group_by(site_int) %>% 
  median_qi(p) %>% 
  ggplot(aes(y = reorder(site_int, p))) + 
  geom_point(aes(x = p)) + 
  geom_linerange(aes(xmin = .lower, xmax = .upper)) + 
  coord_cartesian(xlim = c(0.9, 1))



p_first = pdraws_long %>% 
  group_by(site_int) %>% 
  median_qi(p) %>% 
  left_join(ubms_data %>% select(`1`, site_int, site_id) %>% mutate(site_int = as.integer(site_int))) %>% 
  mutate(abundance = `1` / p,
         first_abund = as.integer(`1`),
         model = "1-pass (corrected for detection probability)",
         site_fac = as.factor(site_int))

p_first %>% 
  ggplot(aes(x = reorder(site_int, abundance), y = abundance)) + 
  geom_point() + 
  scale_y_log10()


pois_pfirst = brm(first_abund ~ site_fac + offset(p),
                  family = poisson(link = "log"),
                  data = p_first,
                  prior = c(prior(normal(0, 1), class = "b"),
                            prior(normal(4, 4), class = "Intercept")),
                  chains = 1, iter = 1000)


p_first_pois = pois_pfirst$data %>% 
  add_epred_draws(pois_pfirst) %>% 
  group_by(site_fac) %>% 
  median_qi(.epred) %>% 
  mutate(site_int = as.integer(parse_number(as.character(site_fac))),
         model = "one pass corrected") %>% 
  left_join(bayes_freq %>% distinct(taxon_id, site_id, site_int))

all_three = bayes_freq %>% 
  bind_rows(p_first_pois %>% 
              select(site_int, .epred, .lower, .upper, model, site_id, taxon_id) %>% 
              rename(abundance = .epred))

method_comparison = all_three %>%
  distinct(site_int, model, site_id, .keep_all = T) %>% 
  ggplot(aes(y = reorder(site_int, abundance), color = model)) + 
  geom_point(aes(x = abundance)) + 
  # geom_linerange(aes(xmin = .lower, xmax = .upper)) +
  scale_x_log10() +
  facet_wrap(~model, ncol = 1) +
  theme(axis.text.y = element_blank()) + 
  labs(y = "Electrofishing sample ordered by abundance",
       x = "Total Number of Fish in reach")

ggsave(method_comparison, file = "code/fish/models/three_pass_vs_one_pass/method_comparison_species.jpg",
       width = 5, height = 5)
saveRDS(method_comparison, file = "code/fish/models/three_pass_vs_one_pass/method_comparison_species.rds")

regress_three_one = all_three %>% 
  distinct(site_int, model, site_id, taxon_id, .keep_all = T) %>% 
  select(site_int, site_id, abundance, model, taxon_id) %>%
  pivot_wider(names_from = model, values_from = abundance) %>%
  # pivot_longer(cols = c(-site_int, -site_id, -contains("UBMS"))) %>% 
  rename(three_pass_bayes = contains("UBMS"),
         one_pass_corrected_for_detection = contains("one pass")) %>% 
  ggplot(aes(y = three_pass_bayes, x = one_pass_corrected_for_detection)) + 
  geom_point(shape = 21) + 
  # scale_x_log10(limits = c(1e-01, 1e4)) + 
  # scale_y_log10(limits = c(1e-01, 1e4)) +
  geom_abline() + 
  labs(title = "Single-pass accruately predicts species-level populations\nwhen corrected for detection probabilities") +
  theme_default()

saveRDS(regress_three_one, file = "code/fish/models/three_pass_vs_one_pass/regress_three_one_species.rds")

