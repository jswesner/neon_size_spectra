library(tidyverse)

E = -0.65 # From West, Brown, Enquist - "WBE", units are eV (electron volts)
k = 8.62*10^-5 # Boltzmann's constant
temp_c = 15
temp_k = temp_c + 273.15   # convert celsius to kelvin by adding 273.15 to the temp in celsius
M = 2 # mean body size (I think, via Brown et al. 2004)
r0 = 100 # resource supply rate (perhaps like GPP g/c/m2/y?)

theme_set(brms::theme_default())

Btot = r0*exp(E/(k*temp_k))*M^0.25

temps = c(1, 10, 20, 30)
M = c(0.01, 0.1, 1, 10)
r0 = c(0.1, 1, 10, 100, 1000)

tibble(E = -0.65, # From West, Brown, Enquist - "WBE", units are eV (electron volts)
       k = 8.62*10^-5, # Boltzmann's constant
       temp_c = 15,
       temp_k = temp_c + 273.15,   # convert celsius to kelvin by adding 273.15 to the temp in celsius
       M = 2, # mean body size (I think, via Brown et al. 2004)
       r0 = 100)



btot_sims = tibble(E = -0.65, 
       k = 8.62*10^-5) %>% 
  expand_grid(temp_c = temps,
              M = M,
              r0 = r0) %>% 
  mutate(temp_k = temp_c + 273.15,
         Btot = r0*exp(E/(k*temp_k))*(M^0.25),
         inverse_temp = 1/(k*temp_k))
       

btot_sims %>% 
  ggplot(aes(x = inverse_temp, y = Btot)) + 
  geom_point() +
  geom_line(aes(group = M)) +
  facet_wrap(~r0) +
  scale_y_log10()

dat_all = readRDS("data/derived_data/dat_all.rds")

mte_mass_preds = dat_all %>% 
  group_by(site_id, temp_mean, gpp, mean_om) %>% 
  reframe(M = exp(mean(log(dw)))) %>% 
  mutate(E = -0.65, 
         k = 8.62*10^-5,
         temp_c = temp_mean,
         r0 = gpp + mean_om) %>% 
  expand_grid(rep = 1:100) %>% 
  mutate(scaling_r = rnorm(nrow(.), 0.25, 0.01),
         E_r = rnorm(nrow(.), -0.65, 0.05)) %>% 
  mutate(temp_k = temp_c + 273.15,
         Btot_r = r0*exp(E_r/(k*temp_k))*(M^scaling_r),
         Btot = r0*exp(E/(k*temp_k))*(M^0.25),
         inverse_temp = 1/(k*temp_k))


mte_mass_preds %>% 
  ggplot(aes(x = site_id, y = Btot*1e9)) + 
  geom_point() +
  scale_y_log10()

mte_mass_preds %>% 
  # filter(rep == 50) %>%
  pivot_longer(cols = c(Btot_r, Btot), names_to = "method", values_to = "Btot") %>% 
  ggplot(aes(x = inverse_temp, y = Btot*1e9, color = method)) + 
  geom_point() +
  scale_y_log10() +
  # theme_default() +
  # theme(axis.text = element_blank(),
        # axis.title = element_blank()) +
  facet_wrap(~method) +
  NULL



# isd ---------------------------------------------------------------------
n = 1000
lambda_sims = tibble(a = rbeta(n, 1, 5),
       b = rlnorm(n, log(10), 0.5),   # from Perkins et al. 2018
       scaling = rnorm(n, 0.66, 0.3), # from Glazier 2005 (estimated to mimic figure 1 for non-pelagic)
       loga = log10(a),
       logb = log10(b),
       subsidy_effect = runif(n, 0.2, 0.5)) %>% # from Perkins et al. 2018 Fig 3a and Hocking (0.2-0.3 effect)
  # filter(a > 0) %>% 
  mutate(lambda = (loga/logb) - scaling - 1,
         lambda_subsidies = lambda + subsidy_effect)

lambda_sims %>% 
  pivot_longer(cols = c(lambda, lambda_subsidies)) %>% 
  ggplot(aes(x = value, fill = name)) + 
  geom_histogram() +
  geom_vline(xintercept = c(-2,-1.22))


