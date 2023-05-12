library(tidyverse)
library(posterior)

# abiotic
dat_all = readRDS("data/derived_data/dat_all.rds") %>% ungroup

dat_all %>%
  distinct(site_id, temp_mean, gpp, mean_om) %>% 
  write_csv(., file = "tables/summary_abiotic.csv")


# isd regression
fishinvertmod = readRDS("models/stan_gppxtempxom2023-05-10.rds")

isd_summary = as_draws_df(fishinvertmod) %>% 
  select("a", starts_with("beta"), starts_with("sigma")) %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(strlength = str_length(name),
         start = str_sub(name, 1, 1),
         model = "ISD") %>%  
  arrange(start, strlength) %>% 
  select(-.width, -.point, -.interval, -strlength, -start)

# community mass
community_mass_brm = readRDS("models/community_mass_brm.rds")

mass_summary = as_draws_df(community_mass_brm) %>% 
  select(starts_with("b"), starts_with("sd"), "sigma") %>% 
  pivot_longer(everything()) %>% 
  group_by(name) %>% 
  median_qi(value) %>% 
  mutate(strlength = str_length(name),
         start = str_sub(name, 1, 1),
         model = "Standing Stock Biomass") %>% 
  arrange(start, strlength) %>% 
  select(-.width, -.point, -.interval, -strlength, -start)

isd_mass_summary = bind_rows(isd_summary, mass_summary)
write_csv(isd_mass_summary, file = "tables/isd_mass_summary.csv")

# compare to literature values
abiotic = dat_all %>%
  distinct(site_id, temp_mean, gpp, mean_om)

abiotic %>% 
  filter(temp_mean == min(temp_mean) | temp_mean == max(temp_mean)) %>% 
  mutate(lamberti_1997 = c(3.5, 5400),
         range = c("min", "max")) %>% 
  pivot_longer(cols = c(temp_mean, lamberti_1997)) 

