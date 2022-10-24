library(tidyverse)
library(janitor)
library(lubridate)
source("code/stream_site_id.R")

# load densities, NEON fish, and length-weight parameters from fishbase via "2_get_fish_dry_weights.R"
fish_length_cutoff = readRDS('code/fish/length_cutoffs.rds') %>% filter(animal_type == "fish") %>% pull(length_cutoff)
stream_fish_perm2 = readRDS(file = "data/derived_data/stream_fish_perm2.rds")
fish <- readRDS("data/raw_data/fish.rds")
total_lengths_with_parameters <- read_csv(file = "data/derived_data/total_lengths_with_parameters.csv") %>% 
  # rename(wet_weight = grams) %>% 
  mutate(wet_weight_units = "grams_wet",
         dw = wet_weight*0.2,           # Conversion used by McGarvey and Kirk who cite Waters 1977. Secondary production in Inland Waters
         dw_units = "grams_dry") %>% 
  filter(fish_total_length >= fish_length_cutoff)

# adjust no_perm2 to number per 0.1km or something. otherwise the densities are all <1 if units are per m2
# This allows us to sample a larger number of lengths and weights in the next step and then convert
# those back to perm2
adjustment = 1e4 # this value adjusts from m2 to something larger as described above

fish_perm2 = stream_fish_perm2 %>% 
  group_by(site_id, date) %>% 
  mutate(multiplier = adjustment) %>% 
  summarize(total_fish_perm2_adjusted = sum(total_fish_perm2*multiplier))

# get family or genus specific length weight parameters
length_weight_parameters = total_lengths_with_parameters %>% distinct(taxon_id, a, b)

# convert lengths to dry mass in grams
fish_length_weight_conversion = fish$fsh_perFish %>% clean_names() %>% as_tibble() %>% 
  mutate(date = ymd(as.Date(pass_start_time))) %>% 
  select(site_id, date, fish_total_length, scientific_name, taxon_id) %>% 
  left_join(length_weight_parameters) %>% 
  mutate(year = year(date),
         fish_total_length_cm = case_when(site_id == "MAYF" & year == 2017 ~ fish_total_length, # Some samples were measured in cm instead of mm. Fix so all are cm.
                                          site_id == "REDB" & year == 2016 ~ fish_total_length,
                                          TRUE ~ fish_total_length/10), # All other samples are in mm. This converts those to cm.
         wet_weight_grams = a*fish_total_length_cm^b) %>%  # this is the conversion from total length to grams
  mutate(wet_weight_units = "grams_wet",
         dw = wet_weight_grams*0.2,           # Conversion used by McGarvey and Kirk who cite Waters 1977. Secondary production in Inland Waters
         dw_units = "grams_dry") %>% 
  filter(fish_total_length_cm >= 1 & dw >= 0.001) %>% # remove extremely small fish. This affects <0.1% of observations
  mutate(dw = dw*1000, dw_units = "mg")

# Sample from length data according to the number of fish caught on that day
weights_and_totals = fish_length_weight_conversion %>% 
  right_join(fish_perm2) 

weight_split = weights_and_totals %>% group_split(site_id, date)


weight_sims_temp = NULL
for(i in 1:length(weight_split)){
  weight_sims_temp[[i]] = sample_n(weight_split[[i]], weight_split[[i]] %>% distinct(total_fish_perm2_adjusted) %>% pull(), replace = T)
}


fish_weights_perm2 <- bind_rows(weight_sims_temp) %>% 
  group_by(site_id, date, dw_units) %>% 
  count(dw) %>% 
  mutate(no_m2 = n/adjustment) %>% 
  select(-n) %>% 
  mutate(animal_type = "fish")

saveRDS(fish_weights_perm2, file = "data/derived_data/fish_weights_perm2.rds")


# sanity check
fish_weights_perm2 %>% 
  ggplot(aes(y = no_m2, x = dw)) + 
  geom_point() +
  facet_wrap(~site_id)



