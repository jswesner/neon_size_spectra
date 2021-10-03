library(tidyverse)
library(janitor)
library(lubridate)
library(rfishbase)

# load or re-run with code below. total_lengths_with_parameters contains
# estimates of fish mass, corrected for collection size. This file will be merged directly with 
# macroinvertebrate size spectrum.
total_lengths_with_parameters <- read_csv(file = "data/derived_data/total_lengths_with_parameters.csv") %>% 
  rename(wet_weight = grams) %>% 
  mutate(wet_weight_units = "grams_wet",
         dw = wet_weight*0.2,           # Conversion used by McGarvey and Kirk who cite Waters 1977. Secondary production in Inland Waters
         dw_units = "grams_dry")
#
#
#
# Recreate file above with code below


# load data
fishbase_families <- fishbase %>% as_tibble() %>% mutate(Species = paste(Genus, Species)) %>% 
  select(Species, Family) %>% 
  mutate(scientific_name = Species)

stream_fish_perm2 <- readRDS(file = "data/derived_data/stream_fish_perm2.rds") %>% 
  left_join(fishbase_families) # number of fish collected per m2 per site per date per species. Fixed reaches only

species_we_have <- pull(stream_fish_perm2 %>% distinct(scientific_name))

fish <- readRDS("data/raw_data/fish.rds")

length_weight_all <- length_weight(species = species_we_have) %>% # length_weight parameters from fishbase
  left_join(fishbase_families) %>% 
  mutate(scientific_name = Species) 

length_weights_by_species_initial <- length_weight_all %>% filter(Type == "TL" &
                                                 !Sex %in% c("Male", "Female", "male", "female")) %>% 
  group_by(Species) %>% 
  mutate(species_a = median(a),
         species_b = median(b)) %>% 
  distinct(Species, species_a, species_b)

length_weights_by_family <- length_weight_all %>% filter(Type == "TL" &
                                                 !Sex %in% c("Male", "Female", "male", "female")) %>% 
  group_by(Family) %>% 
  mutate(family_a = median(a),
         family_b = median(b)) %>% 
  ungroup() %>% 
  distinct(Family, family_a, family_b)

# need to get l-w parameters by shape for these species, then add to length_weight_by_species
species_to_get <- pull(stream_fish_perm2 %>% ungroup()%>% distinct(Species, Family) %>% anti_join(length_weights_by_family) %>% 
                         select(Species))

# add manual parameters from here:https://fishbase.us/popdyn/BayesianAnalysis.php
shape_to_get <- species(species_list = species_to_get) %>% select(Species, BodyShapeI) %>% 
  mutate(species_a = case_when(grepl("eel-lik", BodyShapeI) ~ 10^-2.99,
                               TRUE ~ 10^-1.95,
                               ),
         species_b = case_when(grepl("eel-lik", BodyShapeI) ~ 3.06,
                               TRUE ~ 3.04))
                               
length_weights_by_species <- bind_rows(length_weights_by_species_initial, shape_to_get)

# length data for fish
total_lengths <- fish$fsh_perFish %>% as_tibble() %>% 
  mutate(date = as_date(passStartTime)) %>% 
  clean_names() %>% 
  select(taxon_id, fish_weight, scientific_name, site_id, date, fish_total_length, pass_number) %>% 
  left_join(fishbase_families)

total_lengths_with_parameters <- total_lengths %>%
  filter(pass_number <= 3) %>%
  arrange(fish_total_length) %>% 
  group_by(site_id, date, taxon_id, scientific_name, fish_total_length) %>% 
  tally() %>% 
  group_by(taxon_id, scientific_name, site_id, date) %>% 
  mutate(total_fish_measured = sum(n)) %>% 
  right_join(stream_fish_perm2 %>% select(-total_fish_perm2)) %>% 
  mutate(correction = no/total_fish_measured) %>% 
  mutate(total_fish_per_size = total_fish_measured*correction,
         no_m2 = total_fish_per_size/area_m2) %>% 
  left_join(fishbase_families) %>% 
  left_join(length_weights_by_species) %>% 
  left_join(length_weights_by_family) %>% 
  mutate(a = case_when(is.na(species_a) ~ family_a, TRUE ~ species_a),
         b = case_when(is.na(species_b) ~ family_b, TRUE ~ species_b),
         fish_total_length_cm = fish_total_length/10,
         grams = a*fish_total_length_cm^b) %>% 
  rename(wet_weight = grams) %>% 
  mutate(wet_weight_units = "grams_wet",
         dw = wet_weight*0.2,           # Conversion used by McGarvey and Kirk who cite Waters 1977. Secondary production in Inland Waters
         dw_units = "grams_dry")

write_csv(total_lengths_with_parameters , 
          file = "data/derived_data/total_lengths_with_parameters.csv")

#
total_lengths_with_parameters %>% 
  ggplot(aes(x = fish_total_length_cm*10, y = grams)) +
  geom_point()


ggplot(fish$fsh_perFish, aes(x = fishTotalLength, y = fishWeight)) + 
  geom_point()

# sanity check. Compare modeled weights to measured weights

modeled_weights = total_lengths_with_parameters %>% select(wet_weight) %>% mutate(source = "modeled")
measured_weights = fish$fsh_perFish %>% select(fishWeight) %>% mutate(source = "measured") %>% rename(wet_weight = fishWeight)

all_weights = bind_rows(modeled_weights, measured_weights)

all_weights %>% 
  ggplot(aes(x = wet_weight, fill = source)) +
  geom_histogram(bins = 100) +
  scale_x_log10()

all_weights %>%
  ggplot(aes(x = source, y = wet_weight + 0.01)) +
  geom_point(position = position_jitter(width = 0.2), size = 0.1) + 
  geom_violin() +
  scale_y_log10()


all_weights %>%
  group_by(source) %>% 
  mutate(rank = rank(-wet_weight)) %>% 
  ggplot(aes(x = rank, y = wet_weight + 0.01, color = source)) +
  geom_point(size = 0.1) + 
  scale_y_log10() +
  scale_x_log10() +
  NULL
