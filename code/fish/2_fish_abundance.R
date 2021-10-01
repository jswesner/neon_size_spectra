library(tidyverse)
library(janitor)
library(lubridate)
library(rfishbase)

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
         grams = a*fish_total_length_cm^b)

total_lengths_with_parameters %>% 
  ggplot(aes(x = fish_total_length_cm, y = grams)) +
  geom_point()

