library(neonUtilities)
library(tidyverse)
library(janitor)
library(lubridate)
library(tidybayes)
library(brms)
library(neonstore)
# library(neonDivData)

# directory
Sys.setenv(NEONSTORE_HOME = paste(getwd(), 
                                  "/data/raw_data/fish",
                                  sep=""))

# download data (takes ~15 minutes) --------------------------------
#stream sites
streamsites=c("HOPB", "LEWI", "POSE", "CUPE",
              "GUIL", "KING", "MCDI", "LECO",
              "WALK", "MAYF", "ARIK", "BLUE",
              "PRIN", "BLDE", "COMO", "WLOU", 
              "SYCA", "REDB", "MART", "MCRA",
              "BIGC", "TECR", "OKSR", "CARI")

# neon_download(product="DP1.20107.001",
#               start_date=NA,
#               end_date=NA,
#               type="basic",
#               site= NA)
# 
# neon_download(product="DP1.20190.001",
#               start_date=NA,
#               end_date=NA,
#               table = "rea_widthFieldData",
#               type="basic",
#               site= streamsites)
# 
# # stack data
# fish_stacked = stackFromStore(filepaths=neon_dir(),
#                       dpID="DP1.20107.001",
#                       package="basic",
#                       site = streamsites)
# 
# stream_widths_stacked = stackFromStore(filepaths=neon_dir(),
#                                dpID="DP1.20190.001", 
#                                package="basic",
#                                site = streamsites)
# 
# saveRDS(fish_stacked, file = "data/raw_data/fish/fish_stacked.rds")
# saveRDS(stream_widths_stacked, file = "data/raw_data/fish/stream_widths_stacked.rds")


# wrangle data ------------------
# source("data/raw_data/fish/get_pass_data.R")

# model abundance ---------------------------------------------------------
# source("data/raw_data/fish/02_fish_estimate-total-population-size.R")
# load multinomialPoisson depletion model (made with code above)
three_pass_data_wide = read_csv("data/raw_data/fish/three_pass_data_wide_total_fish.csv")
three_pass_model = readRDS(file = "data/raw_data/fish/three_pass_model.rds")
three_pass_data_species = readRDS(file = "data/raw_data/fish/three_pass_data_species.rds")

# extract posteriors and summarize
sample_1_population  = as_draws_df(three_pass_model@stanfit) %>% 
  clean_names() %>%
  select(contains("state_intercept")) %>% 
  mutate(name = "sample_1") %>% 
  rename(value = beta_state_intercept)

three_pass_population = as_draws_df(three_pass_model@stanfit) %>% 
  select(contains("_state")) %>% 
  pivot_longer(cols = !contains("ntercept")) %>% 
  clean_names() %>% 
  mutate(value = beta_state_intercept + value) %>% 
  bind_rows(sample_1_population) %>% 
  select(name, value) %>% 
  mutate(site_int = parse_number(name)) %>% #get original group names
  left_join(three_pass_data_wide %>% ungroup %>% 
              distinct(site_int, reach_id)) %>% 
  group_by(reach_id) %>% # group and summarize
  median_qi(pop_threepass = exp(value)) %>% # summarize on the probability scale (via link function)
  select(-.width, -.point, -.interval) %>% 
  rename(.lower_threepass = .lower,
         .upper_threepass = .upper) 

# get species population sizes: Split the modeled total population according
# to species relative abundances

fish <- readRDS("data/raw_data/fish/fish_stacked.rds")

reach_lengths_widths = three_pass_data_wide %>% 
  distinct(reach_id, site_int, mean_wetted_width_m, measured_reach_length)

species_population = three_pass_data_species %>% 
  group_by(reach_id, taxon_id) %>% 
  summarize(n = sum(total_fish)) %>% 
  mutate(prop_species = n/sum(n)) %>% 
  left_join(three_pass_population) %>% 
  filter(!is.na(pop_threepass)) %>% 
  filter(pop_threepass <= 10000) %>%  # filters outlier at King's Creek with >30000 estimated fish
  mutate(species_pop = prop_species*pop_threepass,
         species_low = prop_species*.lower_threepass,
         species_high = prop_species*.upper_threepass) %>% 
  left_join(reach_lengths_widths) %>%
  ungroup() %>% 
  mutate(area_m2 = measured_reach_length*mean_wetted_width_m)

species_population_selected = species_population %>% 
  separate(reach_id, into = c("site_id", "date", "reach"), remove = F) %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date)) %>% 
  select(reach_id, taxon_id, species_pop, species_low, species_high,
         site_id, area_m2, year, month, date) %>% 
  mutate(reach_taxon_id = paste(reach_id, taxon_id, sep = "_"))

saveRDS(species_population_selected, "data/raw_data/fish/species_population_selected.rds")


# fish length weight ------------------------------------------------------------
species_population_selected = readRDS("data/raw_data/fish/species_population_selected.rds")

individual_dw = fish$fsh_perFish %>% clean_names() %>% as_tibble() %>%
  mutate(year = year(pass_start_time),
         month = month(pass_start_time),
         reach_id = str_sub(event_id, 1, 16),
         reach_taxon_id = paste(reach_id, taxon_id, sep = "_")) %>% 
  mutate(dw = fish_weight*0.2) %>% 
  select(reach_taxon_id, dw) 

saveRDS(individual_dw, file = "data/derived_data/individual_dw_fish.rds")

# Sample from dw measurements with replacement.
# Number of samples = total number of fish caught per pass per species.
# If 4 fish caught, then sample the 4 lengths from the 50 length measurements
# If 400 fish caught, then sample 400 lengths (with replacement) from the 50 length measurements

# simulate dw totals
size_and_totals = individual_dw %>% 
  # left_join(total_fish) %>%
  left_join(species_population_selected) %>% 
  mutate(fish_perm2 = species_pop/area_m2,
         fish_per10000m2 = fish_perm2*10000) %>% 
  group_by(reach_taxon_id) %>% 
  filter(!any(is.na(fish_perm2)))

# sample individual dry weight with replacement. Sample sizes for individual weights
# are weighted by the population size of a given species

dw_sims = size_and_totals %>% 
  group_by(reach_taxon_id) %>% 
  filter(!is.na(dw)) %>% 
  sample_n(size = fish_per10000m2[1], replace = T) %>% 
  group_by(dw, reach_taxon_id, year, month) %>% 
  dplyr::count(name = "no_10000m2") %>% 
  mutate(no_m2 = no_10000m2/10000,
         animal_type = "fish") 


fish_dw_taxa = dw_sims %>% 
  ungroup %>% 
  separate(reach_taxon_id, into = c("reach_id", "taxon_id"), sep = "_") %>% 
  separate(reach_id, into = c("site_id", "date", NA), remove = F) %>% 
  mutate(date = ymd(date),
         julian = julian(date),
         year_month = paste(year, month, sep = "_")) %>% 
  group_by(dw,reach_id, site_id, year, month, julian, animal_type, year_month, taxon_id) %>% # Sum body size abundance regardless of fish taxon
  summarize(no_m2 = sum(no_m2)) %>% 
  mutate(event_id = paste(site_id, year_month, animal_type, sep = "_"),
         dw = dw*1000,
         dw_units = "mg") %>% 
  saveRDS(., file = "data/derived_data/fish_dw_taxa.rds")


fish_dw = dw_sims %>% 
  ungroup %>% 
  separate(reach_taxon_id, into = c("reach_id", "taxon_id"), sep = "_") %>% 
  separate(reach_id, into = c("site_id", "date", NA), remove = F) %>% 
  mutate(date = ymd(date),
         julian = julian(date),
         year_month = paste(year, month, sep = "_")) %>% 
  group_by(dw,reach_id, site_id, year, month, julian, animal_type, year_month) %>% # Sum body size abundance regardless of fish taxon
  summarize(no_m2 = sum(no_m2)) %>% 
  mutate(event_id = paste(site_id, year_month, animal_type, sep = "_"),
         dw = dw*1000,
         dw_units = "mg") 

saveRDS(fish_dw, file = "data/derived_data/fish_dw-allyears.rds")



# number of fish collected
three_pass_data_wide %>% 
  mutate(total = `1` + `2` + `3`) %>% 
  summarize(total = sum(total))

# number of fish measured for mass
fish$fsh_perFish %>% as_tibble() %>% 
  filter(!is.na(fishWeight)) %>% 
  nrow(.)

# number of species
length(unique(three_pass_data_species$taxon_id))


# old code from when we used lengths --------------------------------------

# total_fish = readRDS(file = "data/derived_data/fish_fish-abundance.rds") # modeled fish abundances

# CHECK FOR SAMPLING BIAS. NONE APPARENT WITH FIELD DRY MASSES, BUT CLEAR BIAS WITH LENGTHS. 
# THEREFORE WE WILL USE THE FIELD DRY MASSES INSTEAD OF LENGTH-WEIGHT REGRESSION.
# genus_to_family = read_csv(file = "data/raw_data/fish/total_lengths_with_parameters.csv") %>% 
#   ungroup %>% distinct(Family, Species, scientific_name, taxon_id) %>% 
#   separate(scientific_name, into = c("genus", "species"), remove = F) %>% 
#   filter(!is.na(Species)) %>% 
#   distinct(genus, Family) %>%
#   clean_names 
# 
# lw_parameters_per_family <- read_csv(file = "data/raw_data/fish/total_lengths_with_parameters.csv") %>% 
#   ungroup %>% distinct(Family, Species, scientific_name, taxon_id, a, b) %>% 
#   separate(scientific_name, into = c("genus", "species"), remove = F) %>%
#   left_join(genus_to_family) %>% 
#   mutate(family_genus = paste(family,genus, sep = "_")) %>% 
#   distinct(family_genus, a, b) %>% 
#   group_by(family_genus) %>% 
#   summarize(a = mean(a),
#             b = mean(b))
# 
# fish_lengths = fish$fsh_perFish %>% as_tibble() %>% clean_names()  %>% 
#   distinct() %>%  # remove duplicates (duplicates confirmed via email with NEON on 2022-01-06)
#   mutate(date = ymd(as.Date(pass_start_time)),
#          year = year(date)) %>%
#   separate(event_id, c("site", "date2", "reach", NA, NA), 
#            remove = F) %>% 
#   unite("reach_id", site:reach, sep = ".") %>% 
#   separate(event_id, c(NA, NA, "reach", NA, NA, NA), remove = F) %>% 
#   ungroup() %>% 
#   # semi_join(fixed_ids) %>% 
#   filter(site_id %in% streamsites) %>% 
#   separate(scientific_name, into = c("genus", "species")) %>% 
#   left_join(genus_to_family) %>% 
#   mutate(family_genus = paste(family,genus, sep = "_")) %>% 
#   # filter(fish_total_length >= fish_length_cutoff) %>%
#   left_join(lw_parameters_per_family) 
# 
# fish_dw_temp = fish_lengths %>%
#   mutate(a = case_when(is.na(family) ~ 0.00785, 
#                        TRUE ~ a),
#          b = case_when(is.na(family) ~ 3.06,
#                        TRUE ~ b),
#          month = month(date),
#          year = year(date)) %>% 
#   mutate(fish_total_length_cm = fish_total_length/10,
#          wet_weight_estimated = a*fish_total_length_cm^b,
#          dw_estimated = wet_weight_estimated*0.2,
#          dw = fish_weight*0.2,           # Conversion used by McGarvey and Kirk who cite Waters 1977. Secondary production in Inland Waters
#          dw_units = "grams_dry",
#          site_fac = paste(site_id, year, month, taxon_id, sep = "_"))

# compare_field_to_lw = fish_dw_temp %>% 
#   ggplot(aes(x = dw_estimated, y = dw, color = family)) + 
#   geom_point(size = 0.2) + 
#   scale_y_log10(limits = c(0.01, 1000)) +
#   scale_x_log10(limits = c(0.01, 1000)) +
#   facet_wrap(~family) +
#   geom_abline() + 
#   guides(color = "none") +
#   labs(y = "dw measured in the field",
#        x = "dw from length-weight equations") +
#   theme_default()
# 
# saveRDS(compare_field_to_lw, file = "plots/fish_compare-mass-methods.RDS")
# ggsave(compare_field_to_lw, file = "plots/fish_compare-mass-methods.jpg")
# 
# sampling_bias = fish_dw_temp %>%
#   pivot_longer(cols = c(fish_total_length_cm, dw)) %>% 
#   ungroup() %>% 
#   group_by(value, name) %>% 
#   tally() %>% 
#   ggplot(aes(y = n, x = value)) + 
#   geom_point() +
#   facet_wrap(~name, scales = "free") +
#   scale_x_log10() + 
#   scale_y_log10() +
#   theme_default()
# 
# saveRDS(sampling_bias, file = "plots/fish_sampling-bias.RDS")
# ggsave(sampling_bias, file = "plots/fish_sampling-bias.jpg", width = 6, height = 3)

fish$fsh_perFish %>% 
  mutate(dw = fishWeight*0.2) %>%
  group_by(dw) %>% 
  add_tally() %>% 
  filter(dw >= 0.5) %>% 
  ggplot(aes(x = dw, y = n)) + 
  geom_point() +
  scale_x_log10() + 
  scale_y_log10() + 
  geom_smooth(method = "lm")
