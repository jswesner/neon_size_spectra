library(neonUtilities)
library(tidyverse)
library(janitor)
library(lubridate)
library(tidybayes)
library(brms)
library(neonstore)
library(neonDivData)

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
fish <- readRDS("data/raw_data/fish/fish_stacked.rds")
stream_widths <- readRDS("data/raw_data/fish/stream_widths_stacked.rds") %>% .[9] %>% # selects rea_widthFieldData
  bind_rows() %>% clean_names() %>% 
  group_by(site_id) %>% 
  summarize(mean_width_m = mean(wetted_width, na.rm = T))

reach_lengths_widths <- fish$fsh_fieldData %>% clean_names %>% 
  mutate(year = year(start_date),
         month = month(start_date)) %>%
  filter(is.na(sampling_impractical)) %>% 
  filter(fixed_random_reach == "fixed") %>% 
  filter(!is.na(measured_reach_length)) %>% 
  distinct(reach_id, measured_reach_length, year, month, site_id) %>% 
  left_join(stream_widths) %>% 
  mutate(area_m2 = measured_reach_length*mean_width_m)

# fsh_perFish individual row for first 50 fish
perfish <- fish$fsh_perFish %>% as_tibble() %>% clean_names() %>% 
  select(-identified_by, -fish_weight, -uid) %>% # remove these columns to allow removal of duplicates
  distinct() %>%  # remove duplicates (duplicates confirmed via email with NEON on 2022-01-06)
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c("site", "date2", "reach", NA, NA), 
           remove = F) %>% 
  unite("reach_id", site:reach, sep = ".") %>% 
  group_by(taxon_id, reach_id, scientific_name, site_id, 
           pass_number, event_id, date, year, named_location) %>% 
  tally() %>% 
  separate(event_id, c(NA, NA, "reach", NA, NA, NA), remove = F) %>% 
  ungroup() 

# bulk count of additional fish after the first 50 in fsh_perFish
bulk_count <- fish$fsh_bulkCount %>% as_tibble() %>% clean_names() %>% 
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c("site", "date2", "reach", NA, NA), 
           remove = F) %>% 
  unite("reach_id", site:reach, sep = ".") %>% 
  # semi_join(fixed_ids) %>% 
  select(-uid, -identified_by) %>%
  distinct() # remove duplicates (confirmed that these were true duplicates on 2022-01-06 using get_dupes())

# all fish - fsh_perFish PLUS fsh_bulkCount
all_fish <- left_join(perfish, bulk_count) %>% 
  replace_na(list(bulk_fish_count = 0)) %>% 
  mutate(total_fish = n + bulk_fish_count) %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  filter(pass_number <= 3)

# final raw abundance data
stream_fish_torun <- all_fish %>% 
  select(site_id, pass_number, taxon_id, total_fish, reach, date, reach_id) %>% 
  arrange(reach, pass_number) %>%
  complete(pass_number, nesting(site_id, taxon_id, reach, reach_id, date), 
           fill = list(total_fish = 0)) %>% #add zeros to fish with no data in a given pass
  arrange(reach, taxon_id, date, pass_number, reach_id) %>% 
  distinct(pass_number, site_id, taxon_id, reach, reach_id, date, .keep_all = T) %>% # remove duplicates
  pivot_wider(names_from = pass_number, values_from = total_fish) %>% 
  mutate(last_minus_first = `3`-`1`) %>% 
  pivot_longer(cols = c(`1`,`2`,`3`), names_to = "pass_number",
               values_to = "total_fish") %>% 
  mutate(increased = case_when(last_minus_first <=0 ~ "no", TRUE ~ "yes"),
         pass_number = as.integer(pass_number),
         year = year(date),
         month = month(date)) %>% 
  left_join(reach_lengths_widths %>% 
              distinct(reach_id, area_m2, measured_reach_length, mean_width_m), 
            by = "reach_id") %>% 
  group_by(site_id, reach_id, date) %>%
  filter(!any(is.na(area_m2))) 

saveRDS(stream_fish_torun, file = "data/raw_data/fish/stream_fish_torun.rds")

# model abundance ---------------------------------------------------------

# Use poisson model of first pass with previously modeled capture efficiency as offset
site_capture_probs = readRDS("data/raw_data/fish/site_capture_probs.rds")

stream_fish_firstpass = readRDS(file = "data/raw_data/fish/stream_fish_torun.rds") %>%
  filter(pass_number == 1) %>%
  mutate(site_fac = paste(site_id, year, month, taxon_id, sep = "_"),
         julian = julian(date)) %>%
  left_join(site_capture_probs) %>%
  group_by(site_fac) %>% 
  mutate(mean_julian = mean(julian)) %>% 
  mutate(area_m2_p = area_m2*median_prob) #this is how to combine two offsets: https://stats.stackexchange.com/questions/250528/is-it-possible-to-use-two-offsets

fish_abundance_poisson = brm(total_fish ~ site_fac + (1|reach) + offset(log(area_m2_p)),
                             family = poisson(link = "log"),
                             data = stream_fish_firstpass,
                             prior = c(prior(normal(0, 1), class = "b"),
                                       prior(normal(4, 4), class = "Intercept"),
                                       prior(exponential(1), class = "sd")),
                             chains = 2, iter = 2000,
                             file = "code/models/model-fits/fish_abundance-poisson.rds",
                             file_refit = "on_change")


total_fish = fish_abundance_poisson$data %>%
  distinct(site_fac, area_m2_p) %>%
  add_epred_draws(fish_abundance_poisson, re_formula = NA) %>%
  mutate(.epred = .epred/area_m2_p) %>%
  group_by(site_fac) %>%
  median_qi(fish_perm2 = .epred) %>%
  mutate(fish_per10000m2 = fish_perm2*10000) %>% 
  left_join(stream_fish_firstpass %>% distinct(site_fac, site_id, area_m2, p, area_m2_p, taxon_id, year , month))

saveRDS(total_fish, file = "data/derived_data/fish_fish-abundance.rds")

# fish length weight ------------------------------------------------------------
total_fish = readRDS(file = "data/derived_data/fish_fish-abundance.rds") # modeled fish abundances

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

individual_dw = fish$fsh_perFish %>% clean_names() %>% as_tibble() %>%
  mutate(year = year(pass_start_time),
         month = month(pass_start_time)) %>% 
  mutate(site_fac = paste(site_id, year, month, taxon_id, sep = "_")) %>% 
  mutate(dw = fish_weight*0.2) %>% 
  select(site_fac, dw) 

# Sample from dw measurements with replacement.
# Number of samples = total number of fish caught per pass per species.
# If 4 fish caught, then sample the 4 lengths from the 50 length measurements
# If 400 fish caught, then sample 400 lengths (with replacement) from the 50 length measurements

# simulate dw totals
size_and_totals = individual_dw %>% 
  left_join(total_fish) %>% 
  group_by(site_fac) %>% 
  filter(!any(is.na(fish_perm2)))

dw_sims = size_and_totals %>% 
  group_by(site_fac) %>% 
  filter(!is.na(dw)) %>% 
  sample_n(size = fish_per10000m2[1], replace = T) %>% 
  group_by(dw, site_fac, year, month) %>% 
  dplyr::count(name = "no_10000m2") %>% 
  mutate(no_m2 = no_10000m2/10000,
         animal_type = "fish") 

fish_dw = dw_sims %>% 
  ungroup %>% 
  left_join(stream_fish_firstpass %>% distinct(site_fac, site_id, month, year, mean_julian)) %>% 
  mutate(year_month = paste(year, month, sep = "_")) %>% 
  group_by(dw, site_id, year, month, mean_julian, animal_type, year_month) %>% 
  summarize(no_m2 = sum(no_m2)) %>% 
  mutate(event_id = paste(site_id, year_month, animal_type, sep = "_"),
         dw = dw*1000,
         dw_units = "mg") 

saveRDS(fish_dw, file = "data/derived_data/fish_dw-allyears.rds")



