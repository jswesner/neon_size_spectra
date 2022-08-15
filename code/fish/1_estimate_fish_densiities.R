library(neonUtilities)
library(tidyverse)
library(janitor)
library(lubridate)
library(FSA)
source("code/stream_site_id.R")

# fish <- loadByProduct(dpID="DP1.20107.001", site="all", package="basic", nCores = 4,
#                       token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJqZWZmd2VzbmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTc4NjU1MjkxMiwiaWF0IjoxNjI4ODcyOTEyLCJlbWFpbCI6ImplZmZ3ZXNuZXJAZ21haWwuY29tIn0.VnIZyX8yUCBfQyLOtS2hxr_tB4JW2CBzD46QezlxnIKCc1biYv9BbVZvl72obmKP1uXu4iK_c2pzDmBFW_S9oA")
# 
# saveRDS(fish, "data/raw_data/fish.rds")

fish <- readRDS("data/raw_data/fish.rds")
stream_widths <- readRDS("data/raw_data/stream_widths.rds") %>% clean_names() # code/fish/get_stream_widths.R
add_missing_reach_lengths <- read_csv("data/raw_data/add_missing_reach_lengths.csv") %>% clean_names()
reach_lengths <- fish$fsh_fieldData %>% clean_names()

# wrangle length and width sampled
reach_lengths_to_add <- reach_lengths %>% ungroup() %>%  as_tibble() %>% distinct(reach_id, measured_reach_length) %>% 
  filter(!is.na(measured_reach_length)) %>% 
  bind_rows(add_missing_reach_lengths %>% dplyr::rename(measured_reach_length = reach_length_sampled) %>% select(-notes))

stream_widths_to_add <- stream_widths %>% as_tibble() %>% 
  mutate(year = year(collect_date),
         month = month(collect_date)) %>% 
  group_by(site_id) %>% 
  summarize(mean_width_m = mean(wetted_width),
            sd_width_m = sd(wetted_width))

#keep only fixed reaches, which are sampled with 3 pass depletion twice each year.
#delete random reaches. Those are sampled at different spots each year
fixed_ids <- fish$fsh_fieldData %>% as_tibble() %>% 
  clean_names() %>% distinct(start_date, reach_id, named_location, fixed_random_reach, measured_reach_length) %>% 
  mutate(date = ymd(as.Date(start_date)),
         year = year(date)) %>% 
  select(-start_date) %>% 
  filter(fixed_random_reach == "fixed") %>% 
  distinct(reach_id)

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
  ungroup() %>% 
  anti_join(fixed_ids)

# bulk count of additional fish after the first 50 in fhs_perFish
bulk_count <- fish$fsh_bulkCount %>% as_tibble() %>% clean_names() %>% 
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c("site", "date2", "reach", NA, NA), 
           remove = F) %>% 
  unite("reach_id", site:reach, sep = ".") %>% 
  anti_join(fixed_ids) %>% 
  select(-uid, -identified_by) %>%
  distinct() # remove duplicates (confirmed that these were true duplicates on 2022-01-06 using get_dupes())

# drop_these <- bulk_count %>% get_dupes(c(date, taxon_id, pass_number, reach_id)) %>% 
#   distinct(site_id, date, reach_id) # is this now redundant?
# 
# bulk_countnodupes <- bulk_count %>% anti_join(drop_these) %>% 
#   select(event_id, taxon_id, reach_id, bulk_fish_count)

# all fish - fsh_perFish PLUS fsh_bulkCount
all_fish <- left_join(perfish, bulk_count) %>% 
  replace_na(list(bulk_fish_count = 0)) %>% 
  mutate(total_fish = n + bulk_fish_count) 


# limit to streams, add length and width sampled
stream_fish_width_length_sampled <- all_fish %>% 
  filter(site_id %in% streams) %>% 
  filter(pass_number <= 3) %>% 
  as_tibble() %>% 
  left_join(reach_lengths_to_add) %>% # add reach length
  distinct() %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  left_join(stream_widths_to_add) %>% # add reach width
  filter(!is.na(measured_reach_length),
         !is.na(mean_width_m)) %>% 
  distinct() %>% 
  mutate(area_m2 = measured_reach_length*mean_width_m,
         total_fish_perm2 = total_fish/area_m2)


stream_fish_torun <- stream_fish_width_length_sampled %>% 
  select(site_id, pass_number, taxon_id, total_fish, reach, date, reach_id, event_id) %>% 
  arrange(reach, pass_number) %>%
  complete(pass_number, nesting(site_id, taxon_id, reach, reach_id, date, event_id), 
           fill = list(total_fish = 0)) %>% #add zeros to fish with no data in a given pass
  arrange(reach, taxon_id, date, pass_number, reach_id) %>% 
  distinct(pass_number, site_id, taxon_id, reach, reach_id, date, .keep_all = T) %>% # remove duplicates
  pivot_wider(names_from = pass_number, values_from = total_fish) %>% 
  mutate(last_minus_first = `3`-`1`) %>% 
  pivot_longer(cols = c(`1`,`2`,`3`), names_to = "pass_number",
               values_to = "total_fish") %>% 
  mutate(increased = case_when(last_minus_first <=0 ~ "no", TRUE ~ "yes"),
         pass_number = as.integer(pass_number)) 


fish_lengths = fish$fsh_perFish %>% as_tibble() %>% clean_names() %>% 
  select(-identified_by, -fish_weight, -uid) %>% # remove these columns to allow removal of duplicates
  distinct() %>%  # remove duplicates (duplicates confirmed via email with NEON on 2022-01-06)
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c("site", "date2", "reach", NA, NA), 
           remove = F) %>% 
  unite("reach_id", site:reach, sep = ".") %>% 
  separate(event_id, c(NA, NA, "reach", NA, NA, NA), remove = F) %>% 
  ungroup() %>% 
  anti_join(fixed_ids) %>% 
  filter(site_id %in% streams)

# Sample from length measurements with replacement.
# Number of samples = total number of fish caught per pass per species.
# If 4 fish caught, then sample 4 lengths from the 50 length measurements
# If 400 fish caught, then sample 400 lengths (with replacement) from the 50 length measurements
# There is surely a more efficient way to do this, but the code below works


# total fish per pass per reach id
total_fish = left_join(perfish, bulk_count) %>% 
  replace_na(list(bulk_fish_count = 0)) %>% 
  mutate(total_fish = n + bulk_fish_count,
         pass_number = as.integer(pass_number)) %>% 
  # filter(reach_id == "LECO.20211102.01") %>% 
  select(total_fish, pass_number, reach_id) %>% 
  group_by(reach_id, pass_number) %>% 
  summarize(pass_total = sum(total_fish)) %>% 
  filter(pass_number <= 3)

# total fish across all passes per reach id
total_fish_overall = left_join(perfish, bulk_count) %>% 
  replace_na(list(bulk_fish_count = 0)) %>% 
  mutate(total_fish = n + bulk_fish_count) %>% 
  # filter(reach_id == "LECO.20211102.01") %>% 
  select(total_fish, pass_number, reach_id) %>% 
  group_by(reach_id) %>% 
  summarize(grand_total = sum(total_fish))


# simulate length totals
lengths_and_totals = fish_lengths %>% 
  select(reach_id, fish_total_length) %>% 
  left_join(total_fish_overall)

length_split = lengths_and_totals %>% group_split(reach_id)

length_sims_temp = NULL
for(i in 1:length(length_split)){
  length_sims_temp[[i]] = sample_n(length_split[[i]], length_split[[i]] %>% distinct(grand_total) %>% pull(), replace = T)
}

length_sims = bind_rows(length_sims_temp)

# make data for 3-pass depletion models
length_sim_counts = length_sims %>%
  expand_grid(pass_number = 1:3) %>% 
  left_join(total_fish) %>% 
  replace_na(list(pass_total = 0)) %>%
  group_by(reach_id, fish_total_length, pass_number) %>% 
  add_tally() %>% 
  mutate(n_adjust = case_when(pass_total == 0 ~ 0, 
                              is.na(pass_total) ~ 0,
                              TRUE ~ 1),
         n_lengths = n*n_adjust)

# check (These look good. King creek has some higher values in pass three, but that is the only odd site
# Will run them in the model and let sampler sort out pop sizes)
length_sim_counts %>% 
  filter(reach_id != "KING.20171018.01") %>% 
  ggplot(aes(x = pass_number, y = n_lengths)) + 
  geom_jitter()  


##############################
##############################
###### 08/12/2022 - Code above works. Next step - figure out how to get 3-pass estiamtes for each body size/reach_id combo
##############################
##############################

# 3-pass depletion model ---------------------------------------------------------------------
# run 3 pass estimations
stream_fish_abund <- stream_fish_torun %>% 
  select(-pass_number) %>%
  group_by(site_id, taxon_id, reach, reach_id, date) %>% 
  mutate(total_collected = sum(total_fish)) %>% 
  ungroup() %>% 
  nest_by(site_id, taxon_id,reach_id, reach, date, total_collected, 
          last_minus_first, increased) %>% View()
  mutate(pop = lapply(data, function(fish) removal(fish, just.ests = T, method = "CarleStrub"))) %>% 
  unnest_wider(pop) %>% 
  clean_names() %>% 
  mutate(date_fac = as.factor(date)) %>% 
  mutate(increased = case_when(last_minus_first <=0 ~ "no", TRUE ~ "yes")) %>% 
  glimpse()

# add sample areas and sum across reaches. Results in the number of fish collected per m2 per site per date per species
stream_fish_perm2 <- stream_fish_abund %>% 
  left_join(reach_lengths_to_add) %>% # add reach length
  distinct() %>% 
  mutate(year = year(date),
         month = month(date)) %>% 
  left_join(stream_widths_to_add) %>% # add reach width
  filter(!is.na(measured_reach_length),
         !is.na(mean_width_m)) %>% 
  distinct() %>% 
  mutate(area_m2 = measured_reach_length*mean_width_m) %>% 
  group_by(site_id, taxon_id, date, increased) %>% 
  summarize(total_collected = sum(total_collected),
            area_m2 = sum(area_m2),
            no = sum(no)) %>% 
  mutate(no = case_when(increased == "yes" ~ total_collected,  # if fish increased in 3rd pass, then replace model estimate with total collected (sensu McGarvey et al. 2018)
                          TRUE ~ no)) %>% 
  mutate(total_fish_perm2 = no/area_m2) %>% 
  left_join(all_fish %>% distinct(taxon_id, scientific_name))

saveRDS(stream_fish_perm2, file = "data/derived_data/stream_fish_perm2.rds")


# sanity check
ggplot(stream_fish_perm2 %>% group_by(site_id, date) %>% mutate(rank = rank(-total_fish_perm2)),
       aes(x = rank, y = total_fish_perm2)) + 
  geom_point() +
  facet_wrap(~site_id)


