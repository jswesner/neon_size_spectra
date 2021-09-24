library(neonUtilities)
library(tidyverse)
library(janitor)
library(lubridate)
library(FSA)
source("code/stream_site_id.R")

# fish <- loadByProduct(dpID="DP1.20107.001", site="all", package="basic", nCores = 4,
#                       token = "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJqZWZmd2VzbmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTc4NjU1MjkxMiwiaWF0IjoxNjI4ODcyOTEyLCJlbWFpbCI6ImplZmZ3ZXNuZXJAZ21haWwuY29tIn0.VnIZyX8yUCBfQyLOtS2hxr_tB4JW2CBzD46QezlxnIKCc1biYv9BbVZvl72obmKP1uXu4iK_c2pzDmBFW_S9oA")

# saveRDS(fish, "data/raw_data/fish.rds")

fish <- readRDS("data/raw_data/fish.rds")
stream_widths <- readRDS("data/raw_data/stream_widths.rds") # code/fish/get_stream_widths.R

#keep only fixed reaches, which are sampled with 3 pass depletion twice each year.
#delete random reaches. Those are sampled at different spots each year
fixed_random <- fish$fsh_fieldData %>% as_tibble() %>% 
  clean_names() %>% distinct(start_date, reach_id, named_location, fixed_random_reach, measured_reach_length) %>% 
  mutate(date = ymd(as.Date(start_date)),
         year = year(date)) %>% 
  select(-start_date) 

# fsh_perFish individual row for first 50 fish
perfish <- fish$fsh_perFish %>% as_tibble() %>% clean_names() %>% 
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

# bulk count of additional fish after the first 50 in fhs_perFish
bulk_count <- fish$fsh_bulkCount %>% as_tibble() %>% clean_names() %>% 
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c("site", "date2", "reach", NA, NA), 
           remove = F) %>% 
  unite("reach_id", site:reach, sep = ".") 

drop_these <- bulk_count %>% get_dupes(c(date, taxon_id, pass_number, reach_id)) %>% 
  distinct(site_id, date, reach_id)

bulk_countnodupes <- bulk_count %>% anti_join(drop_these) %>% 
  select(event_id, taxon_id, reach_id, bulk_fish_count)

# all fish - fsh_perFish PLUS fsh_bulkCount
all_fish <- left_join(perfish, bulk_countnodupes) %>% 
  replace_na(list(bulk_fish_count = 0)) %>% 
  mutate(total_fish = n + bulk_fish_count) 

stream_fish_only <- all_fish %>% 
  filter(site_id %in% streams) %>% 
  filter(pass_number <= 3)


stream_fish_torun <- stream_fish_only %>% 
  # filter(site_id == "HOPB") %>% 
  select(site_id, pass_number, taxon_id, total_fish, reach, date) %>% 
  arrange(reach, pass_number) %>%
  complete(pass_number, nesting(site_id, taxon_id, reach, date), fill = list(total_fish = 0)) %>% #add zeros to fish with no data in a given pass
  arrange(reach, taxon_id, date, pass_number) %>% 
  distinct(pass_number, site_id, taxon_id, reach, date, .keep_all = T) %>% # remove duplicates
  pivot_wider(names_from = pass_number, values_from = total_fish) %>% 
  mutate(last_minus_first = `3`-`1`) %>% 
  pivot_longer(cols = c(`1`,`2`,`3`), names_to = "pass_number",
               values_to = "total_fish") %>% 
  mutate(increased = case_when(last_minus_first <=0 ~ "no", TRUE ~ "yes")) %>% 
  unite("sample_id", c(site_id, taxon_id, reach, date), remove = F)


write_csv(stream_fish_torun, file = "data/derived_data/stream_fish_torun.csv")

# plot passes
stream_fish_torun  %>% 
  ggplot(aes(group = sample_id, x = pass_number, y = total_fish, color = increased)) +
  geom_point() +
  geom_line() +
  scale_y_log10()

# run 3 pass estimations
stream_fish_abund <- stream_fish_torun %>% 
  select(-pass_number) %>%
  group_by(site_id, taxon_id, reach, date) %>% 
  mutate(total_collected = sum(total_fish)) %>% 
  ungroup() %>% 
  nest_by(site_id, taxon_id, reach, date, total_collected, last_minus_first, increased, sample_id) %>% 
  mutate(pop = lapply(data, function(fish) removal(fish, just.ests = T, method = "CarleStrub"))) %>% 
  unnest_wider(pop) %>% 
  clean_names() %>% 
  mutate(date_fac = as.factor(date)) %>% 
  mutate(increased = case_when(last_minus_first <=0 ~ "no", TRUE ~ "yes"))


stream_fish_abund %>% 
  ggplot(aes(x = date, y = no, color = taxon_id)) + 
  geom_point() + 
  facet_wrap(~site_id) +
  scale_y_log10() +
  guides(color = "none")


stream_fish_abund %>% 
  left_join(stream_fish_torun %>% select(sample_id, pass_number, total_fish) %>% filter(pass_number == "1")) %>% 
  ggplot(aes(x = total_collected, y = no, color = increased)) + 
  geom_point() +
  scale_x_log10() + 
  scale_y_log10()

stream_fish_abund %>% 
  left_join(stream_fish_torun %>% select(sample_id, pass_number, total_fish) %>% filter(pass_number == "1")) %>%
  ggplot(aes(x = total_fish, y = no, color = increased)) + 
  geom_point() +
  labs(x = "fish on first pass") +
  scale_x_log10() + 
  scale_y_log10()

