library(tidyverse)
library(janitor)
library(lubridate)
library(rfishbase)
library(here)

# Final dataset - use for size spectra anaylsis:
macro_fish_dw <- readRDS(file = "data/raw_data/macro_fish_dw.rds")

# code to create macro_fish_dw
macro_dw <- readRDS(here("data/derived_data/macro_dw.RDS")) %>% 
  mutate(animal_type = "macroinvertebrates") %>% as_tibble()

fish_dw <- read_csv(file = "data/derived_data/total_lengths_with_parameters.csv") %>% 
  # rename(wet_weight = grams) %>% 
  mutate(animal_type = "fish") %>% as_tibble() # Converts all lengths <=2 to to 2 cm.


fish_dw_short <- fish_dw %>% select(dw, animal_type, area_m2, no_m2, site_id, date) %>%
  mutate(dw = dw*1000, dw_units = "mg") %>%  # convert grams to mg
  mutate(year = year(date),
         month = month(date))

macro_dw_short <- macro_dw %>% clean_names() %>% 
  select(dw, animal_type, no_m2, site_id, collect_date) %>% 
  mutate(year = year(collect_date),
         month = month(collect_date)) %>% 
  rename(date = collect_date) %>% 
  left_join(fish_dw_short %>% distinct(month, year) %>% rename(fish_month = month), by = "year") 


macro_dw_short %>% distinct(month, year, site_id) %>% group_by(month, site_id) %>% tally() %>% 
  mutate(animal_type = "macroinvertebrates") %>% 
  bind_rows(fish_dw_short %>% 
              distinct(month, year, site_id) %>% 
              group_by(month, site_id) %>% tally() %>% mutate(animal_type = "fish")) %>% 
  ggplot(aes(x = month, y = n, color = animal_type)) + 
  geom_point(position = position_dodge(width = 0.2)) +
  facet_wrap(~site_id)

macro_fish_dw <- bind_rows(macro_dw_short, fish_dw_short) %>% 
  group_by(dw, animal_type, month, year) %>%
  mutate(no_m2 = mean(no_m2)) %>% 
  ungroup() %>% 
  group_by(site_id, year) %>% 
  mutate(rank = rank(-dw)) 

saveRDS(macro_fish_dw, file = "data/raw_data/macro_fish_dw.rds")

macro_fish_dw %>% group_by(dw, animal_type) %>% summarize(no_m2 = sum(no_m2)) %>% 
  ggplot(aes(y = no_m2, x = dw, color = animal_type), shape = 21) + 
  geom_point() +
  scale_y_log10() + 
  scale_x_log10()

macro_fish_dw %>% 
  filter(year > 2017 & year < 2020) %>% 
  mutate(dw = round(dw, 1)) %>% 
  ggplot(aes(y = rank, x = dw, color = animal_type)) + 
  geom_point(shape = 21) +
  scale_y_log10() + 
  scale_x_log10() +
  facet_wrap(~site_id) +
  NULL


