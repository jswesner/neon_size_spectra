library(tidyverse)
library(janitor)
library(lubridate)
library(rfishbase)
library(here)

# Final dataset - use for size spectra anaylsis:
macro_fish_dw <- readRDS(file = "data/derived_data/macro_fish_dw.rds")

# code to create macro_fish_dw
macro_dw <- readRDS(here("data/derived_data/macro_dw.RDS")) %>% 
  mutate(animal_type = "macroinvertebrates") %>% as_tibble()

fish_dw <- readRDS(file = "data/derived_data/fish_weights_perm2.rds") %>% as_tibble()

fish_dw_short <- fish_dw %>% select(dw, animal_type, no_m2, site_id, date) %>%
  mutate(year = year(date),
         fish_month = month(date)) %>% 
  mutate(fish_julian = julian(as_date(date)))

saveRDS(fish_dw_short, file = "data/derived_data/fish_dw_short.rds")

# limit macro samples to only those with a fish sample within 30 days
filtered_dates <- macro_dw %>% clean_names() %>% 
  select(dw, animal_type, no_m2, site_id, collect_date, id) %>% 
  mutate(year = year(collect_date),
         month = month(collect_date)) %>% 
  rename(date = collect_date) %>% 
  mutate(julian = julian(as_date(date))) %>% 
  left_join(fish_dw_short %>% distinct(year, fish_julian,fish_month, site_id) , by = c("year", "site_id")) %>% 
  mutate(diff_julian = abs(fish_julian - julian)) %>% # days between macro and fish samples. Some fish samples are on consecutive days.
  filter(diff_julian <= 30) 


# macro data set with only dates that have a corresponding fish date
macro_dw_short <- filtered_dates %>% 
  select(-fish_julian, -diff_julian) %>% 
  distinct()
  

# create dataset for analysis (MAKE A TIE-BREAKER)
macro_fish_temp <- bind_rows(macro_dw_short, 
                           fish_dw_short %>% 
                             right_join(
                               filtered_dates %>% 
                                 distinct(fish_julian, month)) %>% 
                             rename(julian = fish_julian) # only fish dates that are within 30 days of a macro date
                           ) %>% 
  group_by(site_id, year, month) %>% 
  mutate(rank = rank(-dw),
         year_month = paste0(year,"_", month)) 

samples_with_macros_and_fish <- macro_fish_temp %>% 
  ungroup() %>% 
  distinct(animal_type, year_month, site_id) %>% 
  group_by(site_id, year_month) %>% 
  tally() %>%
  filter(n > 1) %>% 
  distinct(site_id, year_month) 

# get mean annual temps
mat_posts = readRDS("code/temperature/posteriors/mat_posts.rds") %>% clean_names() %>% 
  ungroup() %>% 
  mutate(mat_s = (mat_site - mean(mat_site))/sd(mat_site))

macro_fish_dw <- macro_fish_temp %>% select(-id) %>%
  mutate(year_month = paste0(year, "_", month)) %>% 
  right_join(samples_with_macros_and_fish) %>%
  left_join(mat_posts)  %>%           # add mean annual temperature per site
  mutate(ID = cur_group_id()) %>% 
  group_by(ID) %>% 
  mutate(xmin = min(dw),
         xmax = max(dw)) %>%             
  group_by(site_id, year_month) %>% 
  mutate(site_int = as.integer(as.factor(ID)),
         year_int = as.integer(as.factor(year)),
         group = paste(site_id, year_month, sep = "_"))

saveRDS(macro_fish_dw, file = "data/derived_data/macro_fish_dw.rds")
saveRDS(macro_fish_dw, file = "C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/stan_spectra/data/macro_fish_dw.rds")


macro_fish_dw %>% 
  ggplot(aes(y = no_m2, x = dw, color = animal_type), shape = 21) + 
  geom_point() 

macro_fish_dw %>% 
  filter(year > 2017 & year < 2020) %>% 
  mutate(dw = round(dw, 1)) %>% 
  ggplot(aes(y = rank, x = dw, color = animal_type)) + 
  geom_point(shape = 21) +
  # geom_line(aes(group = ID)) +
  scale_fill_grey(start = 0, end = 1) +
  scale_y_log10() + 
  scale_x_log10() +
  facet_wrap(~site_id) +
  NULL
