# combine fish and macros for aquasync

library(tidyverse)

# fish <- read_csv("data/derived_data/aqua-sync_data/fish_size_data.csv")
# change to "full" size data
fish <- read_csv("data/derived_data/aqua-sync_data/fish_size_data_full.csv")
macro <- read_csv("data/derived_data/aqua-sync_data/invertebrate-size-data.csv")

setdiff(names(macro), names(fish))
# need to make year, month, site_date columns in fish
# change site in fish to just be site code, not site_date

fish <- fish %>%
  mutate(
    year = year(date_invert),
    month = month(date_invert)) %>%
  rename(site_date = site) %>%
  separate_wider_delim(site_date, delim = "_", names = c("site", "date"), cols_remove = FALSE) 



# fish
# change sample to be an integer
# get rid of this and just make all sample = 1
# count is already in no_m2, so multiplier = 1
# mutate(sample = 1)
# double check that sum(no_m2) in fish is fine
fish <- fish %>%
  separate(
    sample, 
    into = c("delete_1", "delete_2", "sample", "delete_3")) %>%
  select(-starts_with("delete")) %>%
  mutate(sample = as.numeric(sample))



# macroinvertebrates
# change sample to be an integer
# remove "DNA" samples ~200 rows
macro <- macro %>%
  filter(!str_detect(sample, 'DNA')) %>%
  mutate(sample = as.numeric(str_sub(sample, -1)))

fish_dates <- fish %>% pull(site_date) %>% unique()

macro_small <- macro %>%
  filter(site_date %in% fish_dates)

all_size <- bind_rows(fish, macro_small)

# match columns in template
all_size <- all_size %>%
  select(-date_invert) %>%
  select(site,
         year, 
         month,
         site_date,
         sampling_method,
         sample,
         sampling_area,
         organism_group,
         taxon,
         body_mass,
         body_length,
         body_weight_units,
         body_length_units,
         count,
         multiplier) 

# # read_excel doesn't like NAS (WTF????)
# # replace NA with empty string
# all_size <- all_size %>%
#   mutate(across(everything(), ~ replace(.x, is.na(.x), "")))

write_csv(all_size, "data/derived_data/aqua-sync_data/all-size-aqua-sync.csv")
