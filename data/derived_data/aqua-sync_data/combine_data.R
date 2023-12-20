# combine fish and macros for aquasync

library(readr)
library(dplyr)
library(tidyr)
library(stringr)


fish <- read_csv("data/derived_data/aqua-sync_data/fish_size_data.csv")
macro <- read_csv("data/derived_data/aqua-sync_data/invertebrate-size-data.csv")



# fish
# change sample to be an integer
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

all_size <- bind_rows(fish, macro)

# match columns in template
all_size <- all_size %>%
  select(-date_invert) %>%
  select(site,
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
