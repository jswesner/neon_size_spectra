# combine fish and macros for aquasync

library(readr)
library(dplyr)


fish <- read_csv("data/derived_data/aqua-sync_data/fish_size_data.csv")
macro <- read_csv("data/derived_data/aqua-sync_data/invertebrate-size-data.csv")

all_size <- bind_rows(fish, macro)

write_csv(all_size, "data/derived_data/aqua-sync_data/all-size-aqua-sync.csv")
