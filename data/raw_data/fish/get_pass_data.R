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


# TOTAL POPULATION wrangle data ------------------
fish <- readRDS("data/raw_data/fish/fish_stacked.rds")

fish_bulk = fish$fsh_bulkCount %>% 
  select(eventID, taxonID, bulkFishCount) %>% 
  separate(eventID, into = c("site_id", "date", "reach", "pass", "method")) %>% 
  mutate(reach_id = paste(site_id, date, reach, sep = ".")) %>% 
  rename(n = bulkFishCount)

fish_measures = fish$fsh_perFish %>% 
  select(eventID, taxonID) %>% 
  separate(eventID, into = c("site_id", "date", "reach", "pass", "method")) %>% 
  mutate(reach_id = paste(site_id, date, reach, sep = ".")) %>% 
  group_by(site_id, date, reach, reach_id, pass) %>% 
  tally()

three_pass_data = bind_rows(fish_bulk, fish_measures) %>% group_by(reach_id, pass) %>% 
  summarize(total_fish = sum(n, na.rm = T))

write_csv(three_pass_data, file = "data/raw_data/fish/three_pass_data.csv")

three_pass_data %>% ggplot(aes(x = pass, y = total_fish)) +
  geom_jitter(width = 0.2)

# SPECIES POPULATION wrangle data ------------------
fish <- readRDS("data/raw_data/fish/fish_stacked.rds")

fish_bulk = fish$fsh_bulkCount %>% 
  select(eventID, taxonID, bulkFishCount) %>% 
  separate(eventID, into = c("site_id", "date", "reach", "pass", "method")) %>% 
  mutate(reach_id = paste(site_id, date, reach, sep = ".")) %>% 
  rename(n = bulkFishCount)

fish_measures_species = fish$fsh_perFish %>% 
  select(eventID, taxonID) %>% 
  separate(eventID, into = c("site_id", "date", "reach", "pass", "method")) %>% 
  mutate(reach_id = paste(site_id, date, reach, sep = ".")) %>% 
  group_by(site_id, date, reach, reach_id, pass, taxonID) %>% 
  tally()

three_pass_data = bind_rows(fish_bulk, fish_measures) %>% group_by(reach_id, pass) %>% 
  summarize(total_fish = sum(n, na.rm = T))

write_csv(three_pass_data, file = "data/raw_data/fish/three_pass_data.csv")

three_pass_data %>% ggplot(aes(x = pass, y = total_fish)) +
  geom_jitter(width = 0.2)