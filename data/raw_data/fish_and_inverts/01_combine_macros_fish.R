library(tidyverse)
library(janitor)
library(lubridate)
library(hydroTSM)

# load data
macro_dw = readRDS(file = "data/derived_data/inverts_dw-allyears.rds")
fish_dw = readRDS(file = "data/derived_data/fish_dw-allyears.rds")

gpp = readRDS("data/derived_data/gpp_means.rds") %>% clean_names() %>% 
  rename(gpp = mean, gpp_sd = sd) %>% 
  mutate(log_gpp = log(gpp),
         log_gpp_s = scale(log_gpp, center = T, scale = T),
         log_gpp_s = as.numeric(log_gpp_s),
         gpp_s = (gpp - mean(gpp))/sd(gpp)) 

mat = readRDS("data/derived_data/temperature_mean-annual.rds") %>% 
  mutate(mat_s = (mean - mean(mean))/sd(mean)) %>% clean_names()

fish_collections = fish_dw %>% ungroup %>% distinct(julian, 
                                                    event_id, 
                                                    site_id) %>%
  rename(fish_site_id = site_id,
         fish_event_id = event_id,
         fish_julian = julian)

macro_collections = macro_dw %>% ungroup %>% distinct(julian, event_id, site_id) %>% 
  rename(macro_event_id = event_id,
         macro_site_id = site_id,
         macro_julian = julian)


# the tally's and filters limit the fish/macro pairings to one sample 
# per bout (i.e., if fish were sampled in June 15 and macros on June 2 and 
# June 20, then we only keep the June 20 macro sample and combine it with fish on June 15)
# Otherwise we'd have replicated the same fish sample twice

events_to_keep = fish_collections %>% 
  expand_grid(macro_collections) %>% 
  mutate(date_diff = abs(fish_julian - macro_julian)) %>% 
  filter(fish_site_id == macro_site_id) %>% 
  filter(date_diff <= 30) %>%  # only samples that are within 30 days of each other
  group_by(fish_event_id) %>% 
  add_tally() %>%         
  filter(date_diff == min(date_diff)) %>% # if more than one sample is within 30 days of the other, keep the closest two samples
  ungroup %>% 
  select(-n) %>% 
  group_by(macro_event_id) %>% 
  add_tally() %>% 
  filter(date_diff == min(date_diff)) %>% 
  ungroup %>% 
  mutate(sample_id = 1:nrow(.))

# filter to matched "events"
fish_dw_filtered = fish_dw %>% 
  semi_join(events_to_keep %>% 
              rename(event_id = fish_event_id) %>% 
              select(event_id)) 

macro_dw_filtered = macro_dw %>% 
  semi_join(events_to_keep %>% 
              rename(event_id = macro_event_id) %>% 
              select(event_id)) %>% 
  mutate(year = year(date),
         month = month(date))

# ensure that there are no duplicate samples
fish_dw_filtered %>% 
  ungroup %>% 
  distinct(month, year, site_id) %>%
  group_by(month, year, site_id) %>% 
  add_count() %>% 
  filter(n>1)

macro_dw_filtered %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  ungroup %>% 
  distinct(month, year, site_id) %>%
  group_by(month, year, site_id) %>% 
  add_count() %>% 
  filter(n>1)

fish_dw_wrangled = fish_dw_filtered %>% 
  rename(fish_event_id = event_id) %>% 
  left_join(events_to_keep %>% distinct(fish_event_id, sample_id, macro_julian)) %>% # macro date not fish date so all are the same
  group_by(dw, site_id, sample_id, macro_julian) %>% 
  summarize(no_m2 = sum(no_m2)) %>% 
  ungroup() %>% 
  mutate(xmin = min(dw)) %>% 
  group_by(site_id) %>% 
  mutate(xmax = max(dw)) %>% 
  ungroup %>% 
  left_join(gpp) %>% 
  left_join(mat) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  mutate(date = as_date(macro_julian),
         year = year(date),
         yday = yday(date),
         season = time2season(date, out.fmt = "seasons")) %>% 
  mutate(sample_int = as.integer(as.factor(sample_id)),
         year_int = as.integer(as.factor(year)),
         site_int = as.integer(as.factor(site_id)),
         season_int = as.integer(as.factor(season))) 

macro_dw_wrangled = macro_dw_filtered %>% 
  rename(macro_event_id = event_id) %>% 
  left_join(events_to_keep %>% distinct(macro_event_id, sample_id, macro_julian)) %>% 
  group_by(dw, site_id, sample_id, macro_julian) %>% 
  summarize(no_m2 = sum(no_m2)) %>% 
  ungroup() %>% 
  mutate(xmin = min(dw)) %>% 
  group_by(site_id) %>% 
  mutate(xmax = max(dw)) %>% 
  ungroup %>% 
  left_join(gpp) %>% 
  left_join(mat) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  mutate(date = as_date(macro_julian),
         year = year(date),
         yday = yday(date),
         season = time2season(date, out.fmt = "seasons")) %>% 
  mutate(sample_int = as.integer(as.factor(sample_id)),
         year_int = as.integer(as.factor(year)),
         site_int = as.integer(as.factor(site_id)),
         season_int = as.integer(as.factor(season))) 

macro_fish_dw = bind_rows(fish_dw_wrangled, macro_dw_wrangled) %>% 
  group_by(dw, site_id, sample_id, macro_julian) %>% 
  summarize(no_m2 = sum(no_m2)) %>%    # tally by size, regardless of taxa
  ungroup() %>% 
  mutate(xmin = min(dw)) %>% 
  group_by(site_id) %>% 
  mutate(xmax = max(dw)) %>% 
  ungroup %>% 
  left_join(gpp) %>% 
  left_join(mat) %>% 
  filter(!is.na(log_gpp_s)) %>% 
  mutate(date = as_date(macro_julian),
         year = year(date),
         yday = yday(date),
         season = time2season(date, out.fmt = "seasons")) %>% 
  mutate(sample_int = as.integer(as.factor(sample_id)),
         year_int = as.integer(as.factor(year)),
         site_int = as.integer(as.factor(site_id)),
         season_int = as.integer(as.factor(season))) 


saveRDS(fish_dw_wrangled, file = "data/derived_data/fish_dw-wrangled.rds")
saveRDS(macro_dw_wrangled, file = "data/derived_data/macro_dw-wrangled.rds")
saveRDS(macro_fish_dw, file = "data/derived_data/fish_inverts_dw-allyears.rds")


# sanity check ------------------------------------------------------------

macro_fish_dw %>% 
  group_by(dw, site_id) %>% 
  summarize(no_m2 = sum(no_m2)) %>% 
  ggplot(aes(x = dw, y = no_m2)) +
  # geom_histogram(bins = 200) +
  geom_point() +
  facet_wrap(~site_id) +
  scale_x_log10() +
  scale_y_log10() +
  NULL

macro_fish_dw



