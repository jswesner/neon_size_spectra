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


fish$fsh_bulkCount %>% as_tibble() %>% clean_names() %>% 
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c(NA, NA, "reach", NA, NA, NA), remove = F) %>% 
  filter(site_id %in% streams) %>% 
  pivot_wider(names_from = pass_number, values_from = bulk_fish_count) %>% 
  select(site_id, date, reach, taxon_id, `1`, `2`, `3`)

test_join <- left_join(data.frame(pass_number = seq(max(stream_fish_only$pass_number))), stream_fish_only)

stream_fish_only <- fish$fsh_bulkCount %>% as_tibble() %>% clean_names() %>% 
  mutate(date = ymd(as.Date(pass_start_time)),
         year = year(date)) %>%
  separate(event_id, c(NA, NA, "reach", NA, NA, NA), remove = F) %>% 
  filter(site_id %in% streams)

pass_tallys <- stream_fish_only %>% 
  distinct(site_id, date, reach, pass_number, pass_start_time, pass_end_time) %>% 
  group_by(site_id, date, reach) %>%
  mutate(max_pass_tally = max(pass_number)) %>% 
  arrange(site_id, reach, date, pass_number) 
  

# get sample dates that contain unexplained duplicates. Drop all samples from those sites, dates, and reaches
drop_these <- stream_fish_only %>% get_dupes(c(date, taxon_id, pass_number, reach)) %>% 
  distinct(site_id, date, reach)

stream_fish_torun <- stream_fish_only %>% 
  left_join(pass_tallys) %>% 
  filter(max_pass_tally == 3) %>% 
  # filter(site_id == "HOPB") %>% 
  select(site_id, pass_number, taxon_id, bulk_fish_count, reach, date) %>% 
  arrange(reach, pass_number) %>%
  complete(pass_number, nesting(site_id, taxon_id, reach, date), fill = list(bulk_fish_count = 0)) %>% #add zeros to fish with no data in a given pass
  arrange(reach, taxon_id, date, pass_number) %>% 
  anti_join(drop_these)         #drop the unexplained duplicates

stream_fish_abund <- stream_fish_torun %>% 
  select(-pass_number) %>%
  nest_by(site_id, taxon_id, reach, date) %>% 
  mutate(pop = lapply(data, function(fish) removal(fish, just.ests = T, method = "CarleStrub"))) %>% 
  unnest_wider(pop) %>% 
  clean_names() %>% 
  mutate(date_fac = as.factor(date))

stream_fish_abund %>% 
  ggplot(aes(x = date, y = no, color = taxon_id)) +
  geom_point() +
  # geom_errorbar(aes(ymin = no_lci, ymax = no_uci)) + 
  # scale_y_log10() + 
  facet_wrap(~site_id, scales = "free_y")


# model mean abundance
library(brms)

stream_fish_abund %>% filter(site_id == "KING")

get_prior(no ~ 0 + taxon_id + (1|reach),
              family = Gamma(link = "log"),
              data = stream_fish_abund %>% filter(site_id == "KING"))

# simulate priors
plot(exp((rnorm(1000, 2, 2) + rnorm(1000, 0, 1))))

# fit model
test <- brm(no ~ 1 + taxon_id + (1|taxon_id),
            family = Gamma(link = "log"),
            data = stream_fish_abund %>% filter(site_id == "KING"),
            prior = c(prior(normal(2, 2), class = "Intercept"),
                      prior(normal(0, 1), class = "b"),
                      prior(exponential(0.1), class = "sd")),
            iter = 1000, chains = 1, 
            file = "models/fish/fish_abundance.rds",
            file_refit = "on_change")

test_plot <- plot(conditional_effects(test, re_formula = NULL), points = T)

test_plot$taxon_id +
  facet_wrap(~taxon_id, scales = "free_y")

