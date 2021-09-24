library(tidyverse)
library(janitor)
library(lubridate)

stream_fish_torun <- read_csv("data/derived_data/stream_fish_torun.csv")

stream_fish_torun %>% 
  select(date, site_id, taxon_id, reach, bulk_fish_count, pass_number) %>% 
  pivot_wider(names_from = pass_number, values_from = bulk_fish_count) %>%
  mutate(last_first = `3` - `1`) %>%                   # subtract 3rd pass from 1st pass
  pivot_longer(cols = c(`1`, `2`, `3`), names_to = "pass_number", values_to = "bulk_fish_count") %>%
  mutate(group = paste0(date, site_id, taxon_id, reach),
         pass_number = as.numeric(pass_number),
         more_fish_on_last_pass = case_when(last_first > 0 ~ "yes", TRUE ~ "no"),        #color code by pass
         year = year(date)) %>% 
  ggplot(aes(x = pass_number, y = bulk_fish_count, group = group, color = more_fish_on_last_pass)) + 
  geom_point() +
  geom_line() +
  facet_wrap(~year) 
