library(tidyverse)
library(janitor)
library(lubridate)

stream_fish_first_pass <- stream_fish_perm2 %>% 
  filter(pass_number == 1) %>% 
  group_by(site_id, year, month) %>%
  mutate(rank = rank(-total_fish_perm2))

# plot rank abundnce (NOTE: some sites may have no curve because they only have one species)
stream_fish_first_pass %>% 
  ggplot(aes(x = rank, y = total_fish_perm2)) + 
  geom_point() +
  geom_line(aes(group = interaction(year, month))) +
  facet_wrap(~site_id, scales = "free_y") +
  NULL