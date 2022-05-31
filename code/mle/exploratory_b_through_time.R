# relationship between b estimates through time?

library(tidyverse)
library(lubridate)

### load derived data products

mle_output <- readRDS("results/mle_b_estimate_fish_macro.RDS")
temp <- readRDS("code/temperature/posteriors/mat_posts.rds")

mle_output <- left_join(mle_output, temp, by = c("site_id" = "siteID"))

# my(mle_output$year_month)
# 
# select(coll_date)
# as_date(coll_date)
# 
# mutate(coll_date = as_date(c(year, month)))


mle_output %>%
  separate(year_month,
           into = c("year", "month")) %>%
  mutate(day = "1") %>%
  unite(coll_date, c(year, month, day), sep = "-") %>%
  mutate(coll_date2 = ymd(coll_date)) %>%
  #bind_rows(year_dates) %>%
  ggplot(aes(x = coll_date,
           y = b, 
           color = mat_site,
           group = site_id)) +
  #geom_point()+
  geom_line() +
  facet_wrap(~site_id, scales = "free_x") +
  NULL


mle_output %>%
  #filter(site_id == "ARIK" | site_id == "MAYF") %>%
  separate(year_month,
           into = c("year", "month")) %>%
  mutate(day = "1") %>%
  unite(coll_date, c(year, month, day), sep = "-") %>%
  mutate(coll_date2 = ymd(coll_date)) %>%
  ggplot(aes(x = coll_date,
             y = b, 
             color = mat_site,
             group = site_id)) +
  geom_point()+
  geom_line() +
  facet_wrap(~site_id, scales = "free_y")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  NULL
