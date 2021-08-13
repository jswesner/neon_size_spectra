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

stream_fish_only <- fish$fsh_bulkCount %>% as_tibble() %>% clean_names() %>% 
  mutate(sample_date = ymd(as.Date(pass_start_time)),
         year = year(sample_date)) %>%
  separate(event_id, c(NA, NA, "reach", NA, NA, NA), remove = F) %>% 
  filter(site_id %in% streams) 

stream_fish_only %>% filter(scientific_name == "Semotilus atromaculatus") %>%
  filter(sample_date == "2016-10-27") %>% 
  filter(reach == "10")



ct3 <- c(75, 125, 10)
p4 <- removal(ct3,method="Burnham")
summary(p4)
summary(p4,verbose=TRUE)
summary(p4,parm="No")
summary(p4,parm="p")
confint(p4)
confint(p4,parm="No")
confint(p4,parm="p")
