# temperature data
library(neonUtilities)
library(dplyr)
library(tidyr)
library(ggplot2)
neon_token <- source("C:/Users/jfpom/Documents/Wesner/NEON documents/neon_token_source.R")

# NEON stream temps
temp_all <- loadByProduct(
  "DP1.20053.001",
  package = "basic",
  startdate = "2018-01",
  enddate = "2020-12",
  token = neon_token,
  check.size = FALSE
)$TSW_30min

temp_all %>%
  mutate(date = as.Date(startDateTime)) %>%
  group_by(siteID, date) %>%
  summarize(temp = mean(surfWaterTempMean, na.rm = TRUE)) %>%
  filter(!is.na(temp),
         !is.nan(temp)) %>%
  filter(temp > -10, temp < 40) %>%
  mutate(year = format(date, format = "%Y"),
         jdate = format(date, format = "%j")) %>%
  mutate(jdate = as.numeric(jdate)) %>%
  ggplot(aes(x = jdate,
             y = temp,
             color = year)) +
  geom_point(alpha = 0.2) +
  #geom_smooth() +
  facet_wrap(.~siteID)

temp_all %>%
  mutate(date = as.Date(startDateTime)) %>%
  group_by(siteID, date) %>%
  summarize(temp = mean(surfWaterTempMean, na.rm = TRUE)) %>%
  filter(!is.na(temp),
         !is.nan(temp)) %>%
  count() %>%
  arrange(n) %>% View


365 * 3
