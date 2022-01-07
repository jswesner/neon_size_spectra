library(neonUtilities)
library(dplyr)
library(tidyr)
library(ggplot2)
# neon_token <- source("C:/Users/jfpom/Documents/Wesner/NEON documents/neon_token_source.R")$value
source("code/stream_site_id.R")

# air temp
air_temp <- loadByProduct(
  "DP1.00002.001",
  site = streams,
  startdate = "2019-01",
  enddate = "2019-12",
  check.size = TRUE
)

names(air_temp)
names(air_temp$SAAT_30min)

unique(air_temp$SAAT_30min$siteID)

air <- air_temp$SAAT_30min

air %>%
  mutate(
    date = as.Date(startDateTime)) %>%
  group_by(siteID, date) %>%
  summarize(temp = mean(tempSingleMean, na.rm = TRUE)) %>%
  filter(!is.na(temp),
         !is.nan(temp)) %>%
  #filter(temp > -10, temp < 40) %>%
  mutate(year = format(date, format = "%Y"),
         jdate = format(date, format = "%j")) %>%
  mutate(jdate = as.numeric(jdate)) %>%
  ggplot(aes(x = jdate,
             y = temp,
             color = year)) +
  geom_point(alpha = 0.2) +
  #geom_smooth() +
  facet_wrap(.~siteID)

prt <- loadByProduct(
  "DP1.20053.001",
  package = "basic",
  startdate = "2019-01",
  enddate = "2019-12",
  # token = neon_token,
  check.size = FALSE
)$TSW_30min

air <- air %>%
  mutate(
    date = as.Date(startDateTime)) %>%
  group_by(siteID, date) %>%
  summarize(air_temp = mean(
    tempSingleMean, na.rm = TRUE)) %>%
  mutate(year = format(
    date, format = "%Y"),
         jdate = format(
           date, format = "%j")) %>%
  select(siteID, air_temp, jdate)

air <- ungroup(air)

prt <- prt %>%
  mutate(
    date = as.Date(startDateTime)) %>%
  group_by(siteID, date) %>%
  summarize(water_temp = mean(
    surfWaterTempMean, na.rm = TRUE)) %>%
  mutate(year = format(
    date, format = "%Y"),
    jdate = format(
      date, format = "%j")) %>%
  ungroup() %>%
  select(siteID, water_temp, jdate)


full_join(air, prt) %>%
  filter(!is.na(air_temp),
         !is.nan(air_temp),
         !is.na(water_temp),
         !is.nan(water_temp)) %>%
  filter(water_temp > -10,
         water_temp < 40) %>%
  ggplot(aes(x = air_temp,
             y = water_temp,
             color = siteID)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(.~siteID,
             scales = "free")

prt %>%
  mutate(jdate = as.numeric(jdate)) %>%
  ggplot(aes(x = jdate,
           y = water_temp,
           color = siteID)) +
  geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(.~siteID,
             scales = "free")


prt_small <- prt %>% filter(siteID %in% c("ARIK", "HOPB")) %>% 
  arrange(water_temp) %>% 
  mutate(jdate = as.numeric(jdate),
         jdate_c = (jdate - mean(jdate))/sd(jdate),
         water_temp_s = (water_temp - mean(water_temp))/sd(water_temp))

library(brms)

get_prior(water_temp ~ s(jdate, by = siteID),
          family = gaussian(), data = prt_small)

brm_temp <- brm(water_temp ~ s(jdate, by = siteID) + (1|siteID),
                family = gaussian(), data = prt_small,
                prior = c(prior(normal(0, 1), class = "Intercept"),
                          prior(normal(0, 1), class = "b"),
                          prior(normal(0, 1), class = "sds"),
                          prior(exponential(1), class = "sd")),
                iter = 1000, chains = 1)

plot(conditional_effects(brm_temp, effects = "jdate:siteID", re_formula = NULL),
     points = T)

# extract posterior on each sample date
fit <- brm_temp

list_of_data <- conditional_effects(fit, effects = "jdate:siteID", re_formula = NULL)[[1]]

library(janitor)
library(readr)

new_names <- list_of_data %>% 
  select(-names(fit$data[1])) %>% 
  select(-cond__, -effect1__, -effect2__, -estimate__, -se__, -lower__, -upper__) %>% remove_empty("cols")


posts_temp <- as_tibble(t(fitted(fit, newdata = list_of_data, reformula = NULL, summary = F))) %>% 
  cbind(new_names) %>% 
  pivot_longer(cols = contains("V"), names_to = "iter") %>%
  mutate(iter = parse_number(iter))

posts_temp %>% 
  filter(iter <= 20) %>% 
  ggplot(aes(x = jdate, y = value, color = siteID, 
             group = interaction(siteID, iter))) + 
  geom_line()

