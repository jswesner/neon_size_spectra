# temperature data
library(tidyverse)
library(janitor)
library(brms)
library(lubridate)
library(tidybayes)
library(ggthemes)


# wrangle data for the model ---------------------------------------------

# 1) load data
raw_stream_temperatures = readRDS(file = "data/raw_data/temperature_raw-data.rds")

# 2) clean and thin data
raw_stream_temperatures_cleaned <- raw_stream_temperatures$TSW_30min %>% 
  as_tibble() %>% 
  filter(finalQF == 0) %>% # removes quality flags. (e.g., 0 = pass, 1 = fail) ~ 900K rows are removed by this
  mutate(date = as.Date(startDateTime),
         jday = yday(date),
         year = year(date),
         hour = hour(startDateTime)) %>% 
  filter(surfWaterTempMean > -5 & surfWaterTempMean <50)  %>% # remove extreme values
  filter(jday %in% seq(1, 365, by = 4)) %>%  # get data every 4 days
  filter(hour %in% c(0, 6, 12, 18)) # limit to one measure every 4 hours

# 3) summarize as mean temperature per week
# insert 0 degrees for Alaskan winters that don't have data
add_zeros_to_alaska <- raw_stream_temperatures_cleaned %>% 
  filter(siteID == "OKSR" | siteID == "CARI") %>%
  filter(year >= 2019) %>% # fill in missing dates
  group_by(siteID) %>% 
  complete(date = seq.Date(as_date("2019-01-01"), as_date("2021-12-31"), by="day")) %>% 
  mutate(month = month(date)) %>% 
  mutate(surfWaterTempMean = case_when(siteID == "OKSR" & month %in% c(10, 11, 12, 1, 2, 3, 4, 5) ~ 0,
                                       siteID == "CARI" & month %in% c(11, 12) ~ 0,
                                       TRUE ~ surfWaterTempMean)) %>% 
  select(siteID, date, surfWaterTempMean)

raw_stream_temperatures_formodel = raw_stream_temperatures_cleaned %>% 
  filter(siteID != "OKSR" | siteID != "CARI") %>% # remove Alaska here. Add it below.
  filter(year >= 2019) %>% 
  select(siteID, date, surfWaterTempMean) %>% 
  bind_rows(add_zeros_to_alaska) %>%
  mutate(year = year(date),
         year_f = as.factor(year),
         week = week(date),
         jday = yday(date),
         julian = julian(date),
         hour = hour(date),
         month = month(date))

mean_weekly_temperatures <- raw_stream_temperatures_formodel %>% 
  group_by(siteID) %>% 
  mutate(mean_water = mean(surfWaterTempMean, na.rm = T),
         mean_jday = mean(jday, na.rm = T),
         jday_c_100 = (jday - mean_jday)/100,
         water_c_10 = (surfWaterTempMean - mean_water)/10) %>%
  ungroup() 

saveRDS(raw_stream_temperatures_formodel, file = "data/derived_data/raw_stream_temperatures_formodel.rds")
saveRDS(mean_weekly_temperatures, file = "data/derived_data/mean_weekly_temperatures.rds")

# fit temperature model ---------------------------------------------
# 1) make lists by site
mean_weekly_temperatures_list <- split(mean_weekly_temperatures, f = mean_weekly_temperatures$siteID)

# 2) prior predictive
brm_temperature_priors <- brm(water_c_10 ~  s(jday_c_100, by = siteID) + (1|year_f),
                       family = gaussian(),
                       data = mean_weekly_temperatures %>% filter(siteID == "OKSR" | siteID == "ARIK"),
                       prior =
                         c(prior(normal(0, 2),
                                 class = "Intercept"),
                           prior(normal(0, 1),
                                 class = "b"),
                           prior(normal(0, 1),
                                 class = "sds"),
                           prior(exponential(1),
                                 class = "sd")),
                       iter = 1000, chains = 1,
                       sample_prior = "only",
                       file = "code/temperature/temperature_model-priors.rds",
                       file_refit = "on_change"
                       )

conditional_effects(brm_temperature_priors, effects = "jday_c_100:siteID", re_formula = NA)

# 3) fit to data
temperature_model <- brm_multiple(water_c_10 ~  s(jday_c_100) + (1|year_f),
                                family = gaussian(),
                                data = mean_weekly_temperatures_list,
                                prior =
                                  c(prior(normal(0, 2),
                                          class = "Intercept"),
                                    prior(normal(0, 1),
                                          class = "b"),
                                    prior(normal(0, 1),
                                          class = "sds"),
                                    prior(exponential(1),
                                          class = "sd")),
                                iter = 1000, chains = 2,
                                cores = 4,
                                combine = F
)

saveRDS(temperature_model, file = "code/temperature/temperature_model.rds")

