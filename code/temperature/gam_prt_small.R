# brm model for prt_small
# starting with data downloaded 
# ~ line 122 in gam_fit_template.R

library(brms)
library(tidyverse)
#stream temps measured every 30 minutes
prt <- readRDS("data/raw_data/prt.rds")

# thin stream temp data
# Look at finalQF
prt_short <- prt %>%
  mutate(
    date = as_date(startDateTime),
    hour = hour(startDateTime),
    day = day(date),
    year = year(date),
    jdate = julian(date)) %>% 
  filter(hour %in% c(0, 4, 8, 12, 16, 20)) %>% # limit to one measure every 4 hours 
  # slice(which(row_number() %% 10 == 1)) %>% # limit to every 5 days
  distinct(siteID, surfWaterTempMean, startDateTime, day, hour, date, jdate, year) %>% 
  filter(surfWaterTempMean > -10 & surfWaterTempMean <50)  # remove extreme values (sensor errors)


# plot raw data
prt_short %>% 
  ggplot(aes(x = date, y = surfWaterTempMean, group = siteID)) + 
  geom_line(aes(group = siteID, color = siteID)) +
  facet_wrap(~siteID) +
  NULL


get_prior(water_temp ~ s(jdate, by = siteID),
          family = gaussian(),
          data = prt_small)

brm_temp <- brm(water_temp ~ 
                  s(jdate, by = siteID) +
                  (1|siteID),
                family = gaussian(),
                data = prt_small,
                prior = 
                  c(prior(normal(0, 1),
                          class = "Intercept"),
                    prior(normal(0, 1),
                          class = "b"),
                    prior(normal(0, 1),
                          class = "sds"),
                    prior(exponential(1),
                          class = "sd")),
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

