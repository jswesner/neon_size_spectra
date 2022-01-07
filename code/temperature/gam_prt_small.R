# brm model for prt_small
# starting with data downloaded 
# ~ line 122 in gam_fit_template.R

library(brms)
library(tidyverse)
prt_small <- readRDS("code/temperature/temp_data/prt_small.RDS")

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

