library(tidyverse)
library(tidybayes)

temperature_model = readRDS(file = "code/temperature/temperature_model.rds")
mean_weekly_temperatures = readRDS(file = "data/derived_data/mean_weekly_temperatures.rds")
mean_weekly_temperatures_list <- split(mean_weekly_temperatures, f = mean_weekly_temperatures$siteID)


# data to match "model" to its corresponding site ID
models_conversion_data <- mean_weekly_temperatures %>% 
  distinct(siteID, jday_c_100, year_f, mean_jday,
           mean_water)

# extract posteriors
temperature_conditionals <- NULL

brm_temperature_fit = setNames(temperature_model, nm = names(mean_weekly_temperatures_list))

for(i in 1:24) {
  temperature_model[[i]]$data %>% 
    distinct(jday_c_100) %>% 
    add_epred_draws(temperature_model[[i]], 
                    re_formula = NA,
                    ndraws = 500) %>% 
    mutate(model = names(temperature_model)[i]) %>% 
    bind_rows() -> posts_tidy
  temperature_conditionals <- rbind(temperature_conditionals, posts_tidy) 
}

temperature_posteriors = temperature_conditionals %>% 
  left_join(models_conversion_data) %>% 
  mutate(jday = jday_c_100*100 + mean_jday,
         temperature = .epred*10 + mean_water) %>% 
  distinct(.draw, model, .keep_all = TRUE) %>%  # removes redundant outputs
  select(-year_f) # model is not year dependent - only intercept is. Remove for summaries

saveRDS(temperature_posteriors, file = "data/derived_data/temperature_posteriors.rds")

# plot posteriors
temperature_mean_annual <- temperature_posteriors %>% 
  ungroup() %>% 
  select(siteID, jday, temperature) %>% 
  group_by(siteID) %>% 
  summarize(mean = mean(temperature),
            sd = sd(temperature))

saveRDS(temperature_mean_annual, file = ("code/temperature/temperature_mean-annual.rds"))

temperature_posteriors %>% 
  filter(model == "ARIK") %>% 
  ungroup() %>% 
  group_by(siteID, jday) %>% 
  median_qi(.epred) %>% 
  ggplot(aes(x = jday, y = .epred)) + 
  geom_line()



