# temperature data
library(neonUtilities)
library(tidyverse)
library(janitor)
library(brms)
library(lubridate)
library(tidybayes)
library(modelr)
library(ggridges)
# wesner's token
neon_token <- "eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJqZWZmd2VzbmVyQGdtYWlsLmNvbSIsInNjb3BlIjoicmF0ZTpwdWJsaWMiLCJpc3MiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnLyIsImV4cCI6MTc4NjU1MjkxMiwiaWF0IjoxNjI4ODcyOTEyLCJlbWFpbCI6ImplZmZ3ZXNuZXJAZ21haWwuY29tIn0.VnIZyX8yUCBfQyLOtS2hxr_tB4JW2CBzD46QezlxnIKCc1biYv9BbVZvl72obmKP1uXu4iK_c2pzDmBFW_S9oA"

# NEON stream temps
# temp_all <- loadByProduct(
#   "DP1.20053.001",
#   package = "basic",
#   startdate = "2018-01",
#   enddate = "2021-12",
#   timeIndex = 30,   # ignore the one-minute data. Only download 30 minute data
#   token = neon_token,
#   check.size = FALSE
# )

#temp data with quality flags removed and super cold and super warm temps removed
temp_all <- readRDS("code/temperature/temp_data/temp_all.rds")


days_to_pull <- seq(1, 365, by = 4) # get data every 4 days

temp_all30_cleaned <- temp_all$TSW_30min %>% 
  as_tibble() %>% 
  filter(finalQF == 0) %>% # removes quality flags. (e.g., 0 = pass, 1 = fail) ~ 900K rows are removed by this
  mutate(date = as.Date(startDateTime),
         jday = yday(date),
         year = year(date),
         hour = hour(startDateTime)) %>% 
  filter(surfWaterTempMean > -5 & surfWaterTempMean <50)  %>% # remove extreme values
  filter(jday %in% days_to_pull) %>% 
  filter(hour %in% c(0, 6, 12, 18)) # limit to one measure every 4 hours



# mean temperature per week

test_temp <- temp_all30_cleaned %>% 
  filter(siteID != "OKSR" | siteID != "CARI") %>% # remove to add zeros later
  filter(year >= 2019) %>% 
  select(siteID, date, surfWaterTempMean)

test_temp_oksr_cari <- temp_all30_cleaned %>% 
  filter(siteID == "OKSR" | siteID == "CARI") %>%
  filter(year >= 2019) %>% # fill in missing dates
  group_by(siteID) %>% 
  complete(date = seq.Date(as_date("2019-01-01"), as_date("2021-12-31"), by="day")) %>% 
  mutate(month = month(date)) %>% 
  mutate(surfWaterTempMean = case_when(siteID == "OKSR" & month %in% c(10, 11, 12, 1, 2, 3, 4, 5) ~ 0,
                                       siteID == "CARI" & month %in% c(11, 12) ~ 0,
                                       TRUE ~ surfWaterTempMean)) %>% 
  select(siteID, date, surfWaterTempMean)

test_temp_data <- bind_rows(test_temp, test_temp_oksr_cari) %>%
  mutate(year = year(date),
         year_f = as.factor(year),
         week = week(date),
         jday = yday(date),
         julian = julian(date),
         hour = hour(date),
         month = month(date)) %>% 
  group_by(siteID) %>% 
  mutate(mean_water = mean(surfWaterTempMean, na.rm = T),
         mean_jday = mean(jday, na.rm = T),
         jday_c_100 = (jday - mean_jday)/100,
         water_c_10 = (surfWaterTempMean - mean_water)/10) %>%
  ungroup() 


test_temp_data %>% 
  ggplot(aes(x = jday_c_100, y = water_c_10, color = year_f)) + 
  geom_point(size = 0.1) + 
  facet_wrap(~siteID)


# make lists by site

temp_data_list <- split(test_temp_data, f = test_temp_data$siteID)

# run prior model
test_brm_priors <- brm(water_c_10 ~  s(jday_c_100, by = siteID) + (1|year_f),
                       family = gaussian(),
                       data = test_temp_data %>% filter(siteID == "OKSR" | siteID == "ARIK"),
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
                       sample_prior = "only"
)

conditional_effects(test_brm_priors, effects = "jday_c_100:siteID", re_formula = NULL)

saveRDS(test_brm_priors, file = "code/temperature/models/test_brm_priors.rds")


# fit to data
# 
# 
# test_brm_posterior <- brm_multiple(water_c_10 ~  s(jday_c_100) + (1|year_f),
#                                 family = gaussian(),
#                                 data = temp_data_list,
#                                 prior =
#                                   c(prior(normal(0, 2),
#                                           class = "Intercept"),
#                                     prior(normal(0, 1),
#                                           class = "b"),
#                                     prior(normal(0, 1),
#                                           class = "sds"),
#                                     prior(exponential(1),
#                                           class = "sd")),
#                                 iter = 1000, chains = 2,
#                                 # sample_prior = "only",
#                                 cores = 4,
#                                 combine = F
# )


# 
# saveRDS(test_brm_posterior, file = "code/temperature/models/test_brm_posterior.rds")

test_brm_posterior <- readRDS(file = "code/temperature/models/test_brm_posterior.rds")


# extract posteriors
posts <- NULL

for(i in 1:24) {
  test_brm_posterior[[i]]$data %>% 
    distinct(jday_c_100) %>% 
    add_epred_draws(test_brm_posterior[[i]], 
                    re_formula = NA,
                    ndraws = 500) %>% 
    mutate(model = i) %>% 
    bind_rows() -> posts_tidy
  posts <- rbind(posts, posts_tidy)
}

sites_models <- test_temp_data %>% 
  distinct(siteID) %>% 
  arrange(siteID) %>% 
  mutate(model = row_number())

models_conversion_data <- test_temp_data %>% 
  distinct(siteID, jday_c_100, year_f, mean_jday,
           mean_water) %>% 
  left_join(sites_models) 

water_posteriors <- posts %>% 
  left_join(models_conversion_data) %>% 
  mutate(jday = jday_c_100*100 + mean_jday,
         water = .epred*10 + mean_water) %>% 
  distinct(.draw, model, .keep_all = TRUE) %>%  # removes redundant outputs
  select(-year_f) # model is not year dependent - only intercept is. Remove for summaries

saveRDS(water_posteriors, file = ("code/temperature/posteriors/water_posteriors.rds"))

# plot posteriors
summary_posts <- water_posteriors %>% 
  ungroup() %>% 
  select(siteID, jday, water) %>% 
  group_by(siteID, jday) %>% 
  median_qi(water)

saveRDS(summary_posts, file = ("code/temperature/posteriors/summary_posts.rds"))



# plot
summary_posts %>% 
  ggplot(aes(x = jday, y = water)) +
  geom_point(data = test_temp_data, aes(color = year_f, 
                                        y = surfWaterTempMean), size = 0.1,
             alpha = 0.8) +
  geom_line() + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = 0.5) +
  facet_wrap(~siteID) +
  labs(y = "Stream Temp C",
       x = "Day of the Year",
       title = "NEON Data 2019-2021") +
  NULL
  


# Mean Annual Temperature -------------------------------------------------
mat_posts <- water_posteriors %>%
  group_by(.draw, siteID) %>% 
  summarize(mat_draw = mean(water)) %>% # mat for each draw
  group_by(siteID) %>% 
  summarize(mat_site = mean(mat_draw),
            sdat_site = sd(mat_draw))# mat for each site

saveRDS(mat_posts, file = "code/temperature/posteriors/mat_posts.rds")


water_posteriors %>%
  group_by(.draw, siteID) %>% 
  summarize(mat_draw = mean(water)) %>% 
  group_by(siteID) %>% 
  mutate(order = mean(mat_draw)) %>% 
  ggplot(aes(x = mat_draw, y = reorder(siteID, order), fill = order)) + 
  geom_density_ridges()
  

# compare to air temps
field_data <- read_csv("data/field_data.csv") %>% clean_names()

mat_posts %>% clean_names %>% 
  left_join(field_data) %>% 
  ggplot(aes(x = mat_site, y = mat_c, xmin = mat_site - sdat_site, xmax = mat_site + sdat_site)) +
  geom_pointrange()
