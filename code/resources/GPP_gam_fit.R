rm(list = ls())
source("./code/resources/01_load-packages.R")

# exclude streams with GPP models still under constructions
streamsExclude = c('LECO','MCRA','COMO','WLOU','BLUE')

streamsMod = streams[streams %ni% streamsExclude]

# load in mle models

mleFiles = list.files(path = "./ignore/metab-models/", pattern = ".*_full_mle.rds", full.names = TRUE) %>% unlist

mleModels = mleFiles %>% purrr::map(~readRDS(.x) %>% pluck('metab_daily') %>% dplyr::select(date, matches('GPP'))) %>% setNames(.,gsub("./ignore/metab-models/(.*_full_mle).rds","\\1",mleFiles)) %>% bind_rows(.id = "siteID")

gppSumm = mleModels %>% ungroup %>% 
  # mutate(siteID = gsub("(\\w{4})_full_mle","\\1",siteID))) %>% 
  mutate(siteID = stringr::str_sub(siteID, 1,4)) %>%
  dplyr::group_by(siteID) %>%
  summarise(mean_GPP = mean(GPP, na.rm = TRUE))

gppFull = mleModels %>% ungroup %>%
  dplyr::mutate(siteID = as.factor(gsub("(\\w{4})_full_mle","\\1",siteID))) %>% 
  dplyr::mutate(GPP = ifelse(GPP < 0, NA, GPP)) %>% 
  dplyr::filter(!is.na(GPP)) %>% 
  group_by(siteID) %>% 
  mutate(mean_GPP = mean(GPP, na.rm = TRUE),
         sd_gpp = sd(GPP, na.rm = TRUE),
         jday = lubridate::yday(date),
         mean_jday = mean(jday),  
         jday_c_100 = (jday - mean_jday)/100,
         # gpp_c_100 = (GPP - median_GPP)/100,
         gpp_c = scale(GPP, center = TRUE, scale = TRUE),
         year = lubridate::year(date),
         year_f = as.factor(year)) %>%
  dplyr::select(siteID, date, year_f, jday_c_100, gpp_c)


gppFull %>% ggplot+
  geom_point(aes(x = jday_c_100, y = gpp_c_100, color = siteID)) +
  geom_smooth(aes(x = jday_c_100, y =  gpp_c_100, color = siteID, group = year_f), method = 'loess', span = 0.5, se=F)+
  viridis::scale_color_viridis(discrete = TRUE)+
  facet_wrap(~siteID, scales = 'free_y')+
  theme(legend.position = 'none')

library(brms)

gpp_c_split = split(gppFull, f = gppFull$siteID)

brm_gpp <- brm_multiple(gpp_c ~ 
                  s(jday_c_100) + (1|year_f),
                family = gaussian(),
                data = gpp_c_split,
                prior = 
                  c(prior(normal(0, 2),
                          class = "Intercept"),
                    prior(normal(0, 1),
                          class = "b"),
                    prior(normal(0, 1),
                          class = "sds"),
                    prior(exponential(1),
                          class = "sd")),
                iter = 500, chains = 1, combine = FALSE, 
                backend = 'cmdstanr')

saveRDS(brm_gpp, "./data/derived_data/brm_gpp_fit.rds")

# plot(conditional_effects(brm_gpp, effects = "jdate:siteID", re_formula = NULL),
#      points = T)

# extract posterior on each sample date

brm_gpp <- setNames(brm_gpp, nm = names(gpp_c_split))



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
