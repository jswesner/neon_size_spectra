source("./code/resources/01_load-packages.R")

# exclude streams with GPP models still under constructions
streamsExclude = c('LECO','MCRA','COMO','WLOU','BLUE')

streamsMod = streams[streams %ni% streamsExclude]

# load in mle models

mleFiles = list.files(path = "./ignore/metab-models/", pattern = ".*_full_mle.rds", full.names = TRUE) %>% unlist


mleModels = mleFiles %>% purrr::map(~readRDS(.x) %>% pluck('fit') %>% dplyr::select(date, matches('GPP.daily'))) %>% setNames(.,gsub("./ignore/metab-models/(.*_full_mle).rds","\\1",mleFiles)) %>% bind_rows(.id = "siteID")

gppFull = mleModels %>%
  dplyr::mutate(siteID = gsub("(\\w{4})_full_mle","\\1",siteID),
                jdate = lubridate::yday(date),
                year = lubridate::year(date)) %>%
  dplyr::select(siteID, date, year, jdate, GPP.daily) %>% 
  filter(!is.na(GPP.daily)) %>%
  dplyr::mutate(GPP.daily = ifelse(GPP.daily < 0, 0, GPP.daily))

gppSmall = gppFull %>% dplyr::filter(year == 2019)

gppFull %>% ggplot+
  geom_point(aes(x = jdate, y = GPP.daily, color = siteID)) +
  geom_smooth(aes(x = jdate, y =  GPP.daily, color = siteID, group = year), method = 'loess', span = 0.5)+
  viridis::scale_color_viridis(discrete = TRUE)+
  facet_wrap(~siteID, scales = 'free_y')

library(brms)

get_prior(GPP.daily ~ s(jdate, by = siteID),
          family = gaussian(),
          data = gppFull)

brm_gpp <- brm(GPP.daily ~ 
                  s(jdate, by = siteID) +
                  (1|siteID),
                family = gaussian(),
                data = gppFull,
                prior = 
                  c(prior(normal(0, 1),
                          class = "Intercept"),
                    prior(normal(0, 1),
                          class = "b"),
                    prior(normal(0, 1),
                          class = "sds"),
                    prior(exponential(1),
                          class = "sd")),
                iter = 500, chains = 1, backend = 'cmdstanr')

plot(conditional_effects(brm_gpp, effects = "jdate:siteID", re_formula = NULL),
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
