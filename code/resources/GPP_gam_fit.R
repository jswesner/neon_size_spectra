## to do 
# Confirm and clean status of a few streams
# add in units and confirm
# finalize workflow
rm(list = ls())
source(here::here("code/resources/01_load-packages.R"))

# exclude streams with GPP models still under constructions
streamsExclude = c()#c('LECO','MCRA','COMO','WLOU','BLUE')

streamsMod = streams[streams %ni% streamsExclude]

# load in mle models
# mleFiles = list.files(path = "./ignore/metab-models/", pattern = ".*_full_mle.rds", full.names = TRUE) %>% unlist
mleFiles = list.files(path = "./ignore/metab-models/", pattern = ".*mleEnsemble.rds", full.names = TRUE) %>% unlist


# mleModels = mleFiles %>% purrr::map(~readRDS(.x) %>% pluck('metab_daily') %>% dplyr::select(date, matches('GPP'))) %>% setNames(.,gsub("./ignore/metab-models/(.*_full_mle).rds","\\1",mleFiles)) %>% bind_rows(.id = "siteID")
mleModels = mleFiles %>% purrr::map(~readRDS(.x) %>% dplyr::select(date, matches('GPP'))) %>% setNames(.,gsub("./ignore/metab-models/(\\w{4})mleEnsemble.rds","\\1",mleFiles)) %>% bind_rows(.id = "siteID")

gppSumm = mleModels %>% ungroup %>% 
  # mutate(siteID = gsub("(\\w{4})_full_mle","\\1",siteID))) %>% 
  # mutate(siteID = stringr::str_sub(siteID, 1,4)) %>%
  dplyr::group_by(siteID) %>%
  dplyr::summarise(mean_GPP = mean(GPP, na.rm = TRUE))

gppFull = mleModels %>% ungroup %>%
  # dplyr::mutate(siteID = as.factor(gsub("(\\w{4})_full_mle","\\1",siteID))) %>% 
  dplyr::mutate(GPP = ifelse(GPP < 0.0001, NA, GPP)) %>% 
  dplyr::filter(!is.na(GPP)) %>% 
  group_by(siteID) %>% 
  dplyr::mutate(mean_GPP = mean(GPP, na.rm = TRUE),
         sd_gpp = sd(GPP, na.rm = TRUE),
         jday = lubridate::yday(date),
         mean_jday = mean(jday),  
         jday_c_100 = (jday - mean_jday)/100,
         # gpp_c_100 = (GPP - median_GPP)/100,
         # gpp_c = scale(GPP, center = TRUE, scale = TRUE),
         gpp_100 = GPP/100,
         log_gpp_100 = log(gpp_100),
         gpp_10 = GPP/10,
         year = lubridate::year(date),
         year_f = as.factor(year)) %>%
  dplyr::select(siteID, date, year_f, jday_c_100, GPP,  gpp_100, gpp_10, log_gpp_100)


gppFull %>% ggplot+
  geom_point(aes(x = jday_c_100, y = gpp_100, color = siteID)) +
  geom_smooth(aes(x = jday_c_100, y =  gpp_100, color = siteID, group = year_f), method = 'loess', span = 0.5, se=F)+
  coord_cartesian(ylim = c(0,NA))+
  viridis::scale_color_viridis(discrete = TRUE)+
  facet_wrap(~siteID, scales = 'free_y')+
  theme(legend.position = 'none')

library(brms)

gpp_c_split = split(gppFull, f = gppFull$siteID)

brm_gpp_fit <- brm_multiple(GPP ~ 
                  s(jday_c_100) + (1|year_f),
                family = Gamma(link = "log"),#inverse.gaussian(link="log"),#gaussian(),
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
                iter = 1000, chains = 2, combine = FALSE,
                backend = 'cmdstanr')

saveRDS(brm_gpp_fit, "./data/derived_data/brm_gpp_fit.rds")

##
gpp_conditionals <- NULL
brm_gpp_fit <- setNames(brm_gpp_fit, nm = names(gpp_c_split))
for(i in 1:length(brm_gpp_fit)){
   # brm_gpp_fit[[i]]$data %>%
     # distinct(tibble) %>%
     tibble(jday = 1:365) %>%
     dplyr::mutate(mean_jday = mean(jday),
                   jday_c_100 = (jday -mean_jday)/100) %>% 
     tidybayes::add_epred_draws(brm_gpp_fit[[i]],
                               re_formula = NA,
                               ndraws = 500) %>%
     mutate(model = names(brm_gpp_fit)[i]) %>%
     bind_rows() -> posts_tidy
   gpp_conditionals <- rbind(gpp_conditionals, posts_tidy)
                                  
 }

for(i in seq_along(names(brm_gpp_fit))){
  plot(conditional_effects(brm_gpp_fit[[i]]), points = T)
}
plot(conditional_effects(brm_gpp_fit[['SYCA']]), points = TRUE)

gpp_means = gpp_conditionals %>%
   dplyr::rename(siteID = 'model') %>% 
   dplyr::mutate(.epred = .epred) %>%
   left_join(gppFull %>% group_by(siteID) %>%
             dplyr::mutate(day_min = min(jday_c_100),
                   day_max = max(jday_c_100)) %>%
             distinct(siteID, day_min, day_max)) %>%
   dplyr::mutate(.epred = case_when(jday_c_100 < day_min ~ 0,
                             jday_c_100 > day_max ~ 0,
                             TRUE ~ .epred)) %>%
   group_by(siteID, .draw) %>% 
   dplyr::summarize(total_gpp = sum(.epred)) %>% 
   dplyr::summarize(mean = mean(total_gpp),
             sd = sd(total_gpp))

saveRDS(gpp_means, file = "./data/derived_data/gpp_means.rds")

gpp_means %>% 
  dplyr::mutate(cv = sd/mean) %>% print(n = 24)

library(janitor)
library(readr)

new_names <- list_of_data %>% 
  select(-names(brm_gpp_fit$data[1])) %>% 
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
