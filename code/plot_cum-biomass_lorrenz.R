# Biomass exploration plots

library(tidyverse)

# load in data
b_dat <- macro_fish_dw <- readRDS("data/derived_data/macro_fish_dw.rds")

# sum the total biomass per collection
b_sum <- b_dat %>%
  group_by(site_id, year, month, mat_s) %>%
  mutate(dw_density = dw * no_m2) %>%
  summarize(b_tot = sum(dw_density, na.rm = TRUE))

# plot total biomass per collection across mean annual water temperature
ggplot(data = b_sum,
       aes(x = mat_s,
           y = b_tot,
           color = mat_s)) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  scale_y_log10() +
  theme_bw() +
  geom_smooth(method = "lm")



# gglorenz ----------------------------------------------------------------
library(gglorenz)
ggplot(billionaires, aes(TNW)) +
  stat_lorenz() +
  geom_abline(linetype = "longdash") +
  theme_bw()

small_dat <- b_dat %>%
  filter(site_id == "ARIK", year_month == "2017_10")

small_dat %>%
  select(dw, no_m2) %>%
  mutate(
    rel_body = 
      dw / max((dw),
          na.rm = TRUE)) %>%
  arrange(rel_body) %>%
  mutate(y_sum = cumsum(no_m2),
         rel_sum = y_sum / max(y_sum, na.rm = TRUE)) %>%
  ggplot(aes(x = rel_body,
              y = rel_sum)) +
  geom_point() +
  #stat_smooth() +
  geom_abline(linetype = "dashed")+
  scale_y_log10() +
  scale_x_log10() +
  coord_fixed(ylim = c(0.001, NA))

small_dat2 <- b_dat %>%
  filter(site_id == "CUPE", year_month == "2018_1")


small_dat2 %>%
  select(dw, no_m2) %>%
  mutate(
    rel_body = 
      dw / max((dw),
               na.rm = TRUE)) %>%
  arrange(rel_body) %>%
  mutate(y_sum = cumsum(no_m2),
         rel_sum = y_sum / max(y_sum, na.rm = TRUE)) %>%
  ggplot(aes(x = rel_body,
             y = rel_sum)) +
  geom_point() +
  #stat_smooth() +
  geom_abline(linetype = "dashed") +
  scale_y_log10() +
  scale_x_log10()+
  coord_fixed(ylim = c(0.001, NA))

small_dat3 <- b_dat %>%
  filter(site_id == "ARIK" |
           site_id == "CUPE",
         year_month == "2018_1" |
         year_month == "2017_10")

small_dat2 %>%
  select(dw, no_m2) %>%
  mutate(
    rel_body = 
      dw / max((dw),
               na.rm = TRUE)) %>%
  arrange(rel_body) %>%
  mutate(y_sum = cumsum(no_m2),
         rel_sum = y_sum / max(y_sum, na.rm = TRUE)) %>%
  ggplot(aes(x = rel_body,
             y = rel_sum)) +
  geom_point() +
  #stat_smooth() +
  geom_abline(linetype = "dashed") + 
  scale_y_log10() +
  scale_x_log10()


# violin ------------------------------------------------------------------
b_dat %>%
  filter(year == 2019) %>%
  ggplot(
       aes(x = reorder(site_id,
                       mat_s), 
           y = dw, 
           weight = no_m2,
           fill = mat_s)) +
  geom_violin(trim = FALSE) +
  geom_boxplot() +
  scale_y_log10() +
  coord_flip()

b_dat %>%
  group_by(site_id, year, month, mat_s) %>%
  summarize(
    p90 = cNORM::weighted.quantile(
      dw, probs = 0.9,
      weights = no_m2),
    p98 = cNORM::weighted.quantile(
      dw, probs = 0.98,
      weights = no_m2)
    # p70 = cNORM::weighted.quantile(
    #   dw, probs = 0.7,
    #   weights = no_m2),
    # p50 = cNORM::weighted.quantile(
    #   dw, probs = 0.5,
    #   weights = no_m2),
    # p30 = cNORM::weighted.quantile(
    #   dw, probs = 0.3,
    #   weights = no_m2),
    # p10 = cNORM::weighted.quantile(
    #   dw, probs = 0.1,
    #  weights = no_m2)
    ) %>% 
  pivot_longer(c("p90":"p98")) %>%
  ggplot(aes(x = mat_s, 
             y = value,
             color = mat_s)) +
  geom_point() +
  scale_y_log10() +
  facet_wrap(.~ name) +
  scale_color_viridis_c(option = "plasma") +
  theme_bw() +
  stat_smooth(method = "lm")

b_dat %>%
  filter(site_id == "GUIL" |
           site_id == "OKSR") %>%
  ggplot(aes(x = dw,
             weight = no_m2,
             fill = site_id,
             group = year_month)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  theme_bw() +
  facet_wrap(.~year)

b_dat %>%
  filter(site_id == "GUIL" |
           site_id == "OKSR",
         year == 2019) %>%
  ggplot(aes(x = dw,
             weight = no_m2,
             fill = site_id)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  theme_bw() +
  NULL

sample_dat <- small_dat %>%
  mutate(rel_density = no_m2 / max(no_m2, na.rm = TRUE))

x <- sample(size = 1000,
       x = sample_dat$dw,
       prob = sample_dat$rel_density,
       replace = TRUE)
rel_x <- sort(x / max(x))
sim_dat <- data.frame(x = rel_x,
           y = cumsum(rel_x))

sim_dat %>%
  mutate(rel_y = y / max(y)) %>%
  ggplot(aes(x = rel_x, 
         y = rel_y)) +
  geom_point() +
  geom_abline(linetype = "dashed") +
  theme_bw() +
  stat_smooth()

# tot_sim_dat_source <- b_dat %>% 
#   select(site_id, year, month,
#          dw, no_m2) %>%
#   mutate(rel_density = no_m2 / 
#            max(no_m2, na.rm = TRUE))
# 
# tot_sim_dat <- tot_sim_dat_source %>%
#   select(site_id, year, month) %>%
#   distinct() %>%
#   uncount(1000)
# 
# tot_sim_dat %>%
#   mutate(sim_body = sample())

b_dat %>%
  filter(year == 2017 |
           year == 2018) %>%
  select(site_id, year, month,
         dw, no_m2, mat_s, animal_type) %>%
  group_by(site_id) %>% #, year, month
  mutate(rel_density = no_m2 / max(no_m2, na.rm = TRUE)) %>%
  slice_sample(n = 100,
              replace = TRUE,
              weight_by = rel_density) %>%
  mutate(
    rel_x = dw / max(dw, na.rm = TRUE)) %>%
  arrange(rel_x) %>%
  mutate(
    y = cumsum(dw),
    rel_y = y / max(y)) %>%
  ggplot(aes(x = rel_x, 
             y = rel_y,
             group = mat_s,
             color = animal_type)) +
  geom_point() +
  geom_abline(linetype = "dashed") +
  theme_bw() +
  #stat_smooth() +
  facet_wrap(.~reorder(site_id, mat_s)) +
  scale_y_log10() +
  scale_x_log10() +
  geom_abline(slope = -1,
              intercept = -3.33) +
  NULL

b_dat %>%
  filter(site_id == "ARIK") %>%
  group_by(site_id) %>%
  mutate(
    dw_density = dw * no_m2,
    resid3 =
      (dw - mean(dw, na.rm = TRUE))^3) %>%
  summarize(sum_resid = sum(resid3),
            dw_n = n(),
            sd_dw = sd(dw)^3) %>%
  mutate(skew = (sum_resid / dw_n) / sd_dw)

#### 
install.packages("moments")
library(moments)

b_dat %>%
  filter(site_id == "ARIK") %>%
  summarise(skewness(dw))


b_dat %>%
  filter(year != 2020 & year != 2016) %>%
  select(site_id, year, month,
         dw, no_m2, mat_s, animal_type) %>%
  group_by(site_id, mat_s, year) %>% #, year, month
  mutate(rel_density = no_m2 / max(no_m2, na.rm = TRUE)) %>%
  slice_sample(n = 10000,
               replace = TRUE,
               weight_by = rel_density) %>%
  summarise(skew = skewness(log10(dw))) %>%
  ggplot(aes(x = mat_s,
         y = skew#,
         #group = year,
         #color = year
         )) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)
