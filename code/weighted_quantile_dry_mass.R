# SI: body size quantile regression

# this script takes the body size estimates of macroinvertebrates and fish and calculates the relative change of large and small body sizes across the environmental gradient. 
# We estimate this by using weighted quantile regression. The body sizes present at a site are weighted by their densities within a sample. 
# A sample is a unique combination of [site + date]


# Preliminaries -----------------------------------------------------------

# load libraries
library(Hmisc)
library(tidyverse)

### load derived data products

# macros + fish dry mass estimates and abundances
dw <- readRDS(file = "data/derived_data/macro_fish_dw.rds")
# mean and sd or temperature posterior distirbution
temp <- readRDS("code/temperature/posteriors/mat_posts.rds")


### combine dw and temperature data
dw <- left_join(dw, temp, by = c("site_id" = "siteID"))


# weighted quantiles ------------------------------------------------------

dw_wq <- dw %>%
  group_by(ID) %>%
  summarise(mat_site = unique(mat_site),
            wq05 = wtd.quantile(
              dw,
              weights = no_m2,
              probs = 0.05),
            wq50 = wtd.quantile(
              dw,
              weights = no_m2,
              probs = 0.5),
            wq95 = wtd.quantile(
              dw,
              weights = no_m2,
              probs = 0.95),
            wq99 = wtd.quantile(
              dw,
              weights = no_m2,
              probs = 0.99),
            wq100 = wtd.quantile(
              dw,
              weights = no_m2,
              probs = 1))

# raw body size, OLS regression
dw_wq %>%
  pivot_longer(cols = 3:7) %>%
  ggplot(aes(x = mat_site,
             y = value, 
             group = name,
             linetype = name,
             color = name,
             shape = name,
             fill = name))+
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()+
  labs(y = "Body Size: Raw",
       x = 
         expression(
           "Mean Annual Water Temperature " ( degree*C))) +
  NULL

# raw body size, Loess regression
dw_wq %>%
  pivot_longer(cols = 3:7) %>%
  ggplot(aes(x = mat_site,
             y = value, 
             group = name,
             linetype = name,
             color = name,
             shape = name,
             fill = name))+
  geom_point() +
  stat_smooth() +
  theme_bw()+
  labs(y = "Body Size: Raw",
       x = 
         expression(
           "Mean Annual Water Temperature " ( degree*C))) +
  NULL

# Log10 body size, OLS regression
dw_wq %>%
  pivot_longer(cols = 3:7) %>%
  ggplot(aes(x = mat_site,
             y = value, 
             group = name,
             linetype = name,
             color = name,
             shape = name,
             fill = name))+
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()+
  scale_y_log10() +
  labs(y = "Log 10 Body Size",
       x = 
         expression(
           "Mean Annual Water Temperature " ( degree*C))) +
  NULL

# Log10 body size, Loess regression
dw_wq %>%
  pivot_longer(cols = 3:7) %>%
  ggplot(aes(x = mat_site,
             y = value, 
             group = name,
             linetype = name,
             color = name,
             shape = name,
             fill = name))+
  geom_point() +
  stat_smooth() +
  theme_bw()+
  scale_y_log10() +
  labs(y = "Log 10 Body Size",
       x = 
         expression(
           "Mean Annual Water Temperature " ( degree*C))) +
  NULL

# Linear regressions of weighted quantile values across temperature
summary(lm(wq05~mat_site, data = dw_wq))
summary(lm(wq50~mat_site, data = dw_wq))
summary(lm(wq95~mat_site, data = dw_wq))
summary(lm(wq99~mat_site, data = dw_wq))

### Fitted values

# sequence min to max temperature values
x <- seq(min(dw$mat_site), max(dw$mat_site))
# calculate fitted values at different quantiles
y05 = 2.9e-3 + 1.7e-5 * x
y50 = 0.025 + -0.0002 * x
y95 = 0.32 + 0.007 * x
y999 = 1.23 + 0.058 * x

as_tibble(data.frame(x = x, 
                     y05 = y05, 
                     y50 = y50,
                     y95 = y95,
                     y999 = y999)) %>%
  pivot_longer(2:5) %>%
  ggplot(aes(x = x, y = value, color = name)) +
  geom_line(size = 1) +
  scale_y_log10() +
  theme_bw()


# Unweighted quantile regressions -----------------------------------------

# pretty sure this is not right becasue we need to "scale" 
dw_q <- dw %>%
  group_by(ID) %>%
  summarize(mat_site = unique(mat_site),
            q05 = quantile(dw, probs = 0.05),
            q50 = quantile(dw, probs = 0.5),
            q95 = quantile(dw, probs = 0.95),
            q99 = quantile(dw, probs = 0.99))

dw_q %>%
  pivot_longer(cols = 3:6) %>%
  ggplot(aes(x = mat_site,
             y = value, 
             group = name,
             linetype = name,
             color = name,
             shape = name,
             fill = name))+
  geom_point() +
  scale_y_log10() +
  stat_smooth(method = "lm") +
  theme_bw() +
  NULL
