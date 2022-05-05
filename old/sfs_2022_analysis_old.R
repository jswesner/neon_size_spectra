library(brms)
library(tidyverse)
library(tidybayes)
library(janitor)
library(ggridges)
library(sizeSpectra)
library(scales)
library(ggthemes)

# mle estimates from January Grand Junction meeting. Includes fish and macroinvertebrates
mle_output <- readRDS("results/mle_b_estimate_fish_macro.RDS")

# load temp estimates mean annual water temperatures
mat_posts <- readRDS("code/temperature/posteriors/mat_posts.rds") %>% clean_names

# data to analyze mle as a function of temperature
mle_mat <- mle_output %>% left_join(mat_posts)



# fit model
brm_mle_sfs <- brm(b|mi(stdErr) ~ me(mat_site, sdat_site) + (1|site_id),
               data = mle_mat,
               family = gaussian(),
               file = "code/mle/results/brm_mle_sfs.rds", 
               file_refit = "on_change")


# check model
pp_check(brm_mle_sfs)

# conditional plot
plot(conditional_effects(brm_mle_sfs, re_formula = NA), points = T)

# extract posteriors and summarize slope
posts <- as_draws_df(brm_mle_sfs) %>% as_tibble()

posts %>% 
  median_qi(bsp_memat_sitesdat_site)

posts %>% 
  summarize(prob_negative = sum(bsp_memat_sitesdat_site <0)/nrow(.))


# site specific predictions
post_preds <- brm_mle_sfs$data %>% 
  distinct(site_id, mat_site, stdErr, sdat_site) %>% 
  add_predicted_draws(brm_mle_sfs, re_formula = NULL)

# plot
post_preds %>% 
  ggplot(aes(x = .prediction, fill = mat_site, y = reorder(site_id, mat_site))) + 
  geom_point(data = brm_mle_sfs$data, aes(x = b), shape = "|") +
  geom_density_ridges(alpha = 0.2)
  


# plot spectra ------------------------------------------------------------
### Current plotting doesn't work with betas from the 01_first_ml_b_approx.R result
### Check this with Justin. For now, just run a single b and plot it as
### an example for SFS poster.

#load raw data (fish + macros)
dat <- readRDS("data/derived_data/macro_fish_dw.rds")

# join with posteriors
dat_to_plot <- dat %>% ungroup() %>% 
  select(site_id, year_month, dw, animal_type) %>% 
  left_join(mle_mat) %>% 
  group_by(site_id, year_month) %>% 
  arrange(site_id, year_month, dw) %>% 
  mutate(group = paste(site_id, year_month, sep = "_")) 

x = dat_to_plot %>% filter(site_id == "ARIK") %>% 
  filter(year_month == "2018_10") %>% select(dw) %>% pull()

eight.results <- eightMethodsMEE(x = x)

mle_test <- (pPLB(x = x, 
                  b = eight.results$hMLE.list$b,
                  xmin = min(x),
                  xmax = max(x)))

mle_lower <- (pPLB(x = x, 
                  b = eight.results$hMLE.list$confVals[2],
                  xmin = min(x),
                  xmax = max(x)))

mle_upper <- (pPLB(x = x, 
                  b = eight.results$hMLE.list$confVals[1],
                  xmin = min(x),
                  xmax = max(x)))


sfs_spectra_fig <- tibble(x = x,
       x_prop = x/max(x),
       mle_test = 1 - mle_test,
       mle_test_y = mle_test*length(x),
       mle_lower_y = (1- mle_lower)*length(x),
       mle_upper_y = (1 - mle_upper)*length(x),
       dw = x,
       animal_type = dat %>% ungroup() %>% filter(site_id == "ARIK") %>% 
                     filter(year_month == "2018_10") %>%
         select(dw, animal_type) %>% 
         arrange(dw) %>% 
         select(-dw) %>% pull()) %>% 
  mutate(count_greater = map_int(x, ~ sum(.x < x))) %>%
  ggplot(aes(x = x, y = count_greater + 1)) + 
  geom_line(aes(y = mle_test_y)) + 
  geom_ribbon(aes(ymin = mle_lower_y, ymax = mle_upper_y),
              alpha = 0.2) +
  geom_point(aes(color = animal_type, shape = animal_type), 
             size = 4) + 
  scale_y_log10() + 
  scale_x_log10(labels = c("0.01", "1", "100", "10,000"),
                breaks = c(0.01, 1, 100, 10000)) + 
  labs(x = "Individual dry mass (mg)", 
       color = "Organism",
       shape = "Organism",
       y = "Number of values \u2265 x",
       subtitle = "Arikeree River, Colorado (ARIK) - Oct. 2018") + 
  coord_cartesian(ylim = c(1, 1000)) +
  scale_color_colorblind() +
  theme_default() + 
  theme(text = element_text(size = 38)) + 
  guides(color = "none",
         shape = "none")

ggsave(sfs_spectra_fig, file = "plots/sfs_spectra_fig.jpg", dpi = 600, width = 12, height = 11)




# MAYF --------------------------------------------------------------------

x = dat_to_plot %>% filter(site_id == "MAYF") %>% 
  filter(year_month == "2018_10") %>% select(dw) %>% pull()

eight.results <- eightMethodsMEE(x = x)

mle_test <- (pPLB(x = x, 
                  b = eight.results$hMLE.list$b,
                  xmin = min(x),
                  xmax = max(x)))

mle_lower <- (pPLB(x = x, 
                   b = eight.results$hMLE.list$confVals[2],
                   xmin = min(x),
                   xmax = max(x)))

mle_upper <- (pPLB(x = x, 
                   b = eight.results$hMLE.list$confVals[1],
                   xmin = min(x),
                   xmax = max(x)))


sfs_spectra_fig_MAYF <- tibble(x = x,
                          x_prop = x/max(x),
                          mle_test = 1 - mle_test,
                          mle_test_y = mle_test*length(x),
                          mle_lower_y = (1- mle_lower)*length(x),
                          mle_upper_y = (1 - mle_upper)*length(x),
                          dw = x,
                          animal_type = dat %>% ungroup() %>% filter(site_id == "MAYF") %>% 
                            filter(year_month == "2018_10") %>%
                            select(dw, animal_type) %>% 
                            arrange(dw) %>% 
                            select(-dw) %>% pull()) %>% 
  mutate(count_greater = map_int(x, ~ sum(.x < x))) %>%
  ggplot(aes(x = x, y = count_greater + 1)) + 
  geom_line(aes(y = mle_test_y)) + 
  geom_ribbon(aes(ymin = mle_lower_y, ymax = mle_upper_y),
              alpha = 0.2) +
  geom_point(aes(color = animal_type, shape = animal_type), 
             size = 4) + 
  scale_y_log10() + 
  scale_x_log10(labels = c("0.01", "1", "100", "10,000"),
                breaks = c(0.01, 1, 100, 10000)) + 
  labs(x = "Individual dry mass (mg)", 
       color = "Organism",
       shape = "Organism",
       y = "Number of values \u2265 x",
       subtitle = "MAYF - Oct. 2018") + 
  coord_cartesian(ylim = c(1, 1000)) +
  scale_color_colorblind() +
  theme_default() + 
  theme(text = element_text(size = 20)) + 
  guides(color = "none",
         shape = "none")

ggsave(sfs_spectra_fig_MAYF, file = "plots/sfs_spectra_fig_MAYF.jpg", dpi = 600, width = 8, height = 7)



# ALL SITES  --------------------------------------------------------------------

dat_with_exponents <- dat_to_plot %>% 
  select(-b) %>% 
  left_join(extract_exponents) %>% 
  select(group, dw, b, lower, upper) %>% 
  pivot_longer(cols = c(b, lower, upper)) %>% 
  group_by(group, name, value) %>% 
  mutate(min_x = min(dw),
         max_x = max(dw)) %>% 
  # mutate(dw_list = list(dw)) %>% 
  ungroup() %>% 
  mutate(group = paste(group, name, sep = "_"))


compute_mle_line <- dat_with_exponents %>%
  split(.$group) %>% 
  map(~pPLB(x = .$dw,
            b = unique(.$value),
            xmin = unique(.$min_x),
            xmax = unique(.$max_x)))


extract_mle_line <- compute_mle_line %>% unlist() %>% rbind() %>% as_tibble() %>% 
  pivot_longer(cols = everything()) %>% 
  separate(name, into = c("site_id", "year", "month", "param_rep")) %>% 
  mutate(order = parse_number(param_rep),
         param = str_sub(param_rep, 1, 1),
         value = 1 - value) %>% 
  select(-param_rep) %>% 
  pivot_wider(names_from = "param", values_from = value) %>% 
  rename(mean = b) %>% 
  mutate(year_month = paste(year, month, sep = "_"))
  

add_dw_to_mle <- extract_mle_line %>% 
  mutate(dw = dat_to_plot %>% pull(dw),
         animal_type = dat_to_plot %>% pull(animal_type)) %>% 
  group_by(site_id, year, month) %>% 
  mutate(length_x = length(dw),      # get number of organisms
         mean = mean*length_x,       # multiply proportion by number of organisms (e.g., 100% of 191 organisms, should have 191 as the top value on the y-axis)
         l = l*length_x,
         u = u*length_x,
         count_greater = map_int(dw, ~ sum(.x < dw))) %>% # compute number >= x for raw data
  left_join(dat_with_exponents %>% filter(name == "b") %>% distinct(site_id, year_month, value) %>% 
              rename(b = value)) # add beta values


library(viridis)

all_mle_slopes <- add_dw_to_mle %>% 
  ggplot(aes(x = dw, y = count_greater)) + 
  geom_line(aes(y = mean, group = interaction(site_id, year_month)),
            alpha = 0.5) +
  scale_y_log10() + 
  scale_x_log10() + 
  labs(x = "Individual dry mass (mg)", 
       color = expression(italic("b")),
       y = "Number of values \u2265 x") +
  theme_default() + 
  theme(text = element_text(size = 20)) +
  NULL


ggsave(all_mle_slopes, file = "plots/all_mle_slopes.jpg", dpi = 600, width = 8, height = 7)




# Check exponents against alternative method from Janurary ----------------

extract_exponents %>% 
  rename(b_new = b) %>% 
  left_join(mle_mat %>% unite("group", c(site_id, year_month)) %>% 
            rename(b_old = b)) %>% 
  ggplot(aes(x = mat_site, y = b_new)) + 
  geom_point()
