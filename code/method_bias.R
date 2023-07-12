library(tidyverse)
source("code/sandbox/paretocounts.R")
source("code/SI_1_compare_slope_estimates.R")

n_500 = 1:500
n_1000 = 1:1000
n_10000 = 1:10000

sim_settings = tibble(xmin = c(0.001, 0.1, 1)) %>% 
  expand_grid(xmax = c(100, 30000, 100000)) %>% 
  expand_grid(prop_undercount = c(0.1, 0.5, 1)) %>% 
  mutate(value_undercount = prop_undercount*xmax) %>% 
  expand_grid(dw_bias_size = c(0.1, 0.5, 1, 1.5)) %>% 
  expand_grid(lambda = c(-2, -1.2))

dw_500 = sim_settings %>% 
  mutate(n = 500) %>% 
  expand_grid(individual = n_500)

dw_1000 = sim_settings %>% 
  mutate(n = 1000) %>% 
  expand_grid(individual = n_1000)

dw_10000 = sim_settings %>% 
  mutate(n = 10000) %>% 
  expand_grid(individual = n_10000)

dw_bias_dat = bind_rows(dw_500, dw_1000, dw_10000) %>% 
  mutate(dw = rparetocounts(n = nrow(.), mu = lambda, vreal2 = xmin, vreal3 = xmax)) %>% 
  mutate(bias = case_when(dw >= value_undercount ~ dw_bias_size,
                          TRUE ~ 1)) %>% 
  group_by(value_undercount, prop_undercount, dw_bias_size, lambda, xmin, xmax, n) %>% 
  slice_sample(n = 1000, weight_by = bias, replace = T)

dw_bias_split = dw_bias_dat %>%  
  group_split()


slopes_plb = NULL

for(i in 1:length(dw_bias_split)){
slopes_plb[[i]] = tibble(compare_slopes(data = dw_bias_split[[i]], 
                                   dw_range = c(dw_bias_split[[i]]$xmin,
                                                dw_bias_split[[i]]$xmax), rsp_var = "dw")) %>% 
  mutate(value_undercount = unique(dw_bias_split[[i]]$value_undercount),
         prop_undercount = unique(dw_bias_split[[i]]$prop_undercount),
         dw_bias_size = unique(dw_bias_split[[i]]$dw_bias_size),
         xmin = unique(dw_bias_split[[i]]$xmin),
         xmax = unique(dw_bias_split[[i]]$xmax),
         lambda = unique(dw_bias_split[[i]]$lambda))
}

method_comparisons = bind_rows(slopes_plb) %>% 
  mutate(n = dw_bias_dat %>% distinct(value_undercount,
                                      prop_undercount,
                                      dw_bias_size,
                                      xmin, 
                                      xmax,
                                      lambda,
                                      n) %>% 
           pull(n))
  
saveRDS(method_comparisons, file = "data/derived_data/method_comparisons.rds") 

method_comparisons_long = method_comparisons %>% 
  # mutate(bias = mle - NAS) %>%
  mutate(Perkins = Perkins - 1,
         AS = AS - 1) %>% 
  pivot_longer(cols = c(NAS, AS, Perkins, PN, mle)) %>% 
  mutate(bias = -(lambda - value)) 
  

method_comparisons_long %>% 
  ggplot(aes(x = prop_undercount, y = value, color = as.factor(lambda))) + 
  geom_point(position = position_jitterdodge(dodge.width = 0.2),
             jitter.width = 0.1, jitter.height = 0) +
  geom_hline(aes(yintercept = lambda)) +
  # geom_line(aes(group = interaction(name, dw_bias_size))) +
  facet_wrap(~name) +
  NULL


method_comparisons_long %>% 
  ggplot(aes(x = prop_undercount, y = bias, alpha = dw_bias_size)) +
  geom_jitter(width = 0.1) +
  viridis::scale_color_viridis() +
  facet_wrap(~name)



method_comparisons_long %>% 
  ggplot(aes(x = dw_bias_size, y = value, alpha = prop_undercount)) +
  geom_jitter(width = 0.05, height = 0, shape = 21) +
  viridis::scale_color_viridis() +
  facet_grid(n~name)


method_comparisons_long %>% 
  ggplot(aes(x = reorder(name, bias), y = (bias), color = as.factor(lambda))) +
  geom_jitter(width = 0.05, height = 0, shape = 21) +
  facet_wrap(~lambda)

method_comparisons_long %>% 
  mutate(range = xmax - xmin,
         range_order = round(log10(range), 1)) %>% 
  ggplot(aes(x = range_order, y = bias, color = as.factor(lambda))) +
  geom_jitter(width = 0.05, height = 0, shape = 21) +
  facet_grid(name~lambda) +
  scale_x_log10()




# check rescaling ---------------------------------------------------------

check_range = tibble(dw = rparetocounts(n = 1000, mu = -1.5, 
                     vreal2 = 0.01, vreal3 = 10000)) %>% 
  mutate(no_m2 = 1,
         dw_s = dw/mean(dw),
         xmin = 0.01, 
         xmax = 10000,
         xmin_s = xmin/mean(dw),
         xmax_s = xmax/mean(dw))

library(brms)
brm_s = brm(dw_s | vreal(no_m2, xmin, xmax) ~ 1, 
    data = check_range,
    stanvars = stanvars,
    family = paretocounts(),
    chains = 3, iter = 2000,
    prior = c(prior(normal(-1.3, 0.5), class = "Intercept")))

brm_s

check_range = tibble(dw = rparetocounts(n = 1000, mu = -1.5, 
                                        vreal2 = 0.01, vreal3 = 10000)) %>% 
  mutate(no_m2 = 1,
         dw_s = dw/mean(dw),
         xmin = 0.01, 
         xmax = 10000,
         xmin_s = xmin/mean(dw),
         xmax_s = xmax/mean(dw))

check = update(brm_s, newdata = check_range, 
               formula = dw_s | vreal(no_m2, xmin_s, xmax_s) ~ 1,
               chains = 1)

check
