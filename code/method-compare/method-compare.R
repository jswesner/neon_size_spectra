# compare value of lambda across methods

# does binning method(s) give us slopes/lambda estimates closer to theoretical MTE predictions? 

# i.e., do binning methods (Equal Log Bins, log2 bins) produce across trophic level exponent of -1.0 (non-normalized) or -2 (normalized)?

# within trophic levels lambda = -0.75 or -1.75 (non/normalized)

# source custom functions
# compare slopes function
source("code/method-compare/compare-slopes_function.R")
# MLE for tidy data
source("code/method-compare/MLE-tidy_function.R")
# function to bin and center the data
source("code/method-compare/bin-and-center_function.R")

library(tidyverse)
# see https://github.com/andrew-edwards/sizeSpectra for notes on installing sizeSpectra package
library(sizeSpectra)

dat <- readRDS("C:/Users/jfpom/Documents/neon_size_spectra/data/derived_data/dat_all.rds")

# d1 <- dat %>%
#   filter(sample_id == 1)%>% 
#   slice_sample(n = 1e6,
#                weight_by = no_m2,
#                replace = TRUE)
# 
# (d1_range <-  range(d1$dw))
# 
# compare_slopes(d1, d1_range, "dw")
# 
# dtest <- tibble(
#   dw = seq(3.5e-03, 3.05e+4, length.out = 4), 
#                 no_m2 = c(100, 5, 1, 0.1))
# 
# dtest %>% 
#   slice_sample(n = 10,
#                weight_by = no_m2,
#                replace = TRUE)

dat_id <- sort(unique(dat$sample_id))


dat_out <- list()
counter = 1

for (i in dat_id){
  
  d_in <- dat %>%
    filter(sample_id == i)%>% 
    slice_sample(n = 1e6,
                 weight_by = no_m2,
                 replace = TRUE)
  
  d_name <- d_in$sample_id[1]
  
  d_range <-  range(d_in$dw)
  
  result <- compare_slopes(d_in, d_range, "dw")
  row.names(result) <- NULL
  result$sample_id = d_name
  
  dat_out[[counter]] <- result
  
  counter = counter+1
}

bind_rows(dat_out) %>%
  pivot_longer(1:5, names_to = "method") %>%
  group_by(method) %>%
  summarize(
    mean_val = mean(value,
                    na.rm = TRUE),
    median_val = median(value,
                    na.rm = TRUE),
    q05 = quantile(value, probs = 0.05),
    q95 = quantile(value, probs = 0.95))

bind_rows(dat_out) %>%
  pivot_longer(1:5, names_to = "method") %>%
  ggplot(aes(x = value,
             color = method)) +
  geom_density() +
  brms::theme_default()


isd <- read.csv("data/derived_data/post_sample_lambdas.csv")

isd_row <- isd %>% 
  summarize(
    mean_val = mean(.epred),
    median_val = median(.epred,
                        na.rm = TRUE),
    q05 = quantile(.epred, probs = 0.05),
    q95 = quantile(.epred, probs = 0.95))

bind_rows(dat_out) %>%
  pivot_longer(1:5, names_to = "method") %>%
  #filter(method != "mle") %>%
  group_by(method) %>%
  summarize(
    mean_val = mean(value,
                    na.rm = TRUE),
    median_val = median(value,
                        na.rm = TRUE),
    q05 = quantile(value, probs = 0.05),
    q95 = quantile(value, probs = 0.95)) %>%
  bind_rows(isd_row)

bind_rows(dat_out) %>%
  pivot_longer(1:5, names_to = "method") %>%
  group_by(method) %>%
  summarize(range(value))


x125 <- sizeSpectra::rPLB(n=1000, b = -1.25, xmax = 200000, xmin = 0.003)

compare_slopes(as.data.frame(x = x125), dw_range = range(x125), rsp_var = "x125")

bind_rows(dat_out) %>%
  pairs()



# global xmin and xmax ----------------------------------------------------

dat_id <- sort(unique(dat$sample_id))

d_range <-  range(dat$dw)

dat_out_global <- list()
counter = 1


for (i in dat_id){
  
  d_in <- dat %>%
    filter(sample_id == i)%>% 
    slice_sample(n = 1e6,
                 weight_by = no_m2,
                 replace = TRUE)
  
  d_name <- d_in$sample_id[i]
  
  
  result <- compare_slopes(d_in, d_range, "dw")
  row.names(result) <- NULL
  result$sample_id = d_name
  
  dat_out_global[[counter]] <- result
  
  counter = counter+1
}

bind_rows(dat_out_global) %>%
  pivot_longer(1:5, names_to = "method") %>%
  #filter(method != "mle") %>%
  group_by(method) %>%
  summarize(
    mean_val = mean(value,
                    na.rm = TRUE),
    median_val = median(value,
                        na.rm = TRUE),
    q05 = quantile(value, probs = 0.05),
    q95 = quantile(value, probs = 0.95)) %>%
  bind_rows(isd_row)


# mean difference between non/normalized ----------------------------------

bind_rows(dat_out_global) %>%
  mutate(l2 = NAS - AS,
         el = PN - Perkins) %>%
  summarize(mean(l2), mean(el))

# number of bins log2 breaks ----------------------------------------------

breaks2_out <- NULL
counter = 1


for (i in dat_id){
  
  d_in <- dat %>%
    filter(sample_id == i)
  
  dw_range <- range(d_in$dw)
  
  breaks2 <- length(2^(floor(log2(min(dw_range))):
                  ceiling(log2(max(dw_range)))))
  
  breaks2_out[counter] <- breaks2
  
  counter = counter+1
}

bind_rows(bins = breaks2_out)
range(breaks2_out)

plot(bind_rows(bins = breaks2_out))


# bin range for ELBn ------------------------------------------------------
breaks2_out <- NULL
counter = 1


for (i in dat_id){
  
  d_in <- dat %>%
    filter(sample_id == i)
  
  dw_range <- range(d_in$dw)
  
  breaks_log_6 <- exp(seq(floor(log(min(dw_range))),
                          ceiling(log(max(dw_range))),
                          length.out = 7))
  
  breaks2_out[counter] <- breaks2
  
  counter = counter+1
}

