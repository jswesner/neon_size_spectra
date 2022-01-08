# mle fit to fish + macro data

library(sizeSpectra)
library(tidyverse)

# load fish_dw data
dat <- readRDS("data/derived_data/macro_fish_dw.rds")
# load mean annual air temp data 
# this is for plots below
mat_c <- read_csv("data/raw_data/mat_c.csv")
# trim temp data to 2 columns
mat_c <- mat_c %>%
  select(Site, mat.c)

# check names of fish-dw data
names(dat)

# add "ID" column
# this is for giving each "collection" a unique identifier
dat <- dat %>%
  ungroup() %>%
  group_by(site_id, year_month) %>%
  mutate(ID = cur_group_id())

### ~~~~~~~~ Jeff - Please read this ~~~~~~~ ###
### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# Double check that this is right. some of the c(month, fish_month, year_month) combos don't seem right. This may be an issue in code/fish/3_combine_fish_macros.R ??
# currently (1/8/22), there are 172 groups
# But I think should be:
# 24 sites * 2 fish collections * 2 years = 96 collections?
# plus maybe a few extras when there were 3 fish samples in a year??
names(dat)


# rename columns to be ocmpatible with sizeSpectra::eight.methods.count()
### NOTE:
# eight.methods.count() automatically produces a plot for each "collection". These will be found in the home directory of the R project. I recommend ***NOT*** pushing those figures to the REPO. 
dat <- dat %>%
  rename(Year = ID,
         Number = no_m2,
         bodyMass = dw)

# for loop using count method in sizeSpectra
# make vector of unique collection identifiers
# "ID" column was renamed "Year" to match sizeSpectra package
fullid <- unique(dat$Year)
# make empty data frame for slope estimates
fullResults <- data.frame()
# for loop - might take ~ 30-60 minutes
for(ii in fullid){
  eightMethodsRes = 
    eightMethods.count(data = dat,
                       oneYear = ii,
                       figName = "jan8_full_run")
  fullResults = rbind(fullResults, eightMethodsRes)}

# clean object for export
# This is result to be added to SFS JASM 2022 talk, Wesner et al. 
mle_output <- fullResults %>%
  tibble() %>%
  select(-dat) %>%
  filter(Method == "MLE") %>%
  rename(ID = Year) %>%
  left_join(dat %>%
              ungroup() %>%
              select(site_id,
                     year_month,
                     Year) %>%
              distinct(),
            by = c("ID" = "Year"))

# save output to be pushed to REPO for Jeff's talk
# made a new "results" folder
# maybe this should be saved to models??
saveRDS(mle_output, 
        "results/mle_b_estimate_fish_macro.RDS")



# exploration -------------------------------------------------------------

# macroinvertebrate est. b ####
# estimate slopes only looking at the macroinvertebrate data

# make empty data frame
macroResults <- data.frame()
# subset data to only macros
macro_dat <- dat %>% 
  filter(animal_type == "macroinvertebrates")

# for loop as before, but different data input
# note different figName argument
for(ii in fullid){
  eightMethodsRes = 
    eightMethods.count(data = macro_dat,
                       oneYear = ii,
                       figName = "jan8_macro_run")
  macroResults = 
    rbind(macroResults, eightMethodsRes)}

# reality check
# can compare "...fullrun" and "...macro_run" plots to ensure that fish are included. Check bodymass range in plots, matched by "Year" number. i.e., 1, 23, 100, 152, .... etc. 

# add "dat" column to both runs to distinguish slope estimates
fullResults$dat <- "both"
macroResults$dat <- "macros"

# combine both slope estimates
both_runs <- bind_rows(fullResults, macroResults)

# filter to only look at the MLE estimate
both_runs <- both_runs %>%
  filter(Method == "MLE")

# add site_id and "ID" = "Year" to slope estimates
# this is for joining temp data below
both_runs <- both_runs %>%
  left_join(y = dat %>%
              ungroup() %>% 
              select(site_id, Year) %>%
              unique())

# add temperature data based on "site_id" == "Site" columns
both_runs <- both_runs %>%
  left_join(y = mat_c, 
            by = c("site_id" = "Site"))

# make some plots
# compare effect of temperature on slope estimates using macros+fish and macros only
both_runs %>%
  ggplot(aes(y = b,
             ymin = confMin,
             ymax = confMax,
             x = mat.c)) +
  geom_pointrange() +
  labs(y = "b estimate",
       x = "mean annual air temp") +
  stat_smooth(method = "lm") +
  facet_wrap(~dat) +
  theme_bw() +
  NULL

# plot both estimates on same figure
both_runs %>%
  ggplot(aes(y = b,
             ymin = confMin,
             ymax = confMax,
             x = mat.c,
             color = dat, 
             shape = dat)) +
  geom_pointrange(alpha = 0.5 #, position = position_jitter(width = 0.5, height = 0.0)
                  ) +
  theme_bw() +
  labs(y = "b estimate",
       x = "mean annual air temp") +
  NULL

# add regression line to same figure
both_runs %>%
  ggplot(aes(y = b,
             ymin = confMin,
             ymax = confMax,
             x = mat.c,
             color = dat, 
             shape = dat)) +
  geom_pointrange(alpha = 0.5 #, position = position_jitter(width = 0.5, height = 0.0)
  ) +
  stat_smooth(method = "lm",
              se = FALSE) +
  theme_bw() +
  labs(y = "b estimate",
       x = "mean annual air temp") +
  NULL

# print out lm() summaries for effect of temperature on different data inputs
summary(lm(b ~ mat.c, data = both_runs %>%
             filter(dat == "both")))
# full data: mat.c coef ~ -0.002
summary(lm(b ~ mat.c, data = both_runs %>%
             filter(dat == "macros")))
# macros only: mat.c coef ~ -0.003

both_runs %>%
  group_by(dat) %>%
  summarize(median_b = median(b),
            median_se = median(stdErr),
            median_CImin = median(confMin),
            median_CImax = median(confMax))

