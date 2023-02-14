library(tidyverse)
library(brms)

#This script plots the biomass or length distribution data for macroinverts and fishes to check for
#potential sampling biases (especially against smaller individuals)


# check macroinvertebrate bias -------------------------------------------
# Stoffels, R. J., Karbe, S., & Paterson, R. A. (2003). Length‐mass models for some common New Zealand littoral‐benthic macroinvertebrates, with a note on within‐taxon variability in parameter values among published models. New Zealand Journal of Marine and Freshwater Research, 37(2), 449-460.
# Stoffels et al. (2003) shows regression between body length and head capsule width
# Macroinvertebrates with a length of ~3mm corresponds to a head capsule width of 
# ~0.25mm, which is approximately the mesh width of the aquatic nets used by NEON.
# Therefore, remove sizes less than 3mm
macro <- readRDS("data/raw_data/macro.rds")

all_macros = macro$inv_taxonomyProcessed %>% 
  group_by(sizeClass) %>% 
  count() %>% 
  mutate(data = "all data",
         remove = case_when(sizeClass <3 ~ "yes",
                            TRUE ~ "no"))

culled_macros = all_macros %>% filter(remove == "no") %>% mutate(data = "culled data")

all_and_culled_macros = bind_rows(all_macros, culled_macros)

all_and_culled_macros = all_and_culled_macros %>% 
  ggplot(aes(x = sizeClass, y = n, shape = remove)) + 
  geom_point() + 
  scale_x_log10() + 
  scale_y_log10() + 
  scale_shape_manual(values = c(16, 21)) +
  facet_wrap(~data) +
  theme_default() +
  labs(x = "Macroinvertebrate Length (mm)",
       title = "Macroinvertebrate Length Distributions in NEON data") +
  NULL


ggsave(all_and_culled_macros, file = "plots/all_and_culled_macros.jpg", 
       width = 6, height = 3)


# check fish bias ---------------------------------------------------------

fish <- readRDS("data/raw_data/fish.rds")

# estimate girth, then convert to diameter
# filter out fish that are less than 2 times the mesh diameter (they probably swim through easily)
# Jones, R. E., Petrell, R. J., & Pauly, D. (1999). Using modified length–weight relationships to assess the condition of fish. Aquacultural engineering, 20(4), 261-276.
# Jones et al. estimate that the length/girth ratio ranges from ~1.5 to 2.25
# García, C. M., Jiménez-Gomez, F., Rodríguez, J., Bautista, B., Estrada, M., Garcia Braun, J., ... & Varela, M. (1994). The size structure and functional composition of ultraplankton and nanoplankton at a frontal station in the Alboran Sea. Working groups 2 and 3 report. Scientia Marina (Espana).
# Garcia et al. 1994 excluded data that were less that 2 times the mesh diameter

all_fish = fish$fsh_perFish %>% 
  mutate(girth = fishTotalLength/1.75,
         diameter_mm = girth/pi,
         mesh_mm = 3.175) %>% 
  mutate(data = "all data") %>% 
  group_by(fishTotalLength, diameter_mm, mesh_mm, data) %>% 
  count() %>% 
  mutate(remove = case_when(diameter_mm <= mesh_mm*2 ~ "yes", 
                            TRUE ~ "no"))
  
culled_fish = fish$fsh_perFish %>% 
  mutate(girth = fishTotalLength/1.75,
         diameter_mm = girth/pi,
         mesh_mm = 3.175) %>% 
  mutate(data = "culled data") %>% 
  mutate(remove = case_when(diameter_mm <= mesh_mm*2 ~ "yes", 
                            TRUE ~ "no")) %>% 
  filter(remove == "no") %>% 
  group_by(fishTotalLength, diameter_mm, mesh_mm, data, remove) %>% 
  count() 


# length cutoff
diameter = 3.174*2
girth = diameter*pi
length_cutoff_fish = girth*1.75

all_and_culled = bind_rows(all_fish, culled_fish)


all_and_culled_plot = all_and_culled %>% 
  ggplot(aes(x = fishTotalLength, y = n, shape = remove)) + 
  geom_point() + 
  scale_x_log10() +
  # scale_y_log10() +
  facet_wrap(~data) +
  scale_shape_manual(values = c(16, 21)) +
  geom_rug(sides = "b", alpha = 0.2) +
  theme_default() +
  geom_vline(aes(xintercept = length_cutoff)) +
  labs(x = "Fish Total Length (mm)",
       title = "Fish Length Distributions in NEON data") +
  NULL


ggsave(all_and_culled_plot, file = "plots/all_and_culled.jpg", width = 6, height = 3)


# save cutoffs
length_cutoffs = tibble(animal_type = c("fish", "macroinvertebrate"),
                        length_cutoff = c(length_cutoff, 3))

saveRDS(length_cutoffs, file = "code/fish/length_cutoffs.rds")
