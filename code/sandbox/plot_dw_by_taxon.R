library(brms)
library(tidyverse)
library(tidybayes)
library(ggview)
library(janitor)
library(ggthemes)

fish_dw = readRDS(file = "data/derived_data/individual_dw_fish.rds") %>% 
  separate(reach_taxon_id, into = c("site_id", "date", "reach", "taxon")) %>% 
  mutate(date = ymd(date)) %>% 
  mutate(animal_type = "fish",
         dw = dw*1000,
         units = "mg")

invert_dw = readRDS(file = "data/derived_data/MN.no.damage.taxa.rds") %>% 
  clean_names() %>% 
  mutate(date = ymd(as.Date(collect_date))) %>% 
  select(order, dw, site_id, date) %>% 
  rename(taxon = order) %>% 
  mutate(animal_type = "inverts",
         units = "mg")

fish_invert_dw = bind_rows(fish_dw, invert_dw) %>% 
  distinct(site_id, taxon, dw, animal_type) %>% 
  filter(!is.na(taxon)) %>% 
  filter(!is.na(dw)) %>% 
  group_by(taxon) %>% 
  mutate(max_dw = max(dw, na.rm = T)) 

plot_size_taxon = fish_invert_dw %>% 
  ggplot(aes(x = dw, y = reorder(taxon, max_dw), color = animal_type)) + 
  geom_point(alpha = 0.3) +
  scale_color_colorblind() +
  theme_default() + 
  theme(axis.text.y = element_blank(),
        legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 2))) +
  labs(y = "Taxon (ranked by dw)",
       x = "mgDM Individual")


saveRDS(plot_size_taxon, file = "plots/plot_size_taxon.rds")
ggview(plot_size_taxon, width = 6, height = 8)
ggsave(plot_size_taxon, file = "plots/plot_size_taxon.jpg", dpi = 500, width = 6, height = 8, units = "in")


fish_invert_dw %>% distinct(taxon, max_dw, animal_type) %>% 
  arrange(-max_dw)



# plot mgm2 rankings


# individuals
fish_invert_dw %>% 
  group_by(site_id, taxon) %>% 
  mutate(max_dw = max(dw)) %>% 
  arrange(site_id, dw) %>%
  group_by(site_id) %>% 
  mutate(order = row_number(),
         prop = order/max(order)) %>%
  ggplot(aes(x = dw, y = prop, color = animal_type)) + 
  geom_point(alpha = 0.3, size = 1, shape = 21) +
  scale_color_colorblind() +
  scale_x_log10() +
  scale_y_log10() +
  theme_default() + 
  theme(axis.text.y = element_blank(),
        legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 2))) +
  facet_wrap(~site_id, scales = "free_y") +
  labs(y = "Relative Size Ranking",
       x = "mgDM Individual")


# individuals mgm2
dat_invert = readRDS(file = "data/derived_data/dat_invert.rds") %>% mutate(animal_type = "inverts")
dat_fish = readRDS(file = "data/derived_data/dat_fish.rds") %>% mutate(animal_type = "fish")

nsamples = 10000

sim_fishinvert = bind_rows(dat_invert, dat_fish) %>% 
  # filter(sample_int == id) %>%
  group_by(sample_int) %>% 
  sample_n(nsamples, weight = no_m2, replace = T) %>% 
  select(dw, site_id, year, sample_int, xmin, xmax, no_m2, animal_type) %>% 
  group_by(sample_int) %>% 
  arrange(desc(dw)) %>% 
  mutate(y_order = 1:nsamples) 


sim_fishinvert %>%
  ggplot(aes(x = dw, y = y_order, color = animal_type)) + 
  geom_point(alpha = 0.3, shape = 21, aes(size = dw)) +
  scale_color_colorblind() +
  scale_x_log10() +
  scale_y_log10() +
  theme_default() + 
  theme(axis.text.y = element_blank(),
        legend.title = element_blank()) + 
  guides(color = guide_legend(override.aes = list(alpha = 1,
                                                  size = 2))) +
  facet_wrap(~site_id, scales = "free_y") +
  labs(y = "Relative Size Ranking",
       x = "mgDM Individual (density corrected)")
