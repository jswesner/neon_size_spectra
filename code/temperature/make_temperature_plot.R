library(tidybayes)
library(tidyverse)
library(lubridate)
library(ggridges)
library(viridis)
library(brms)
library(maps)
library(usmap)
library(ggrepel)

# load data
test_temp_data <- readRDS(file = "code/temperature/temp_data.rds")

# load posteriors
water_posteriors <- readRDS(file = ("code/temperature/posteriors/water_posteriors.rds")) # full posterior - all sites
summary_posts <- readRDS(file = ("code/temperature/posteriors/summary_posts.rds"))
mat_posts <- readRDS(file = "code/temperature/posteriors/mat_posts.rds")
mat_distribution <- water_posteriors %>%
  group_by(.draw, siteID) %>% 
  summarize(mat_draw = mean(water))

# plot mat
mat_distribution %>% 
  left_join(mat_posts) %>% 
  ggplot(aes(x = mat_draw, y = reorder(siteID, -mat_site))) + 
  geom_density_ridges(aes(fill = mat_site)) + 
  scale_fill_viridis() + 
  theme_default() + 
  labs(y = "NEON Site",
       x = expression("Mean Annual Water Temp " ( degree*C))) + 
  guides(fill = "none")



# make a map --------------------------------------------------------------

neon_latlong <- read_csv(file = "data/site_latlong.csv") %>% 
  rename(siteID = site) %>% 
  left_join(mat_posts)

world <- map_data("world")
states <- map_data("state")

map <- ggplot() + 
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey50") + 
  geom_polygon(data = states, aes(x = long, y = lat, group =group), color = "white", fill = "grey50")+
  coord_quickmap() +
  geom_label_repel(data = neon_latlong, aes(label = siteID, x = long, y = lat), size = 5) +
  geom_point(data = neon_latlong, aes(x = long, y = lat, fill = mat_site), size = 7,
             color = "black", shape = 21)+
  ylim(c(10,75))+
  xlim(c(-180,-50)) +
  theme_void() +
  labs(fill = "Mean Annual\nTemp (deg C)") +
  scale_fill_viridis() + 
  theme(text = element_text(size = 24),
        legend.position = c(0.25,0.45))


ggsave(map, file = "plots/map.jpg", dpi = 600, width = 14, height = 14)
