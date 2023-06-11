library(tidyverse)
library(maps)
library(usmap)
library(ggrepel)
library(lubridate)
library(janitor)
library(viridis)
library(rnaturalearthdata)
library(rnaturalearth)

neon_latlong <- read_csv(file = "data/raw_data/site_lat_longs.csv") %>% distinct(siteID, lat, long) %>% 
  clean_names()

temp_gpp_om = readRDS("data/derived_data/dat_all.rds") %>% ungroup %>% distinct(site_id, temp_mean,
                                                                                gpp, mean_om) %>% 
  left_join(neon_latlong) %>% 
  pivot_longer(cols = c(gpp, mean_om, temp_mean))

world <- map_data("world") %>% expand_grid(name = temp_gpp_om %>% distinct(name))
states <- map_data("state") %>% expand_grid(name = temp_gpp_om %>% distinct(name))
usa <- ne_countries(scale='medium',returnclass = 'sf') 


(map_temp <- usa %>% 
    filter(sovereignt == "United States of America") %>% 
    ggplot() + 
    # coord_sf() + 
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey70") +
    geom_sf(color = "white", fill = "grey70") +
    # geom_label_repel(data = temp_gpp_om, aes(label = site_id, x = long, y = lat), size = 2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "white", fill = "grey70")  +
    geom_point(data = temp_gpp_om %>% filter(name == "temp_mean"), 
               aes(x = long, y = lat, fill = value),
               size = 2,
               alpha = 0.9,
               color = "black", shape = 21,
               position = position_jitter(width = 2, height = 1, seed = 2323)) +
    # geom_label_repel(data = temp_gpp_om  %>% filter(name == "temp_mean"),
    #                  aes(x = long, y = lat, fill = value),
    #                  label = ".",
    #                  color = NA,
    #                  segment.color = "black",
    #                  label.r = 0.5) +
    theme_void() +
    coord_sf(ylim = c(10, 68), xlim = c(-160, -68)) +
    labs(fill = "\u00b0C",
         subtitle = "a) Temperature") +
    scale_fill_viridis() + 
    theme(legend.position = c(0.25, 0.4),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    NULL)


(map_gpp <- usa %>% 
    filter(sovereignt == "United States of America") %>% 
    ggplot() + 
    # ylim(c(10,65))+
    # xlim(c(-160,-50)) +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey70") +
    geom_sf(color = "white", fill = "grey70") +
    # geom_label_repel(data = temp_gpp_om, aes(label = site_id, x = long, y = lat), size = 2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "white", fill = "grey70")  +
    geom_point(data = temp_gpp_om %>% filter(name == "gpp"),
               aes(x = long, y = lat, fill = value),
               size = 2,
               color = "black", shape = 21,
               position = position_jitter(width = 2, height = 1, seed = 2323)) +
    # geom_label_repel(data = temp_gpp_om  %>% filter(name == "gpp"),
    #                  aes(x = long, y = lat, fill = value),
    #                  label = ".",
    #                  color = NA,
    #                  segment.color = "black",
    #                  label.r = 0.5) +
  # facet_wrap(~name) +
  theme_void() +
    coord_sf(ylim = c(10, 68), xlim = c(-160, -68)) +
  labs(fill = expression(paste("",gC/m ^ 2/yr,"")),
       subtitle = "b) GPP") +
  scale_fill_viridis(trans = "log", breaks = c(300, 1000, 3000, 10000)) + 
  theme(legend.position = c(0.25, 0.4),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8)) + 
  NULL)


(map_om <- usa %>% 
    filter(sovereignt == "United States of America") %>% 
    ggplot() + 
    # coord_sf() + 
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey70") +
    geom_sf(color = "white", fill = "grey70") +
    # geom_label_repel(data = temp_gpp_om, aes(label = site_id, x = long, y = lat), size = 2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "white", fill = "grey70")  +
    geom_point(data = temp_gpp_om %>% filter(name == "mean_om"), 
               aes(x = long, y = lat, fill = value),
               size = 2,
               color = "black", shape = 21,
               position = position_jitter(width = 2, height = 1, seed = 2323)) +
    # geom_label_repel(data = temp_gpp_om  %>% filter(name == "mean_om"),
    #                  aes(x = long, y = lat, fill = value),
    #                  label = ".",
    #                  color = NA,
    #                  segment.color = "black",
    #                  label.r = 0.5) +
    # facet_wrap(~name) +
    theme_void() +
    coord_sf(ylim = c(10, 68), xlim = c(-160, -68)) +
    labs(fill = expression(paste("",gAFDM/m ^ 2,"")),
         subtitle = "c) Organic Matter") + 
    theme(legend.position = c(0.25, 0.4),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    scale_fill_viridis(trans = "log") +
    NULL)

(map_empty <- usa %>% 
    filter(sovereignt == "United States of America") %>% 
    ggplot() + 
    # ylim(c(10,65))+
    # xlim(c(-160,-50)) +
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "white") +
    geom_sf(color = "white", fill = "white") +
    # geom_label_repel(data = temp_gpp_om, aes(label = site_id, x = long, y = lat), size = 2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "white", fill = "white")  +
    # geom_point(data = temp_gpp_om %>% filter(name == "gpp"),
    #            aes(x = long, y = lat, fill = value),
    #            size = 2.5,
    #            color = "white", shape = 21,
    #            position = position_jitter(width = 2, height = 2)) +
    # geom_label_repel(data = temp_gpp_om  %>% filter(name == "gpp"),
    #                  aes(x = long, y = lat, fill = value),
    #                  label = ".",
    #                  color = NA,
    #                  segment.color = "black",
    #                  label.r = 0.5) +
    # facet_wrap(~name) +
    theme_void() +
    coord_sf(ylim = c(10, 68), xlim = c(-160, -68)) +
    # labs(fill = expression(paste("",gC/m ^ 2/yr,"")),
    #      subtitle = "GPP") +
    scale_fill_viridis(trans = "log", breaks = c(300, 1000, 3000, 10000)) + 
    theme(legend.position = c(0.25, 0.4),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) + 
    NULL)


library(patchwork)
map = (map_temp + map_gpp)/
  (map_om + map_empty)

saveRDS(map, file = "plots/ms_plots/map.rds")
ggview::ggview(map, width = 6.5, height = 6.5, units = "in")
ggsave(map, width = 6.5, height = 6.5, units = "in", dpi = 500,
       file = "plots/ms_plots/map.jpg")



map_sites <- usa %>% 
    filter(sovereignt == "United States of America") %>% 
    ggplot() + 
    geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "grey70") + 
    geom_sf(color = "white", fill = "grey70") +
    # geom_label_repel(data = temp_gpp_om, aes(label = site_id, x = long, y = lat), size = 2) +
    geom_polygon(data = states, aes(x = long, y = lat, group = group), color = "white", fill = "grey70")  +
    # geom_point(data = temp_gpp_om %>% filter(name == "temp_mean"), 
    #            aes(x = long, y = lat, fill = value),
    #            size = 2,
    #            alpha = 0.9,
    #            color = "black", shape = 21,
    #            position = position_jitter(width = 2, height = 1, seed = 2323)) +
    geom_label_repel(data = temp_gpp_om  %>% filter(name == "temp_mean"),
                     aes(x = long, y = lat,label = site_id),
                     segment.color = "black",
                     size = 2)  +
  # geom_text_repel(data = temp_gpp_om  %>% filter(name == "temp_mean"),
  #                  aes(x = long, y = lat, fill = value, label = site_id), 
  #                  size = 4)  +
    theme_void() +
    coord_sf(ylim = c(10, 68), xlim = c(-160, -68)) +
    labs(fill = "\u00b0C") +
    scale_fill_viridis() +
    theme(legend.position = c(0.25, 0.4),
          legend.key.size = unit(0.4, "cm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8)) +
    NULL

map_sites
ggsave(map_sites, file = "plots/map_sites.jpg", width = 5, height = 5, dpi = 500, units = "in")


abiotic_quantiles = temp_gpp_om %>% 
  group_by(name) %>% 
  reframe(min = min(value),
          q25 = quantile(value, probs = 0.25),
            q5 = quantile(value, probs = 0.5),
            q75 = quantile(value, probs = 0.75),
          max = max(value)) %>% 
  mutate_if(is.numeric, round, 0) %>% 
  mutate(metric = case_when(name == "gpp" ~ "g/C/m2/yr",
                            name == "mean_om" ~ "gAFDM/m2",
                            TRUE ~ "degrees C"))
saveRDS(abiotic_quantiles, file = "tables/abiotic_quantiles.rds")
write_csv(abiotic_quantiles, file = "tables/abiotic.csv")
          