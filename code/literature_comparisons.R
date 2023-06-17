library(tidyverse)
library(brms)
# Literature Figure Comparisons -------------------------------------------

# literature comparison

lit <- read_csv("data/temp_summaries_table.csv") %>% 
        filter(Include == "Y") %>% 
  mutate(size_magnitude = round(log10(xmax) - log10(xmin),0),
         size_scale = size_magnitude*scale_km)

mod_best <- readRDS("models/fit_pareto.rds")
mod_summary = summary(mod_best)
conds = conditional_effects(fit_pareto)
conds$mat_s %>% as_tibble() %>% 
  filter(mat_s == min(mat_s) | mat_s == max(mat_s)) %>% 
  select(upper__)


lit %>% 
  # mutate(Driver = fct_relevel(Driver, "Temperature", "Land Use")) %>% 
  group_by(Driver) %>% 
  mutate(median = median(direction)) %>% 
  ungroup %>% 
  ggplot(aes(x = direction, y = reorder(Driver, median))) +
  # geom_segment(aes(y= 0, yend = b_diff, xend = reorder(Author, -b_diff))) +
  geom_point(aes(color = organisms, size = scale_km))



lit %>% 
  filter(!is.na(scale_km)) %>% 
  ggplot(aes(x = scale_km, y = reorder(Author, scale_km), size = size_magnitude, color = organisms)) + 
  geom_point()


lit %>% 
  filter(!is.na(size_magnitude)) %>% 
  ggplot(aes(x = size_magnitude, y = reorder(Author, size_magnitude))) + 
  geom_point()


lit_plot <- lit %>% 
                mutate(Driver = fct_relevel(Driver, "Temperature", "Land Use")) %>% 
                ggplot(aes(x = reorder(Author, -b_diff), y = b_diff)) +
                coord_flip() +
                # geom_segment(aes(y= 0, yend = b_diff, xend = reorder(Author, -b_diff))) +
                geom_pointrange(aes(ymin =b_diff - error, ymax = b_diff + error, shape = Driver, 
                                    fill = Driver), size = 1) +
                geom_hline(yintercept = range_bmat_summary$mean) +
                annotate("text", x = 11.6, y = 0.65, label = "This study (median and 95% CrI)",
                         size = 3) +
                geom_rect(aes(xmin = 0, xmax = 13, ymin = range_bmat_summary$lower, 
                              ymax = range_bmat_summary$upper), color = NA, alpha = 0.01) +
                # scale_fill_brewer(type = "qual") +
                # facet_grid(Driver ~ .) +
                # scale_shape_manual(values = c(21, 22, 23, 24)) +
                # scale_fill_manual(values = c("black", "white", "white", "white")) +
                labs(y = "Absolute change in ISD exponent (or slope)",
                     shape = "",
                     fill = "") +
                theme_classic() +
                theme(axis.title.y = element_blank(),
                      text = element_text(size = 10),
                      axis.text.y = element_text(size = 8),
                      legend.position = "top") +
                annotate("segment", x = 11.6, y = 0.43, xend = 11.6, 
                         yend = 0.24, arrow=arrow(type = "closed")) +
                ylim(0,1) 


ggsave(lit_plot, file = "plots/lit_plot.tiff", width = 7, height = 3.5, units = "in", dpi = 500)




