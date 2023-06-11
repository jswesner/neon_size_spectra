library(tidyverse)
library(packcircles)
library(viridis)
source("code/sandbox/paretocounts.R")

# make pareto sizes
n = 8000
dw <- sort(rparetocounts(n = n, mu = -1.75))

# dot plots ---------------------------------------------------------------

# make uniform sizes
dw_flat = rep(1, n)
packing2 <- circleProgressiveLayout( rev(dw_flat) ) 
dat2 <- circleLayoutVertices(packing2)

(uniform_circle1 = 
    ggplot(data = dat2, aes(x, y)) + 
    geom_polygon(aes(group = id),
                 alpha = 0.9,
                 fill = "#e34a33",
                 colour = "black", show.legend = FALSE) +
    # scale_fill_distiller(palette = "YlOrRd") +
    # scale_fill_brewer(type = "", palette = 3) +
    # scale_alpha_continuous() +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
)


(uniform_circle = 
  ggplot(data = dat2, aes(x, y)) + 
    geom_polygon(aes(group = id, fill = -id),
                 alpha = 0.9,
                 colour = "black", show.legend = FALSE) +
    # scale_fill_distiller(palette = "YlOrRd") +
    # scale_fill_brewer(type = "", palette = 3) +
    # scale_alpha_continuous() +
    viridis::scale_fill_viridis() +
    coord_equal() +
    theme_void()
)

# make pareto sizes
dw <- sort(rparetocounts(n = n, mu = -1.75))

packing1 <- circleProgressiveLayout( rev(dw) ) 
dat1 <- circleLayoutVertices(packing1)

(pareto_circle = 
  ggplot(data = dat1, aes(x, y)) + 
    geom_polygon(aes(group = id, fill = -id),
                 alpha = 0.9,
                 colour = "black", show.legend = FALSE) +
  # scale_fill_distiller(palette = "YlOrRd") +
    # scale_fill_brewer(type = "", palette = 3) +
    # scale_alpha_continuous() +
  viridis::scale_fill_viridis() +
  coord_equal() +
  theme_void()
)

ggsave(uniform_circle, file = "plots/uniform_circle.jpg", width = 5, height = 5, units = "in", dpi = 400)
ggsave(pareto_circle, file = "plots/pareto_circle.jpg", width = 5, height = 5, units = "in", dpi = 400)



# food web plot -----------------------------------------------------------
trophic_dots = tibble(dw = dw) %>% 
  mutate(x_bins = cut( dw, breaks = c(1, 400, 800, 950, 1000))) %>% 
  group_by(x_bins) %>% 
  add_tally() %>% 
  separate(x_bins, into = c("lower", "upper"), sep = ",", remove = F) %>% 
  mutate(lower = parse_number(lower),
         upper = parse_number(upper)) %>% 
  ungroup %>% 
  mutate(ndot = runif(nrow(.), lower, upper)) %>% 
  group_by(x_bins) %>% 
  mutate(mindot = min(ndot),
         ndot = ndot - mindot,
         mass = sum(dw),
         ndot = ndot*mass,
         nbar = max(ndot)) %>% 
  ungroup %>% 
  mutate(trophic = case_when(upper <= 400 ~ "1",
                             upper <= 800 ~ "2",
                             upper <= 990 ~ "3",
                             TRUE ~ "4"))

trophic_bars = trophic_dots %>% 
  ungroup %>% 
  distinct(x_bins, nbar, trophic)

set.seed(20202)
trophic_plot = trophic_dots %>% 
  ggplot(aes(x = nbar, y = trophic)) + 
  geom_bar(data = trophic_bars, stat = "identity") +
  geom_jitter(aes(x = ndot,y = trophic, size = dw, fill = dw),
              shape = 21, width = 0, height = 0.4) +
  scale_size_continuous(range = c(0, 5)) +
  scale_fill_viridis() +
  theme_void() +
  labs(x = "Mass") +
  guides(fill = "none",
         size = "none") + 
  theme(axis.text.y = element_text(),
        axis.title.x = element_text())
  
ggsave(trophic_plot, file = "plots/trophic_plot.jpg", width = 4, height = 4, dpi = 300)

# isd plot ----------------------------------------------------------------
library(viridis)
source("code/sandbox/paretocounts.R")

n = 8000
xmin = 1
xmax = 1000

isd_plot_data = tibble(lambda = c(-2, -1.75, -1.5, -1.2),
                       .lower = c(-2.05,-1.8, -1.55, -1.25),
                       .upper = c(-1.95,-1.7, -1.45, -1.15)) %>% 
  expand_grid(individual = 1:n) %>%
  mutate(xmin = xmin, 
         xmax = xmax,
         dw = rparetocounts(n = nrow(.), mu = lambda, vreal2 = 1, vreal3 = 1000)) %>% 
  arrange(lambda, dw) %>% 
  group_by(lambda) %>% 
  mutate(rownumber = row_number(),
         y_order = 1:max(rownumber),
         y_order = seq(1, 0, length.out = max(rownumber)))

rows_to_slice = as.integer(seq(1,n, length.out = 4000))

isd_lines = isd_plot_data %>% 
  slice(rows_to_slice) %>% 
  mutate(y_order = (1 - (dw^(lambda + 1) - (xmin^(lambda+1)))/(xmax^(lambda + 1) - (xmin^(lambda+1)))),
         ymin.PLB = (1 - (dw^(.lower + 1) - (xmin^(.lower+1)))/(xmax^(.lower + 1) - (xmin^(.lower+1)))),
         ymax.PLB = (1 - (dw^(.upper + 1) - (xmin^(.upper+1)))/(xmax^(.upper + 1) - (xmin^(.upper+1)))))

isd_plot_data %>% 
  # sample_n(size = 100) %>% 
  ggplot(aes(x = dw, y = y_order, group = lambda)) + 
  geom_point(aes(size = dw, fill = dw)) +
  scale_x_log10() + 
  scale_y_log10() +
  scale_fill_viridis() +
  coord_cartesian(ylim = c(0.0001, 1)) +
  geom_line(data = isd_lines) + 
  geom_ribbon(data = isd_lines, aes(ymin = ymin.PLB, ymax = ymax.PLB), alpha = 0.5)

set.seed(20202)

isd_maxmin = isd_plot_data %>% 
  filter(lambda == -1.75) %>% 
  filter(dw == min(dw) | dw == max(dw)) %>% 
  ggplot(aes(x = dw, y = y_order)) +
  geom_point(shape = 21, aes(fill = dw, size = dw)) +
  scale_size_continuous(range = c(0.1, 10)) +
  # scale_y_log10() +
  scale_x_log10() +
  coord_cartesian(ylim = c(0.0001, 1)) +
  viridis::scale_fill_viridis() +
  # geom_line(data = post_lines) +
  # geom_ribbon(data = post_lines,
  #             aes(ymin = ymin + 0.00, ymax = ymax + 0.00), alpha = 0.5) +
  # theme_void() +
  guides(fill = "none",
         size = "none") +
  labs(y = "Proportion of individuals \u2265 dw",
       x = "Mass of individual (dw)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))


isd_noline = isd_plot_data %>%
  sample_n(1000) %>%
  filter(lambda == -1.75) %>% 
  bind_rows(isd_plot_data %>% filter(dw == min(dw) | dw == max(dw))) %>% 
  ggplot(aes(x = dw, y = y_order)) +
  geom_point(shape = 21, aes(fill = dw, size = dw)) +
  scale_size_continuous(range = c(0.1, 10)) +
  # scale_y_log10() +
  scale_x_log10() +
  viridis::scale_fill_viridis() +
  # geom_line(data = post_lines) +
  # geom_ribbon(data = post_lines,
  #             aes(ymin = ymin + 0.00, ymax = ymax + 0.00), alpha = 0.5) +
  theme_void() +
  guides(fill = "none",
         size = "none") +
  labs(y = "Proportion of individuals \u2265 dw",
       x = "Mass of individual (dw)") 


isd_withline = isd_plot_data %>% 
  filter(lambda == -1.75) %>% 
  # filter(dw == min(dw) | dw == max(dw)) %>% 
  ggplot(aes(x = dw, y = y_order)) +
  geom_point(shape = 21) +
  scale_size_continuous(range = c(0.1, 10)) +
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian(ylim = c(0.0001, 1)) +
  viridis::scale_fill_viridis() +
  geom_line(data = isd_lines %>% 
              filter(lambda == -1.75) ) +
  geom_ribbon(data = isd_lines %>% 
                filter(lambda == -1.75) ,
              aes(ymin = ymin.PLB, ymax = ymax.PLB), alpha = 0.5) +
  # theme_void() +
  guides(fill = "none",
         size = "none") +
  labs(y = "Proportion of individuals \u2265 dw",
       x = "Mass of individual (dw)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))


isd_onlyline = isd_plot_data %>% 
  filter(lambda == -1.75) %>% 
  # filter(dw == min(dw) | dw == max(dw)) %>% 
  ggplot(aes(x = dw, y = y_order)) +
  # geom_point(shape = 21) +
  scale_size_continuous(range = c(0.1, 10)) +
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian(ylim = c(0.0001, 1)) +
  viridis::scale_fill_viridis() +
  geom_line(data = isd_lines %>% 
              filter(lambda == -1.75) ) +
  geom_ribbon(data = isd_lines %>% 
                filter(lambda == -1.75) ,
              aes(ymin = ymin.PLB, ymax = ymax.PLB), alpha = 0.5) +
  # theme_void() +
  guides(fill = "none",
         size = "none") +
  labs(y = "Proportion of individuals \u2265 dw",
       x = "Mass of individual (dw)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))


isd_onlyline_groups = isd_plot_data %>% 
  # filter(lambda == -1.75) %>% 
  # filter(dw == min(dw) | dw == max(dw)) %>% 
  ggplot(aes(x = dw, y = y_order, group = lambda)) +
  # geom_point(shape = 21) +
  # scale_size_continuous(range = c(0.1, 10)) +
  scale_y_log10() +
  scale_x_log10() +
  coord_cartesian(ylim = c(0.0001, 1)) +
  viridis::scale_fill_viridis() +
  geom_line(data = isd_lines ) +
  geom_ribbon(data = isd_lines ,
              aes(ymin = ymin.PLB, ymax = ymax.PLB), alpha = 0.5) +
  # theme_void() +
  guides(fill = "none",
         size = "none") +
  labs(y = "Proportion of individuals \u2265 dw",
       x = "Mass of individual (dw)") +
  theme_minimal() +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 18))

ggsave(isd_maxmin, file = "plots/isd_maxmin.jpg", width = 5, height = 5, dpi = 300)
ggsave(isd_noline, file = "plots/isd_noline.jpg", width = 5, height = 5, dpi = 300)
ggsave(isd_withline, file = "plots/isd_withline.jpg", width = 5, height = 5, dpi = 300)
ggsave(isd_onlyline, file = "plots/isd_onlyline.jpg", width = 5, height = 5, dpi = 300)
ggsave(isd_onlyline_groups, file = "plots/isd_onlyline_groups.jpg", width = 5, height = 5, dpi = 300)



