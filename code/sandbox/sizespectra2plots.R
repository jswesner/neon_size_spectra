library(isdbayes)
library(tidyverse)
library(tidybayes)
library(ggthemes)

x1 = rparetocounts(mu=-1.2, n = 1000) %>% as_tibble() %>% 
  mutate(group = "x1")

x2 = rparetocounts(mu=-1.6, n = 1000) %>% as_tibble() %>% 
  mutate(group = "x2")

x3 = rparetocounts(mu=-2, n = 2000) %>% as_tibble() %>% 
  add_row(value = 1) %>% 
  add_row(value = 1000)%>% 
  mutate(group = "x3")


dat = bind_rows(x1, x2, x3) %>% 
  mutate(xmin = 1, xmax = 1000, counts = 1) %>% 
  rename(x = value)

fit2 <- readRDS("C:/Users/Jeff.Wesner/OneDrive - The University of South Dakota/USD/Github Projects/isdbayes/models/fit2.rds")

fit2a = update(fit2, newdata = dat)


fit2_posts = fit2a$data %>% 
  distinct(group) %>% # optional groups
  mutate(counts = 1, # placeholder
         xmin = 1, # placeholder
         xmax = 1000) %>% # placeholder
  tidybayes::add_epred_draws(fit2a)

# get posterior of prob X>=x
prob_yx_posts = get_isd_posts(fit2_posts)

isd_data = fit2a$data %>%
  group_by(group) %>% 
  get_isd_data(resp = x) 

# plot
labels = prob_yx_posts %>% filter(x.PLB <= 300 & x.PLB >= 298) %>% 
  filter(.draw == 1) %>% 
  mutate(label = case_when(group == "x1" ~ "\u03bb = -1.2",
                           group == "x2" ~ "\u03bb = -1.6",
                           TRUE ~ "\u03bb = -2"))

library(scales)
example_isd = prob_yx_posts %>% 
  group_by(x.PLB, group) %>% 
  reframe(prob_yx = median(prob_yx)) %>% 
  ggplot(aes(x = x.PLB, y = prob_yx)) +
  geom_line(aes(color = group)) +
  geom_point(data = isd_data, aes(x = x, size = x, color = group), shape = 21) +
  # facet_grid(~group) +
  scale_x_log10() +
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100, 1000),
                labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100", "1000")) + 
  scale_size_continuous(breaks = c(1, 10, 100, 1000)) +
  geom_label(data = labels, aes(label = label)) +
  brms::theme_default() + 
  scale_color_colorblind() + 
  labs(x = "Body Size (mg)",
       y = "Proportion \u2265 x",
       size = "Body Size (mg)",
       caption = "Fig. 1. ISD's with \u03bb's ranging from -1.2 to -2. Smaller values of \u03bb (e.g. -2)\n indicate less efficient trophic transfer, supporting relatively fewer large\norgansisms than ecosystems with larger \u03bb values (e.g., -1.2, -1.6).\nCircle sizes represent sizes of individuals in an ecosystem.") +
  guides(color = "none") +
  theme(legend.position = c(0.25, 0.3))


ggview::ggview(example_isd, width = 5, height = 5, units = "in")
ggsave(example_isd, width = 5, height = 5, units = "in", 
       file = "plots/example_isd.jpg", dpi = 500)
saveRDS(example_isd, file = "plots/example_isd.rds")




# example analysis --------------------------------------------------------

reg_sim = tibble(x = seq(-2, 2, length.out = 10)) %>% 
  mutate(int = 0) %>%
  expand_grid(group = c("TTE", "PPMR", "MS")) %>% 
  mutate(slope = case_when(group == "TTE" ~ -1,
                           group == "PPMR" ~ 1,
                           TRUE ~ 1)) %>%
  mutate(resid = rnorm(nrow(.), 0, 0.3),
         y = int + slope*x + resid)

tte_plot = reg_sim %>% 
  filter(group == "TTE") %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_smooth(method = lm, color = "black",
              linewidth = 0.2, fill = "#c7eae5") +
  brms::theme_default() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 6)) +
  labs(x = "Production",
       y = expression(paste("δ", "15N (\u2030)")),
       subtitle = "a) TTE") +
  geom_segment(aes(x = -0, y = -0.2, xend = -0.6, yend = -0.2),
               linetype = "dashed", linewidth = 0.25) +
  geom_segment(aes(x = -0.6, y = 0.45, xend = -0.6, yend = -0.2),
               linetype = "dashed", linewidth = 0.25) +
  annotate(geom = "text", x = -0.95, y = -0.45, 
           label = expression(beta[P - delta^{15} * N]), 
           size = 2)

ppmr_plot = reg_sim %>% 
  filter(group == "PPMR") %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_smooth(method = lm, color = "black",
              linewidth = 0.2, fill = "#5ab4ac") +
  brms::theme_default() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 6)) +
  labs(x = "Mass",
       y = expression(paste("δ", "15N (\u2030)")),
       subtitle = "b) PPMR") +
  geom_segment(aes(x = -0.05, y = 0, xend = -0.6, yend = 0),
               linetype = "dashed", linewidth = 0.25) +
  geom_segment(aes(x = -0.6, y = 0, xend = -0.6, yend = -0.6),
               linetype = "dashed", linewidth = 0.25) +
  annotate(geom = "text", x = -0.8, y = 0.35, 
           label = expression(beta[M - delta^{15} * N]), 
           size = 2)

ms_plot = reg_sim %>% 
  filter(group == "MS") %>% 
  ggplot(aes(x = x, y = y)) + 
  geom_smooth(method = lm, color = "black",
              linewidth = 0.2, fill = "#01665e") +
  brms::theme_default() + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(size = 6)) +
  labs(x = "log10 Mass",
       y = "log10 Metabolic Rate",
       subtitle = "c) Metabolic Scaling") +
  geom_segment(aes(x = -0.05, y = 0, xend = -0.6, yend = 0),
               linetype = "dashed", linewidth = 0.25) +
  geom_segment(aes(x = -0.6, y = 0, xend = -0.6, yend = -0.6),
               linetype = "dashed", linewidth = 0.25) +
  annotate(geom = "text", x = -1, y = 0.45, label = "Metabolic\nScaling", size = 1.5)


library(patchwork)

regs = tte_plot / ppmr_plot / ms_plot
ggview::ggview(regs, width = 1.5, height = 4)
ggsave(regs, width = 1.5, height = 4, file = "plots/regs2.jpg", dpi = 600)


dens2 = tibble(x = rnorm(500000, -1, 0.3)) %>% 
  ggplot(aes(x = x)) + 
  geom_density(fill = "purple", alpha = 0.3) +
  theme_void()

ggsave(dens2, file = "plots/dens2.jpg", dpi = 600)



# remake fourpanel plot -------------------------------------------------------------------
library(tidyverse)
library(patchwork)

# load individual plots
a = readRDS("plots/a.rds")
b = readRDS("plots/b.rds")
c = readRDS("plots/c.rds")
d = readRDS("plots/d.rds")

# combine into four panels with patchwork.
isd_mass_fourpanel = (a + b)/(c + d)