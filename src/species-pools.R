library(tidyverse); library(ggvenn)

read.csv("data/header.csv", fileEncoding = "latin1") %>%
  filter(habitat != "N4D") -> header
read.csv("data/species.csv", fileEncoding = "latin1") %>% group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # merge layers
  species
read.csv("data/fungi-species.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria-species.csv", fileEncoding = "latin1") -> bacteria

### Plant species pools

list(
  Forests = 
    header %>% 
    filter(habitat == "N1") %>% 
    select(id) %>% 
    merge(species) %>% 
    pull(taxon) %>% 
    unique, 
  Meadows = 
    header %>% 
    filter(habitat == "N2") %>% 
    select(id) %>% 
    merge(species) %>% 
    pull(taxon) %>% 
    unique, 
  `Urban habitats` = 
    header %>% 
    filter(habitat %in% c("N3", "N4A", "N4B", "N4C")) %>% 
    select(id) %>% 
    merge(species) %>% 
    pull(taxon) %>% 
    unique) -> df1

### Fungal species pools

list(
  Forests = 
    header %>% 
    filter(habitat == "N1") %>% 
    select(id) %>% 
    merge(fungi) %>% 
    pull(taxon) %>% 
    unique, 
  Meadows = 
    header %>% 
    filter(habitat == "N2") %>% 
    select(id) %>% 
    merge(fungi) %>% 
    pull(taxon) %>% 
    unique, 
  `Urban habitats` = 
    header %>% 
    filter(habitat %in% c("N3", "N4A", "N4B", "N4C")) %>% 
    select(id) %>% 
    merge(fungi) %>% 
    pull(taxon) %>% 
    unique) -> df2

### Bacterial species pools

list(
  Forests = 
    header %>% 
    filter(habitat == "N1") %>% 
    select(id) %>% 
    merge(bacteria) %>% 
    pull(taxon) %>% 
    unique, 
  Meadows = 
    header %>% 
    filter(habitat == "N2") %>% 
    select(id) %>% 
    merge(bacteria) %>% 
    pull(taxon) %>% 
    unique, 
  `Urban habitats` = 
    header %>% 
    filter(habitat %in% c("N3", "N4A", "N4B", "N4C")) %>% 
    select(id) %>% 
    merge(bacteria) %>% 
    pull(taxon) %>% 
    unique) -> df3

# Venn

ggvenn(df3,
       fill_color = c("forestgreen", "gold", "#551A8B"),
       stroke_size = 0.5, set_name_size = 3, text_size = 2.5,
       show_percentage = FALSE) +
  ggtitle(label = "(A) Bacterial taxa") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0,0,0,0), "cm")) -> F1A; F1A

ggvenn(df2,
       fill_color = c("forestgreen", "gold", "#551A8B"),
       stroke_size = 0.5, set_name_size = 3, text_size = 2.5,
       show_percentage = FALSE) +
  ggtitle(label = "(B) Fungal taxa") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0,0,0,0), "cm")) -> F1B; F1B

ggvenn(df1,
       fill_color = c("forestgreen", "gold", "#551A8B"),
       stroke_size = 0.5, set_name_size = 3, text_size = 2.5,
       show_percentage = FALSE) +
  ggtitle(label = "(C) Plant taxa") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10, margin = margin(0, 0, 5, 0)),
        plot.margin = unit(c(0,0,0,0), "cm")) -> F1C; F1C

cowplot::plot_grid(F1A, F1B, F1C, nrow = 1) -> F1; F1

ggsave(F1, file = "results/figures/Fig1.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 75, units = "mm", dpi = 600)

