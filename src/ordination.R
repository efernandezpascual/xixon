library(tidyverse)

c("forestgreen",  
  "gold", 
  "#40E0D0",
  "#A020F0",
  "#5CACEE",
  "#551A8B") -> colores

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/species.csv", fileEncoding = "latin1") %>% group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # merge layers
  species
read.csv("data/fungi-species.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria-species.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### NMDS plant communities

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(species) %>%
  select(id, taxon, cover) %>%
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) %>%
  mutate(habitat = fct_relevel(habitat, "N1", "N2", "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "N1", "Meadows" = "N2",
                              "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) -> sitescores

### Species scores

vegan::scores(nmds, "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = Taxon) %>%
  separate(Species, into = c("G", "S")) %>%
  mutate(Species = paste(substr(G, 1, 3), substr(S, 1, 3), sep = "")) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2) -> sppscores

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(fill = habitat), color = "black", size = 3.5, shape = 23) +
  #ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 3.5, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  ggtitle(label = "(C) Plant communities") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_blank(),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_x_continuous(name = "NMDS1", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) + 
  scale_y_continuous(name = "NMDS2", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F2C; F2C

### NMDS fungal communities

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(fungi) %>%
  select(id, taxon, cover) %>%
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) %>%
  mutate(habitat = fct_relevel(habitat, "N1", "N2", "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "N1", "Meadows" = "N2",
                              "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) -> sitescores

### Species scores

vegan::scores(nmds, "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = Taxon) %>%
  separate(Species, into = c("G", "S")) %>%
  mutate(Species = paste(substr(G, 1, 3), substr(S, 1, 3), sep = "")) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2) -> sppscores

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(fill = habitat), color = "black", size = 3.5, shape = 23) +
  #ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 3.5, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  ggtitle(label = "(B) Fungal communities") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_blank(),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_x_continuous(name = "NMDS1", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 1)) + 
  scale_y_continuous(name = "NMDS2", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F2B; F2B

### NMDS bacterial communities

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(bacteria) %>%
  select(id, taxon, cover) %>%
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) %>%
  mutate(habitat = fct_relevel(habitat, "N1", "N2", "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "N1", "Meadows" = "N2",
                              "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) -> sitescores

### Species scores

vegan::scores(nmds, "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = Taxon) %>%
  separate(Species, into = c("G", "S")) %>%
  mutate(Species = paste(substr(G, 1, 3), substr(S, 1, 3), sep = "")) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2) -> sppscores

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(fill = habitat), color = "black", size = 3.5, shape = 23) +
  #ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 3.5, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  ggtitle(label = "(A) Bacterial communities") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_blank(),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0,0,0,0), "cm")) +
  scale_x_continuous(name = "NMDS1", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) + 
  scale_y_continuous(name = "NMDS2", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-.3, .3, by = 0.3)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F2A; F2A

### Legend

cowplot::get_plot_component(F2A + 
                      theme(legend.position = "bottom", 
                            legend.box.margin = margin(1, 0, 0, 0),
                            legend.title = element_blank()) +
                        guides(fill = guide_legend(nrow = 1)),
                      "guide-box-bottom") -> legend

### Merge

cowplot::plot_grid(F2A, F2B, F2C, nrow = 1) -> F2; F2
cowplot::plot_grid(F2, legend, nrow = 2,
                   rel_heights = c(1, .1)) -> F2l; F2l

ggsave(F2l, file = "results/figures/Fig2.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 75, units = "mm", dpi = 600)

