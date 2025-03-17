library(tidyverse); library(ggvenn); library(vegan)

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/plants.csv", fileEncoding = "latin1") -> plants
read.csv("data/fungi.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### Plant species pools

list(
  `Forests (5 plots)` = 
    header %>% 
    filter(habitat == "forest") %>% 
    select(id) %>% 
    merge(plants) %>% 
    pull(taxon) %>% 
    unique, 
  `Meadows (5 plots)` = 
    header %>% 
    filter(habitat == "meadow") %>% 
    select(id) %>% 
    merge(plants) %>% 
    pull(taxon) %>% 
    unique, 
  `Urban habitats (20 plots)` = 
    header %>% 
    filter(habitat %in% c("park", "residential", "roadside", "industrial")) %>% 
    select(id) %>% 
    merge(plants) %>% 
    pull(taxon) %>% 
    unique) -> df1

plants %>%
  filter(taxon %in% df1$`Forests (5 plots)` & ! taxon %in% df1$`Meadows (5 plots)` & ! taxon %in% df1$`Urban habitats (20 plots)`) %>%
  group_by(taxon) %>%
  tally() %>%
  arrange(-n)

plants %>%
  filter(! taxon %in% df1$`Forests (5 plots)` & taxon %in% df1$`Meadows (5 plots)` & ! taxon %in% df1$`Urban habitats (20 plots)`) %>%
  group_by(taxon) %>%
  tally() %>%
  arrange(-n)

plants %>%
  filter(! taxon %in% df1$`Forests (5 plots)` & ! taxon %in% df1$`Meadows (5 plots)` & taxon %in% df1$`Urban habitats (20 plots)`) %>%
  group_by(taxon) %>%
  tally() %>%
  arrange(-n)

### Fungal species pools

list(
  `Forests (5 plots)` = 
    header %>% 
    filter(habitat == "forest") %>% 
    select(id) %>% 
    merge(fungi) %>% 
    pull(ASV) %>% 
    unique, 
  `Meadows (5 plots)` = 
    header %>% 
    filter(habitat == "meadow") %>% 
    select(id) %>% 
    merge(fungi) %>% 
    pull(ASV) %>% 
    unique, 
  `Urban habitats (20 plots)` = 
    header %>% 
    filter(habitat %in% c("park", "residential", "roadside", "industrial")) %>% 
    select(id) %>% 
    merge(fungi) %>% 
    pull(ASV) %>% 
    unique) -> df2

fungi %>%
  filter(ASV %in% df2$`Forests (5 plots)` & ! ASV %in% df2$`Meadows (5 plots)` & ! ASV %in% df2$`Urban habitats (20 plots)`) %>% 
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)

fungi %>%
  filter(! ASV %in% df2$`Forests (5 plots)` & ASV %in% df2$`Meadows (5 plots)` & ! ASV %in% df2$`Urban habitats (20 plots)`) %>%
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)

fungi %>%
  filter(! ASV %in% df2$`Forests (5 plots)` & ! ASV %in% df2$`Meadows (5 plots)` & ASV %in% df2$`Urban habitats (20 plots)`) %>%
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)

### Bacterial species pools

list(
  `Forests (5 plots)` = 
    header %>% 
    filter(habitat == "forest") %>% 
    select(id) %>% 
    merge(bacteria) %>% 
    pull(ASV) %>% 
    unique, 
  `Meadows (5 plots)` = 
    header %>% 
    filter(habitat == "meadow") %>% 
    select(id) %>% 
    merge(bacteria) %>% 
    pull(ASV) %>% 
    unique, 
  `Urban habitats (20 plots)` = 
    header %>% 
    filter(habitat %in% c("park", "residential", "roadside", "industrial")) %>% 
    select(id) %>% 
    merge(bacteria) %>% 
    pull(ASV) %>% 
    unique) -> df3

bacteria %>%
  filter(ASV %in% df3$`Forests (5 plots)` & ! ASV %in% df3$`Meadows (5 plots)` & ! ASV %in% df3$`Urban habitats (20 plots)`) %>% 
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n) %>%
  print(n =  200)

bacteria %>%
  filter(! ASV %in% df3$`Forests (5 plots)` & ASV %in% df3$`Meadows (5 plots)` & ! ASV %in% df3$`Urban habitats (20 plots)`) %>%
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)

bacteria %>%
  filter(! ASV %in% df3$`Forests (5 plots)` & ! ASV %in% df3$`Meadows (5 plots)` & ASV %in% df3$`Urban habitats (20 plots)`) %>%
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)

# Venn

ggvenn(df3,
       fill_color = c("forestgreen", "gold", "#551A8B"),
       stroke_size = 0.5, set_name_size = 3, text_size = 2.5,
       show_percentage = FALSE) +
  ggtitle(label = "(C) Bacterial ASVs") +
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
        plot.margin = unit(c(0,0,0,0), "cm")) -> F3C; F3C

ggvenn(df2,
       fill_color = c("forestgreen", "gold", "#551A8B"),
       stroke_size = 0.5, set_name_size = 3, text_size = 2.5,
       show_percentage = FALSE) +
  ggtitle(label = "(B) Fungal ASVs") +
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
        plot.margin = unit(c(0,0,0,0), "cm")) -> F3B; F3B

ggvenn(df1,
       fill_color = c("forestgreen", "gold", "#551A8B"),
       stroke_size = 0.5, set_name_size = 3, text_size = 2.5,
       show_percentage = FALSE) +
  ggtitle(label = "(A) Plant species") +
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
        plot.margin = unit(c(0,0,0,0), "cm")) -> F3A; F3A

cowplot::plot_grid(F3A, F3B, F3C, nrow = 1) -> F3ABC; F3ABC

### NMDS

c("forestgreen",  
  "gold", 
  "#40E0D0",
  "#A020F0",
  "#5CACEE",
  "#551A8B") -> colores

read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### NMDS plant communities

header %>%
  select(habitat, id) %>%
  merge(plants) %>%
  select(id, taxon, cover) %>%
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Env fit

soils %>% select(Pb, OM, Mg) -> soilvar

vegan::envfit(nmds, soilvar, permutations = 100000, strata = NULL, 
       choices=c(1,2),  display = "sites", w  = weights(nmds, display),
       na.rm = FALSE)

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial")) -> sitescores

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
  geom_point(aes(fill = habitat), color = "black", size = 3.5, shape = 21) +
  #ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 3.5, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  ggtitle(label = "(D) Plant communities") +
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
        plot.margin = unit(c(0.25,0,0,0), "cm")) +
  scale_x_continuous(name = "NMDS1", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) + 
  scale_y_continuous(name = "NMDS2", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F3D; F3D

### NMDS fungal communities

fungi %>%
  select(id, ASV, presence) %>%
  spread(ASV, presence, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Env fit

soils %>% select(Pb, OM, Mg) -> soilvar

vegan::envfit(nmds, soilvar, permutations = 100000, strata = NULL, 
              choices=c(1,2),  display = "sites", w  = weights(nmds, display),
              na.rm = FALSE) -> ef

ef
ef$vectors$arrows %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable == "OM") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) -> pcaVars

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial")) -> sitescores

### Species scores

vegan::scores(nmds, "species") %>%
  data.frame() %>%
  rownames_to_column("Taxon") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  mutate(Species = Taxon) %>%
  dplyr::select(Taxon, Species, Dim.1, Dim.2) -> sppscores

### Plot NMDS sites

ggplot(sitescores, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(aes(fill = habitat), color = "black", size = 3.5, shape = 21) +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2)) +
  geom_label(data = pcaVars, aes(x = Dim.1, y = Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  #ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 3.5, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  ggtitle(label = "(E) Fungal communities") +
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
        plot.margin = unit(c(0.25,0,0,0), "cm")) +
  scale_x_continuous(name = "NMDS1", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 1)) + 
  scale_y_continuous(name = "NMDS2", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F3E; F3E

### NMDS bacterial communities

bacteria %>%
  select(id, ASV, presence) %>%
  spread(ASV, presence, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

### Env fit

soils %>% select(Pb, OM, Mg) -> soilvar

vegan::envfit(nmds, soilvar, permutations = 100000, strata = NULL, 
              choices=c(1,2),  display = "sites", w  = weights(nmds, display),
              na.rm = FALSE) -> ef

ef
ef$vectors$arrows %>%
  data.frame %>%
  rownames_to_column(var = "Variable") %>%
  filter(Variable == "OM") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) -> pcaVars

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial")) -> sitescores

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
  geom_point(aes(fill = habitat), color = "black", size = 3.5, shape = 21) +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = Dim.1, yend = Dim.2)) +
  geom_label(data = pcaVars, aes(x = Dim.1, y = Dim.2, label = Variable),  show.legend = FALSE, size = 4) +
  #ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 3.5, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  ggtitle(label = "(F) Bacterial communities") +
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
        plot.margin = unit(c(0.25,0,0,0), "cm")) +
  scale_x_continuous(name = "NMDS1", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-2, 2, by = 0.5)) + 
  scale_y_continuous(name = "NMDS2", labels = scales::number_format(accuracy = 0.1),
                     breaks = seq(-.3, .3, by = 0.3)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F3F; F3F

### Legend

cowplot::get_plot_component(F3D + 
                      theme(legend.position = "bottom", 
                            legend.box.margin = margin(1, 0, 0, 0),
                            legend.title = element_blank()) +
                        guides(fill = guide_legend(nrow = 1)),
                      "guide-box-bottom") -> legend

### Merge

cowplot::plot_grid(F3D, F3E, F3F, nrow = 1) -> F3DEF; F3DEF
cowplot::plot_grid(F3DEF, legend, nrow = 2,
                   rel_heights = c(1, .1)) -> F3DEFL; F3DEFL

cowplot::plot_grid(F3ABC, F3DEFL, nrow = 2) -> F3

ggsave(F3, file = "results/figures/Fig3.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 2*75, units = "mm", dpi = 600)


