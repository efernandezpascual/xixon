library(tidyverse); library(vegan)

c("#FFA500",  
  "yellow", 
  "#B3EE3A", 
  "#40E0D0",
  "#5CACEE", 
  "#A020F0",
  "#551A8B") -> colores

read.csv("data/header.csv", fileEncoding = "latin1") %>%
  filter(habitat != "N4D") -> header
read.csv("data/species.csv", fileEncoding = "latin1") %>% group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # merge layers
  species
read.csv("data/fungi-species.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria-species.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### PCA soils

header %>% select(id, habitat) %>% merge(soils) -> datos
datos %>% select(-c(id, habitat)) %>% FactoMineR::PCA(graph = FALSE) -> pca1
cbind((datos %>%  dplyr::select(id, habitat)), data.frame(pca1$ind$coord[, 1:2])) -> pcaInds
pca1$var$coord[, 1:2] %>% data.frame %>% rownames_to_column(var = "Variable") -> pcaVars

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 3*Dim.1, yend = 3*Dim.2)) +
  geom_point(aes(fill = habitat), color = "black", size = 7, shape = 21) +
  ggrepel::geom_label_repel(data = pcaVars, aes(x = 3*Dim.1, y = 3*Dim.2, label = Variable),  show.legend = FALSE, size = 2) +
  ggthemes::theme_tufte() + 
  ggtitle(label = "PCA of soil properties") +
  theme(text = element_text(family = "sans"),
        legend.position = "right", 
        legend.title = element_blank(),
        legend.text = element_text(size = 12, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12, color = "black"),
        plot.margin = unit(c(0, 0.1, 0, 0), "cm")) +
  guides(fill = guide_legend(override.aes = list(shape = 22))) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F1; F1

pca1$eig
pca1$var$contrib %>% data.frame %>% select(Dim.1) %>% arrange(-Dim.1) %>% head(5)
pca1$var$contrib %>% data.frame %>% select(Dim.2) %>% arrange(-Dim.2) %>% head(5)
pca1$var$contrib %>% data.frame %>% select(Dim.3) %>% arrange(-Dim.3) %>% head(5)
datos %>% select(-c(id, habitat)) %>% cor()

ggsave(F1, file = "results/Soil PCA.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 150, units = "mm", dpi = 600)

### NMDS plant communities

header %>%
  select(habitat, id) %>%
  merge(species) %>%
  select(id, taxon, cover) %>%
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 2) -> 
  nmds # Ordination output

decorana(matrix) -> dca1

vegan::scores(dca1) %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  merge(header) %>%
  ggplot(aes(x = DCA1, y = DCA2, fill = habitat)) + 
  geom_point(shape = 21, size = 5, show.legend = T)

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  rename(Dim.1 = NMDS1, Dim.2 = NMDS2) %>%
  merge(header) %>%
  dplyr::select(id, habitat, Dim.1, Dim.2) -> sitescores

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
  geom_point(aes(fill = habitat), color = "black", size = 7, shape = 21) +
  scale_shape_manual(values = c(21, 24)) +
  scale_alpha_manual(values = c(0.4, 1), guide = FALSE) +
  ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 4, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  guides(fill=guide_legend(override.aes=list(shape = 21))) + 
  ggtitle(label = "NMDS of plant communities") +
  theme(text = element_text(family = "sans"),
        legend.position = "right", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
  coord_cartesian(xlim = c(-1.1, 1), ylim = c(-1, 1)) +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F2; F2

ggsave(F2, file = "results/NMDS plants.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 150, units = "mm", dpi = 600)

### NMDS fungal communities

fungi %>%
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
  dplyr::select(id, habitat, Dim.1, Dim.2) -> sitescores

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
  geom_point(aes(fill = habitat), color = "black", size = 7, shape = 21) +
  scale_shape_manual(values = c(21, 24)) +
  scale_alpha_manual(values = c(0.4, 1), guide = FALSE) +
  ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 4, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  guides(fill=guide_legend(override.aes=list(shape = 21))) + 
  ggtitle(label = "NMDS of fungal communities") +
  theme(text = element_text(family = "sans"),
        legend.position = "right", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
  
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F3; F3

ggsave(F3, file = "results/NMDS fungi.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 150, units = "mm", dpi = 600)

### NMDS bacterial communities

bacteria %>%
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
  dplyr::select(id, habitat, Dim.1, Dim.2) -> sitescores

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
  geom_point(aes(fill = habitat), color = "black", size = 7, shape = 21) +
  scale_shape_manual(values = c(21, 24)) +
  scale_alpha_manual(values = c(0.4, 1), guide = FALSE) +
  ggrepel::geom_text_repel(data = sppscores, aes(label = Species), color = "grey33", fontface = "italic", size = 4, segment.color = "transparent") +
  ggthemes::theme_tufte() +
  guides(fill=guide_legend(override.aes=list(shape = 21))) + 
  ggtitle(label = "NMDS of bacterial communities") +
  theme(text = element_text(family = "sans"),
        legend.position = "right", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        legend.text = element_text(size = 10, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 10, color = "black")) +
  scale_x_continuous(name = "NMDS1") + 
  scale_y_continuous(name = "NMDS2") +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F4; F4

ggsave(F4, file = "results/NMDS bacteria.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 150, units = "mm", dpi = 600)