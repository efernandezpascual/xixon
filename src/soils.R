library(tidyverse)

c("forestgreen",  
  "gold", 
  "#40E0D0",
  "#A020F0",
  "#5CACEE",
  "#551A8B") -> colores

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### PCA soils

header %>% select(id, habitat) %>% merge(soils) -> datos
datos %>% select(-c(id, habitat)) %>% FactoMineR::PCA(graph = FALSE) -> pca1
cbind((datos %>%  dplyr::select(id, habitat)), data.frame(pca1$ind$coord[, 1:2])) %>%
  mutate(habitat = fct_relevel(habitat, "N1", "N2", "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "N1", "Meadows" = "N2",
                              "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) -> pcaInds
pca1$var$coord[, 1:2] %>% data.frame %>% rownames_to_column(var = "Variable") %>%
  mutate(Dim.1 = ifelse(Variable == "Pb", Dim.1 + .05, Dim.1)) -> pcaVars

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 6*Dim.1, yend = 6*Dim.2)) +
  geom_point(aes(fill = habitat), color = "black", size = 5, shape = 23) +
  geom_label(data = pcaVars, aes(x = 6*Dim.1, y = 6*Dim.2, label = Variable),  show.legend = FALSE, size = 3) +
  ggthemes::theme_tufte() + 
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "bottom", 
        #legend.direction = "vertical",
        legend.title = element_blank(),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  guides(fill = guide_legend(nrow = 1)) -> FS1; FS1

ggsave(FS1, file = "results/figures/FigS1.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 160, units = "mm", dpi = 600)

pca1$eig
pca1$var$contrib %>% data.frame %>% select(Dim.1) %>% arrange(-Dim.1) %>% head(5)
pca1$var$contrib %>% data.frame %>% select(Dim.2) %>% arrange(-Dim.2) %>% head(5)
pca1$var$contrib %>% data.frame %>% select(Dim.3) %>% arrange(-Dim.3) %>% head(5)

### Correlation

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

datos %>%
  select(-c(id, habitat)) %>%
  cor() %>% 
  round(1) %>%
  get_lower_tri() %>%
  reshape2::melt(na.rm = TRUE) %>%
  # data.frame %>%
  # rownames_to_column(var = "var1") %>%
  # gather(var2, value, -var1) %>%
  # na.omit %>%
  # mutate(value = round(value, 2)) %>%
  ggplot(aes(Var2, Var1, fill = value)) +
  geom_tile() +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkblue", high = "indianred3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggthemes::theme_tufte() +
  geom_text(aes(label = value), size = 3) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "right", 
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
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) +
  coord_fixed() -> FS2; FS2

ggsave(FS2, file = "results/figures/FigS2.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 180, units = "mm", dpi = 600)

