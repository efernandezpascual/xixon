library(tidyverse)

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/plants.csv", fileEncoding = "latin1") -> plants
read.csv("data/fungi.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils
read.csv("data/soil-pca.csv", fileEncoding = "latin1") -> pca

### Compositional novelty for plants

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(plants) %>%
  select(id, taxon, cover) %>%
  mutate(cover = 1) %>% # Transform cover to presence
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 4) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  gather(Trait, Value, - id) %>%
  merge(select(header, id, habitat)) -> sitescores

### Novelty (versus forest)

sitescores %>%
  filter(habitat %in% c("forest")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = forest - Value) %>%
  group_by(id, habitat) %>%
  summarise(plant.forest = sum(abs(Novelty)))  -> plant.forest

### Novelty (versus meadow)

sitescores %>%
  filter(habitat %in% c("meadow")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = meadow - Value) %>%
  group_by(id, habitat) %>%
  summarise(plant.meadow = sum(abs(Novelty))) -> plant.meadow

### Compositional novelty for fungi

fungi %>%
  select(id, ASV, presence) %>%
  spread(ASV, presence, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 4) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  gather(Trait, Value, - id) %>%
  merge(select(header, id, habitat)) -> sitescores

### Novelty (versus forest)

sitescores %>%
  filter(habitat %in% c("forest")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = forest - Value) %>%
  group_by(id, habitat) %>%
  summarise(fungi.forest = sum(abs(Novelty))) -> fungi.forest

### Novelty (versus meadow)

sitescores %>%
  filter(habitat %in% c("meadow")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = meadow - Value) %>%
  group_by(id, habitat) %>%
  summarise(fungi.meadow = sum(abs(Novelty))) -> fungi.meadow

### Compositional novelty for bacteria

bacteria %>%
  select(id, ASV, presence) %>%
  spread(ASV, presence, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

matrix %>%
  vegan::metaMDS(trymax = 100000, k = 4) -> 
  nmds # Ordination output

### Site scores

vegan::scores(nmds, "sites") %>%
  data.frame() %>%
  rownames_to_column("id") %>%
  gather(Trait, Value, - id) %>%
  merge(select(header, id, habitat)) -> sitescores

### Novelty (versus forest)

sitescores %>%
  filter(habitat %in% c("forest")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = forest - Value) %>%
  group_by(id, habitat) %>%
  summarise(bacterial.forest = sum(abs(Novelty))) -> bacterial.forest

### Novelty (versus meadow)

sitescores %>%
  filter(habitat %in% c("meadow")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = meadow - Value) %>%
  group_by(id, habitat) %>%
  summarise(bacterial.meadow = sum(abs(Novelty)))  -> bacterial.meadow

### Merge novelty

header %>%
  select(id, habitat) %>%
  merge(plant.forest) %>%
  merge(plant.meadow) %>%
  merge(fungi.forest) %>%
  merge(fungi.meadow) %>%
  merge(bacterial.forest) %>%
  merge(bacterial.meadow) %>%
  filter(! habitat %in% c("forest", "meadow")) -> novelty

read.csv("data/soils.csv", fileEncoding = "latin1") %>%
  select(id, OM, Pb, Mg) %>%
  merge(novelty)  %>%
  gather(Trait, Value, plant.forest:bacterial.meadow) %>%
  separate(Trait,  
           into = c("Group", "Reference"),
           sep = "\\.") -> df1

c("limegreen",  
  "grey95", 
  "indianred") -> colores

df1 %>%
  group_by(Group) %>%
  summarise(m = mean(Value), se = sd(Value)/sqrt(5)) %>%
  mutate(Trait = fct_relevel(Group, "plant", "fungi", "bacterial")) %>%
  mutate(Trait = fct_recode(Trait, "PLANTAS" = "plant", "HONGOS" = "fungi", "BACTERIAS" = "bacterial")) %>%
  ggplot(aes(Trait, m, fill = Trait)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin =  m - se, ymax = m + se), width = 0.1, size = 1) +
  ggthemes::theme_tufte() +
  labs(y = "Novedad") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 14), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 18, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F1

ggsave(F1, file = "results/figures/Fig4-1-SEBOT.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 140, units = "mm", dpi = 600)


c("#551A8B",
  "#A020F0",
  "#40E0D0",
  "#5CACEE") -> 
  colores

df1 %>%
  group_by(habitat) %>%
  summarise(m = mean(Value), se = sd(Value)/sqrt(5)) %>%
  mutate(Trait = fct_relevel(habitat, "industrial", "roadside", "park", "residential")) %>%
  mutate(Trait = fct_recode(Trait, "INDUSTRIALES" = "industrial", "CUNETAS" = "roadside", "PARQUES" = "park", "RESIDENCIALES" = "residential")) %>%
  ggplot(aes(Trait, m, fill = Trait)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin =  m - se, ymax = m + se), width = 0.1, size = 1) +
  ggthemes::theme_tufte() +
  labs(y = "Novedad") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 14), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 18, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F2

ggsave(F2, file = "results/figures/Fig4-2-SEBOT.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 140, units = "mm", dpi = 600)


c("forestgreen",  
  "gold") -> 
  colores

df1 %>%
  group_by(Reference) %>%
  summarise(m = mean(Value), se = sd(Value)/sqrt(5)) %>%
  mutate(Trait = fct_relevel(Reference, "forest", "meadow")) %>%
  mutate(Trait = fct_recode(Trait, "BOSQUES" = "forest", "PRADOS" = "meadow")) %>%
  ggplot(aes(Trait, m, fill = Trait)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_errorbar(aes(ymin =  m - se, ymax = m + se), width = 0.1, size = 1) +
  ggthemes::theme_tufte() +
  labs(y = "Novedad") +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 14), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 18, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 18),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(.5,.5,.5,.5), "cm")) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) -> F3; F3

ggsave(F3, file = "results/figures/Fig4-3-SEBOT.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 140, units = "mm", dpi = 600)



novelty %>%
  gather(Trait, Novelty, -c(id, habitat)) %>%
  mutate(Trait = fct_relevel(Trait, "plant.forest", "plant.meadow", "fungi.forest", "fungi.meadow", "bacterial.forest", "bacterial.meadow")) %>%
  mutate(Trait = fct_recode(Trait, "(E) Bacterial\n      novelty\n      vs. forests" = "bacterial.forest",
                            "(C) Fungal\n      novelty\n      vs. forests" = "fungi.forest",
                            "(A) Plant\n      novelty\n      vs. forests" = "plant.forest",
                            "(F) Bacterial\n      novelty\n      vs. meadows" = "bacterial.meadow",
                            "(D) Fungal\n      novelty\n      vs. meadows" = "fungi.meadow",
                            "(B) Plant\n      novelty\n      vs. meadows" = "plant.meadow",)) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial"))%>%
  ggplot(aes(habitat, Novelty, fill = habitat)) +
  geom_boxplot() +
  facet_wrap( ~ Trait, nrow = 1)+
  ggthemes::theme_tufte() +
  xlab("Urban habitats") + ylab("Novelty") +
  scale_fill_manual(values = c( 
    "#40E0D0",
    "#A020F0",
    "#5CACEE",
    "#551A8B")) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 10, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) +
  geom_hline(yintercept = 0, linetype = "dashed") -> F4; F4

ggsave(F4, file = "results/figures/Fig4-SEBOT.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 140, units = "mm", dpi = 600)