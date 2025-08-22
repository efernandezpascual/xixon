library(tidyverse)

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/plants.csv", fileEncoding = "latin1") -> plants
read.csv("data/fungi.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### Compositional novelty for plants

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(plants) %>%
  select(id, taxon, cover) %>%
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

novelty %>% 
  select(-c(id, habitat)) %>% cor

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

ggsave(F4, file = "results/figures/Fig4.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 75, units = "mm", dpi = 600)

### Correlation

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

read.csv("data/soils.csv", fileEncoding = "latin1") %>%
  select(id, OM, Pb, Mg) %>%
  merge(novelty) %>% 
  select(OM, Pb, Mg, plant.forest:bacterial.meadow) %>%
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
  geom_tile(color = "white") +
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

### Model

read.csv("data/soils.csv", fileEncoding = "latin1") %>%
  select(id, OM, Pb, Mg) %>%
  merge(novelty)  %>%
  gather(Trait, Value, plant.forest:bacterial.meadow) %>%
  separate(Trait,  
          into = c("Group", "Reference"),
          sep = "\\.") -> df1

df1 %>% head

df1 %>%
  group_by(habitat) %>%
  summarise(m = mean(Value), se = sd(Value)/sqrt(5))

df1 %>%
  group_by(Group) %>%
  summarise(m = mean(Value), se = sd(Value)/sqrt(5))

df1 %>%
  group_by(Reference) %>%
  summarise(m = mean(Value), se = sd(Value)/sqrt(5))

nlme::lme(Value ~ Group + Reference + habitat + OM + Pb + Mg,  random=~1 | id, data = df1) -> m1
save(m1, file = "results/model/m1.RData")
summary(m1)
nlme::anova.lme(m1)

### Diagnostic plots

library(performance)
library(see)
check_model(m1)

### Alternative models

# nlme::lme(Value ~ Group + Reference + habitat, random=~1 | id, data = df1) -> m2
# summary(m2)
# nlme::anova.lme(m2)
# 
# lm(Value ~ Group + Reference + habitat, data = df1) -> m3
# summary(m3)
# anova(m3)
# 
# lm(Value ~ Group, data = df1) -> m3
# summary(m3)
# anova(m3)
# 
# lm(Value ~ Reference, data = df1) -> m3
# summary(m3)
# anova(m3)
# 
# lm(Value ~ habitat, data = df1) -> m3
# summary(m3)
# anova(m3)
