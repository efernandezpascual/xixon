library(tidyverse)

read.csv("data/header.csv", fileEncoding = "latin1") %>%
  filter(habitat != "N4D") -> header
read.csv("data/species.csv", fileEncoding = "latin1") %>% group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # merge layers
  species
read.csv("data/fungi-species.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria-species.csv", fileEncoding = "latin1") -> bacteria

### Compositional novelty for plants

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(species) %>%
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

### Novelty (versus N1)

sitescores %>%
  filter(habitat %in% c("N1")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N1 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CPN1 = sum(abs(Novelty)))  -> cpn1

### Novelty (versus N2)

sitescores %>%
  filter(habitat %in% c("N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N2 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CPN2 = sum(abs(Novelty))) -> cpn2

### Compositional novelty for fungi

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(fungi) %>%
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

### Novelty (versus N1)

sitescores %>%
  filter(habitat %in% c("N1")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N1 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CFN1 = sum(abs(Novelty))) -> cfn1

### Novelty (versus N2)

sitescores %>%
  filter(habitat %in% c("N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N2 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CFN2 = sum(abs(Novelty))) -> cfn2

### Compositional novelty for bacteria

header %>%
  select(habitat, id) %>%
  filter(habitat != "N4D") %>%
  merge(bacteria) %>%
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

### Novelty (versus N1)

sitescores %>%
  filter(habitat %in% c("N1")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N1 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CBN1 = sum(abs(Novelty))) -> cbn1

### Novelty (versus N2)

sitescores %>%
  filter(habitat %in% c("N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N2 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CBN2 = sum(abs(Novelty)))  -> cbn2

### Merge novelty

header %>%
  select(id, habitat) %>%
  merge(cpn1) %>%
  merge(cpn2) %>%
  merge(cfn1) %>%
  merge(cfn2) %>%
  merge(cbn1) %>%
  merge(cbn2) %>%
  filter(! habitat %in% c("N1", "N2")) -> novelty

novelty %>% 
  select(-c(id, habitat)) %>% cor

novelty %>%
  gather(Trait, Novelty, -c(id, habitat)) %>%
  mutate(Trait = fct_relevel(Trait, "CBN1", "CFN1", "CPN1", "CBN2", "CFN2", "CPN2")) %>%
  mutate(Trait = fct_relevel(Trait, "CBN1", "CBN2", "CFN1", "CFN2", "CPN1", "CPN2")) %>%
  mutate(Trait = fct_recode(Trait, "(A) Bacterial\n      novelty\n      vs. forests" = "CBN1",
                           "(C) Fungal\n      novelty\n      vs. forests" = "CFN1",
                           "(E) Plant\n      novelty\n      vs. forests" = "CPN1",
                           "(B) Bacterial\n      novelty\n      vs. meadows" = "CBN2",
                           "(D) Fungal\n      novelty\n      vs. meadows" = "CFN2",
                           "(F) Plant\n      novelty\n      vs. meadows" = "CPN2",)) %>%
  mutate(habitat = fct_relevel(habitat, "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) %>%
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
  geom_hline(yintercept = 0, linetype = "dashed") -> F3; F3

ggsave(F3, file = "results/figures/Fig3.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 75, units = "mm", dpi = 600)

### Test

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

read.csv("data/soils.csv", fileEncoding = "latin1") %>%
  select(id, OM, Pb) %>%
  merge(novelty) %>% 
  select(OM, Pb, CPN1:CBN2) %>%
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

read.csv("data/soils.csv", fileEncoding = "latin1") %>%
  select(id, OM, Pb) %>%
  merge(novelty) -> df1

lm(CBN1 ~ habitat + OM + Pb, data = df1) -> m1
summary(m1)

lm(CBN2 ~ habitat + OM + Pb, data = df1) -> m2
summary(m2)

lm(CFN1 ~ habitat + OM + Pb, data = df1) -> m3
summary(m3)

lm(CFN2 ~ habitat + OM + Pb, data = df1) -> m4
summary(m4)

lm(CPN1 ~ habitat + OM + Pb, data = df1) -> m5
summary(m5)

lm(CPN2 ~ habitat + OM + Pb, data = df1) -> m6
summary(m6)



lm(CBN1 ~  OM + Pb, data = df1) -> m1
summary(m1)

lm(CBN2 ~ OM + Pb, data = df1) -> m2
summary(m2)

lm(CFN1 ~ OM + Pb, data = df1) -> m3
summary(m3)

lm(CFN2 ~  OM + Pb, data = df1) -> m4
summary(m4)

lm(CPN1 ~  OM + Pb, data = df1) -> m5
summary(m5)

lm(CPN2 ~  OM + Pb, data = df1) -> m6
summary(m6)

df1 %>%
  gather(Trait, Value, CPN1:CBN2) %>%
  separate(Trait,  
          into = c("Group", "Reference"),
          sep = 2) -> df2

nlme::lme(Value ~ Group + Reference  + OM + Pb,  random=~1 | habitat, data = df2) -> m1
summary(m1)
