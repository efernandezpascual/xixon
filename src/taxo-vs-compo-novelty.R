library(tidyverse)

read.csv("data/header.csv", fileEncoding = "latin1") %>%
  filter(habitat != "N4D") -> header
read.csv("data/species.csv", fileEncoding = "latin1") %>% group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # merge layers
  species
read.csv("data/fungi-species.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria-species.csv", fileEncoding = "latin1") -> bacteria

### Plant novelty versus N1

header %>%
  filter(habitat %in% c("N1")) %>%
  merge(species) %>%
  pull(taxon) %>%
  unique -> spp # Non-urban species

species %>%
  mutate(Novelspp = ifelse(! taxon %in% spp, "Yes", "No")) %>%
  group_by(id, Novelspp) %>%
  tally() %>%
  spread(Novelspp, n, fill = 0) %>%
  mutate(TPN1 = Yes / (No + Yes)) %>%
  select(id, TPN1) -> tpn1 # taxonomic plant novelty
  
### Fungal novelty versus N1

header %>%
  filter(habitat %in% c("N1")) %>%
  merge(fungi) %>%
  pull(taxon) %>%
  unique -> spp # Non-urban species

fungi %>%
  mutate(Novelspp = ifelse(! taxon %in% spp, "Yes", "No")) %>%
  group_by(id, Novelspp) %>%
  tally() %>%
  spread(Novelspp, n, fill = 0) %>%
  mutate(TFN1 = Yes / (No + Yes)) %>%
  select(id, TFN1) -> tfn1 # taxonomic fungal novelty

### Bacterial novelty versus N1

header %>%
  filter(habitat %in% c("N1")) %>%
  merge(bacteria) %>%
  pull(taxon) %>%
  unique -> spp # Non-urban species

bacteria %>%
  mutate(Novelspp = ifelse(! taxon %in% spp, "Yes", "No")) %>%
  group_by(id, Novelspp) %>%
  tally() %>%
  spread(Novelspp, n, fill = 0) %>%
  mutate(TBN1 = Yes / (No + Yes)) %>%
  select(id, TBN1) -> tbn1 # taxonomic bacterial novelty

### Plant novelty versus N2

header %>%
  filter(habitat %in% c("N2")) %>%
  merge(species) %>%
  pull(taxon) %>%
  unique -> spp # Non-urban species

species %>%
  mutate(Novelspp = ifelse(! taxon %in% spp, "Yes", "No")) %>%
  group_by(id, Novelspp) %>%
  tally() %>%
  spread(Novelspp, n, fill = 0) %>%
  mutate(TPN2 = Yes / (No + Yes)) %>%
  select(id, TPN2) -> tpn2 # taxonomic plant novelty

### Fungal novelty versus N2

header %>%
  filter(habitat %in% c("N2")) %>%
  merge(fungi) %>%
  pull(taxon) %>%
  unique -> spp # Non-urban species

fungi %>%
  mutate(Novelspp = ifelse(! taxon %in% spp, "Yes", "No")) %>%
  group_by(id, Novelspp) %>%
  tally() %>%
  spread(Novelspp, n, fill = 0) %>%
  mutate(TFN2 = Yes / (No + Yes)) %>%
  select(id, TFN2) -> tfn2 # taxonomic fungal novelty

### Bacterial novelty versus N2

header %>%
  filter(habitat %in% c("N2")) %>%
  merge(bacteria) %>%
  pull(taxon) %>%
  unique -> spp # Non-urban species

bacteria %>%
  mutate(Novelspp = ifelse(! taxon %in% spp, "Yes", "No")) %>%
  group_by(id, Novelspp) %>%
  tally() %>%
  spread(Novelspp, n, fill = 0) %>%
  mutate(TBN2 = Yes / (No + Yes)) %>%
  select(id, TBN2) -> tbn2 # taxonomic bacterial novelty

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
  summarise(CPN1 = sum(abs(Novelty))) %>%
  filter(! habitat %in% c("N1", "N2")) %>%
  group_by() %>%
  mutate(CPN1 = CPN1/max(CPN1))  -> cpn1

### Novelty (versus N2)

sitescores %>%
  filter(habitat %in% c("N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N2 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CPN2 = sum(abs(Novelty))) %>%
  filter(! habitat %in% c("N1", "N2")) %>%
  group_by() %>%
  mutate(CPN2 = CPN2/max(CPN2))  -> cpn2

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
  summarise(CFN1 = sum(abs(Novelty))) %>%
  filter(! habitat %in% c("N1", "N2")) %>%
  group_by() %>%
  mutate(CFN1 = CFN1/max(CFN1)) -> cfn1

### Novelty (versus N2)

sitescores %>%
  filter(habitat %in% c("N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N2 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CFN2 = sum(abs(Novelty)))%>%
  filter(! habitat %in% c("N1", "N2")) %>%
  group_by() %>%
  mutate(CFN2 = CFN2/max(CFN2)) -> cfn2

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
  summarise(CBN1 = sum(abs(Novelty))) %>%
  filter(! habitat %in% c("N1", "N2")) %>%
  group_by() %>%
  mutate(CBN1 = CBN1/max(CBN1)) -> cbn1

### Novelty (versus N2)

sitescores %>%
  filter(habitat %in% c("N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N2 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CBN2 = sum(abs(Novelty))) %>%
  filter(! habitat %in% c("N1", "N2")) %>%
  group_by() %>%
  mutate(CBN2 = CBN2/max(CBN2)) -> cbn2

### Merge novelty

header %>%
  select(id, habitat) %>%
  merge(tpn1) %>%
  merge(tfn1) %>%
  merge(tbn1)  %>%
  merge(tpn2) %>%
  merge(tfn2) %>%
  merge(tbn2)  %>%
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
  separate(Trait, into = c("Type", "Groups", "Reference"), sep = c(1, 2)) %>%
  spread(Type, Novelty) %>%
  ggplot(aes(C, T, color = Reference)) +
  facet_grid(~ Groups) +
  geom_point() +
  geom_smooth(method = "lm")

novelty %>%
  gather(Trait, Novelty, -c(id, habitat)) %>%
  separate(Trait, into = c("Type", "Groups", "Reference"), sep = c(1, 2)) %>%
  spread(Type, Novelty) %>%
  select(C, T) %>%
  cor


novelty %>%
  gather(Trait, Novelty, -c(id, habitat)) %>%
  separate(Trait, into = c("Type", "Reference"), sep = c(2)) %>%
  mutate(Type = fct_relevel(Type, "TB", "TF","TP", "CB", "CF", "CP")) %>%
  mutate(Type = fct_recode(Type, "(A) Taxonomic\nbacterial novelty" = "TB",
                           "(B) Taxonomic\nfungal novelty" = "TF",
                           "(C) Taxonomic\nplant novelty" = "TP",
                           "(D) Compositional\nbacterial novelty" = "CB",
                           "(E) Compositional\nfungal novelty" = "CF",
                           "(P) Compositional\nplant novelty" = "CP",)) %>%
  mutate(habitat = fct_relevel(habitat, "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Parks" = "N3", "Roadsides" = "N4B",
                            "Residential vacant lots" = "N4A", "Industrial vacant lots" = "N4C")) %>%
  mutate(Reference = fct_recode(Reference, "Forest patches" = "N1", "Meadows" = "N2")) %>%
  ggplot(aes(habitat, Novelty, fill = Reference)) +
  geom_boxplot() +
  facet_grid( ~ Type)+
  ggthemes::theme_tufte() +
  xlab("Urban habitat") + ylab("Novelty (proportion)") +
  scale_fill_manual(name = "Reference non-urban habitat",
                    values = c("forestgreen",
                               "gold")) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "bottom", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 10, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 8, color = "black", angle = -90, hjust = 0),
        axis.text.y = element_text(size = 8, color = "black"),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) +
  geom_hline(yintercept = 0.5, linetype = "dashed") -> F3; F3

ggsave(F3, file = "results/figures/Fig3.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 120, units = "mm", dpi = 600)
