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

### Novelty (versus N1 and N2 together)

sitescores %>%
  filter(habitat %in% c("N1", "N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  gather(Reference, Centroid, N1:N2) %>%
  arrange(id, Trait, Reference) %>%
  mutate(Novelty = Centroid - Value) %>%
  group_by(id, Trait) %>%
  filter(Novelty == min(Novelty)) %>%
  group_by(id, habitat) %>%
  summarise(Novelty = sum(Novelty)) %>%
  group_by(habitat) %>%
  summarise(Novelty = mean(Novelty)) 

### Novelty (versus N1)

sitescores %>%
  filter(habitat %in% c("N1")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  mutate(Novelty = N1 - Value) %>%
  group_by(id, habitat) %>%
  summarise(CPN1 = sum(abs(Novelty))) -> cpn1

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

### Novelty (versus N1 and N2 together)

sitescores %>%
  filter(habitat %in% c("N1", "N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  gather(Reference, Centroid, N1:N2) %>%
  arrange(id, Trait, Reference) %>%
  mutate(Novelty = Centroid - Value) %>%
  group_by(id, Trait) %>%
  filter(Novelty == min(Novelty)) %>%
  group_by(id, habitat) %>%
  summarise(Novelty = sum(Novelty)) %>%
  group_by(habitat) %>%
  summarise(Novelty = mean(Novelty)) 

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

### Novelty (versus N1 and N2 together)

sitescores %>%
  filter(habitat %in% c("N1", "N2")) %>%
  group_by(habitat, Trait) %>%
  summarise(Value = mean(Value)) %>%
  spread(habitat, Value) %>%
  merge(sitescores) %>%
  gather(Reference, Centroid, N1:N2) %>%
  arrange(id, Trait, Reference) %>%
  mutate(Novelty = Centroid - Value) %>%
  group_by(id, Trait) %>%
  filter(Novelty == min(Novelty)) %>%
  group_by(id, habitat) %>%
  summarise(Novelty = sum(Novelty)) %>%
  group_by(habitat) %>%
  summarise(Novelty = mean(Novelty)) 

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
  summarise(CBN2 = sum(abs(Novelty))) -> cbn2

cpn1 %>%
  merge(cpn2) %>%
  merge(cfn1) %>%
  merge(cfn2) %>%
  merge(cbn1) %>%
  merge(cbn2) %>%
  select(-c(id, habitat)) %>%
  FactoMineR::PCA()