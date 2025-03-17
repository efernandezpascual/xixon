library(tidyverse)

### Clean fungi

read.csv("data/fungi-original.csv", sep = "\t", 
         #fileEncoding = "latin1", 
         skip = 1) %>%
  separate(taxonomy, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "; ") %>%
  mutate(kingdom = gsub("k__", "", kingdom), 
         phylum = gsub("p__", "", phylum), 
         class = gsub("c__", "", class), 
         order = gsub("o__", "", order), 
         family = gsub("f__", "", family), 
         genus = gsub("g__", "", genus), 
         species = gsub("s__", "", species), 
         species = gsub("_", " ", species)) %>%
  rename(ASV = X.OTU.ID,
         Xixon01 = X1.ITS,
         Xixon02 = X2.ITS,
         Xixon03 = X3.ITS,
         Xixon04 = X4.ITS,
         Xixon05 = X5.ITS,
         Xixon06 = X6.ITS,
         Xixon07 = X7.ITS,
         Xixon08 = X8.ITS,
         Xixon09 = X9.ITS,
         Xixon10 = X10.ITS,
         Xixon11 = X11.ITS,
         Xixon12 = X12.ITS,
         Xixon13 = X13.ITS,
         Xixon14 = X14.ITS,
         Xixon15 = X15.ITS,
         Xixon16 = X16.ITS,
         Xixon17 = X17.ITS,
         Xixon18 = X18.ITS,
         Xixon19 = X19.ITS,
         Xixon20 = X20.ITS,
         Xixon21 = X21.ITS,
         Xixon22 = X22.ITS,
         Xixon23 = X23FD.ITS,
         Xixon24 = X24.ITS,
         Xixon25 = X25.ITS,
         Xixon26 = X26.ITS,
         Xixon27 = X27.ITS,
         Xixon28 = X28.ITS,
         Xixon29 = X29.ITS,
         Xixon30 = X30.ITS) -> fungi

### Fungi numbers

fungi %>% group_by(ASV)

fungi %>% filter(species != "unidentified" & !is.na(species)) %>% group_by(ASV)
fungi %>% filter(species != "unidentified" & !is.na(species)) %>% group_by(ASV) %>% pull(species) %>% unique %>% length

fungi %>% filter(species == "unidentified" | is.na(species)) %>% 
  filter(genus != "unidentified" & !is.na(genus)) %>% group_by(ASV)
fungi %>% filter(species == "unidentified" | is.na(species)) %>% 
  filter(genus != "unidentified" & !is.na(genus)) %>% group_by(ASV) %>%
  pull(genus) %>% unique %>% length

fungi %>% 
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, Xixon01:Xixon09) %>%
  gather(id, Value, -species) %>%
  filter(Value != 0) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)

fungi %>% 
  filter(genus != "unidentified" & !is.na(genus)) %>%
  select(genus, Xixon01:Xixon09) %>%
  gather(id, Value, -genus) %>%
  filter(Value != 0) %>%
  select(genus, id) %>%
  unique %>%
  group_by(genus) %>%
  tally() %>%
  arrange(-n)

fungi %>% filter(phylum != "unidentified" & !is.na(phylum)) %>% group_by(ASV)
fungi %>% filter(phylum != "unidentified" & !is.na(phylum)) %>% group_by(ASV) %>% pull(phylum) %>% unique 
fungi %>% group_by(phylum) %>% tally %>% mutate(n = n/4679) %>% arrange(-n)

fungi %>% 
  filter(phylum != "unidentified" & !is.na(phylum)) %>%
  select(phylum, Xixon01:Xixon09) %>%
  gather(id, Value, -phylum) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(habitat, phylum) %>%
  tally() %>%
  arrange(-n) %>%
  group_by(habitat) %>%
  mutate(n = n/sum(n)) %>%
  spread(habitat, n)

fungi %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  pull(n) %>% mean

fungi %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  pull(n) %>% max

fungi %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  pull(n) %>% min

fungi %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  group_by(habitat) %>% 
  summarise(n = mean(n)) %>%
  arrange(-n)

### ASV matrix

fungi %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  mutate(Value = 1) %>%
  spread(ASV, Value, fill = 0) -> asvmatrix

### Permanova

header %>%
  select(id, habitat) %>%
  merge(asvmatrix) -> datos

datos %>% select(-c(id, habitat)) -> df1

RVAideMemoire::pairwise.perm.manova(dist(df1, "euclidian"), datos$habitat, nperm = 1000000, p.method = "holm") -> pairperma
vegan::adonis2(df1 ~ habitat, data = datos, permutations = 100000) -> perma
anova(betadisper(vegdist(df1, method = "euclidean"), df1b$Alliance)) -> betadis

### Save fungi

fungi %>%
  gather(id, value, Xixon01:Xixon09) %>%
  filter(value != 0) %>%
  mutate(presence = 1) %>%
  write.csv("data/fungi.csv", row.names = FALSE, fileEncoding = "latin1")
  
