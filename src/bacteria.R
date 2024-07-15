library(tidyverse)

read.csv("data/bacteria-original.csv", sep = "\t", 
         #fileEncoding = "latin1", 
         skip = 1) %>%
  separate(taxonomy, into = c("kingdom", "phylum", "class", "order", "family", "genus", "species"), sep = "; ") %>%
  mutate(kingdom = gsub("d__", "", kingdom), 
         phylum = gsub("p__", "", phylum), 
         class = gsub("c__", "", class), 
         order = gsub("o__", "", order), 
         family = gsub("f__", "", family), 
         genus = gsub("g__", "", genus), 
         species = gsub("s__", "", species), 
         species = gsub("_", " ", species)) %>%
  rename(ASV = X.OTU.ID,
         Xixon01 = X1.16S,
         Xixon02 = X2.16S,
         Xixon03 = X3.16S,
         Xixon04 = X4.16S,
         Xixon05 = X5.16S,
         Xixon06 = X6.16S,
         Xixon07 = X7.16S,
         Xixon08 = X8.16S,
         Xixon09 = X9.16S,
         Xixon10 = X10.16S,
         Xixon11 = X11.16S,
         Xixon12 = X12.16S,
         Xixon13 = X13.16S,
         Xixon14 = X14.16S,
         Xixon15 = X15.16S,
         Xixon16 = X16.16S,
         Xixon17 = X17.16S,
         Xixon18 = X18.16S,
         Xixon19 = X19.16S,
         Xixon20 = X20.16S,
         Xixon21 = X21.16S,
         Xixon22 = X22.16S,
         Xixon23 = X23FD.16S,
         Xixon24 = X24.16S,
         Xixon25 = X25.16S,
         Xixon26 = X26.16S,
         Xixon27 = X27.16S,
         Xixon28 = X28.16S,
         Xixon29 = X29.16S,
         Xixon30 = X30.16S) -> bacteria

### numbers

bacteria %>% group_by(ASV)

bacteria %>% filter(species != "unidentified" & !is.na(species)) %>% group_by(ASV)
bacteria %>% filter(species != "unidentified" & !is.na(species)) %>% group_by(ASV) %>% pull(species) %>% unique %>% length

bacteria %>% filter(species == "unidentified" | is.na(species)) %>% 
  filter(genus != "unidentified" & !is.na(genus)) %>% group_by(ASV)
bacteria %>% filter(species == "unidentified" | is.na(species)) %>% 
  filter(genus != "unidentified" & !is.na(genus)) %>% group_by(ASV) %>%
  pull(genus) %>% unique %>% length

bacteria %>% 
  filter(species != "unidentified" & !is.na(species)) %>%
  select(species, Xixon01:Xixon09) %>%
  gather(id, Value, -species) %>%
  filter(Value != 0) %>%
  select(species, id) %>%
  unique %>%
  group_by(species) %>%
  tally() %>%
  arrange(-n)%>%
  print(n = 100)

bacteria %>% 
  filter(genus != "unidentified" & !is.na(genus)) %>%
  select(genus, Xixon01:Xixon09) %>%
  gather(id, Value, -genus) %>%
  filter(Value != 0) %>%
  select(genus, id) %>%
  unique %>%
  group_by(genus) %>%
  tally() %>%
  arrange(-n) %>%
  print(n = 100)

bacteria %>% 
  filter(family != "unidentified" & !is.na(family)) %>%
  select(family, Xixon01:Xixon09) %>%
  gather(id, Value, -family) %>%
  filter(Value != 0) %>%
  select(family, id) %>%
  unique %>%
  group_by(family) %>%
  tally() %>%
  arrange(-n) %>%
  print(n = 100)

bacteria %>% filter(phylum != "unidentified" & !is.na(phylum)) %>% group_by(ASV)
bacteria %>% filter(phylum != "unidentified" & !is.na(phylum)) %>% group_by(ASV) %>% pull(phylum) %>% unique 
bacteria %>% group_by(phylum) %>% tally %>% mutate(n = n/16306) %>% arrange(-n)

bacteria %>% 
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
  spread(habitat, n) %>%
  filter(phylum %in% c("Proteobacteria", "Acidobacteriota", "Actinobacteriota", "Planctomycetota"))

bacteria %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  pull(n) %>% mean

bacteria %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  pull(n) %>% max

bacteria %>%
  select(ASV, Xixon01:Xixon09) %>%
  gather(id, Value, -ASV) %>%
  filter(Value != 0) %>%
  merge(header) %>%
  group_by(id, habitat) %>%
  summarise(n = length(ASV)) %>%
  pull(n) %>% min

bacteria %>%
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

bacteria %>%
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

### Save bacteria

bacteria %>%
  gather(id, value, Xixon01:Xixon09) %>%
  filter(value != 0) %>%
  mutate(presence = 1) %>%
  write.csv("data/bacteria.csv", row.names = FALSE, fileEncoding = "latin1")
