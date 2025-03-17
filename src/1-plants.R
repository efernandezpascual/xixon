library(tidyverse); library(indicspecies); library(labdsv)

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/plants.csv", fileEncoding = "latin1") -> plants

### Plant numbers

plants %>% group_by(taxon)
plants %>% group_by(id) %>% summarise(n = length(taxon)) %>% pull(n) %>% mean
plants %>% group_by(id) %>% summarise(n = length(taxon)) %>% pull(n) %>% min
plants %>% group_by(id) %>% summarise(n = length(taxon)) %>% pull(n) %>% max
plants %>% group_by(id) %>% summarise(n = length(taxon)) %>% merge(header) %>% group_by(habitat) %>% summarise(n = round(mean(n), 0)) %>% arrange(n)
plants %>% group_by(taxon) %>% tally() %>% arrange(-n)

### Permanova

header %>%
  select(id, habitat) %>%
  merge(plants) %>%
  spread(taxon, cover, fill = 0) -> datos

datos %>% select(-c(id, habitat)) -> df1

RVAideMemoire::pairwise.perm.manova(dist(df1, "euclidian"), datos$habitat, nperm = 1000000, p.method = "holm") -> pairperma
vegan::adonis2(df1 ~ habitat, data = datos, permutations = 100000) -> perma
anova(betadisper(vegdist(df1, method = "euclidean"), df1b$Alliance)) -> betadis
