library(tidyverse)

read.csv("data/bacteria/Table3/table_after_filtering.csv", sep = "\t", 
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

bacteria %>% filter(is.na(kingdom))
bacteria %>% filter(is.na(phylum))
bacteria %>% filter(is.na(class))
bacteria %>% filter(is.na(order))
bacteria %>% filter(is.na(family))
bacteria %>% filter(is.na(genus))

bacteria %>% pull(kingdom) %>% na.omit %>% unique -> kingdoms 
bacteria %>% pull(phylum) %>% na.omit %>% unique -> phyla 
bacteria %>% pull(class) %>% na.omit %>% unique -> classes 
bacteria %>% pull(order) %>% na.omit %>% unique -> orders 
bacteria %>% pull(family) %>% na.omit %>% unique -> families 
bacteria %>% pull(genus) %>% na.omit %>% unique -> genera  
bacteria %>% select(species) %>% filter(species != "unidentified") %>% arrange(species) %>% na.omit %>% unique -> spp  

bacteria %>%
  select(kingdom:species) %>%
  unique %>%
  arrange(kingdom, phylum, class, order, family, genus, species) %>%
  write.csv("results/bacteria-taxa.csv", row.names = FALSE, fileEncoding = "latin1")

bacteria %>%
  select(ASV, species, Xixon01:Xixon09) %>%
  gather(id, cover, Xixon01:Xixon09) %>%
  group_by(species, id) %>%
  summarise(cover = sum(cover)) %>%
  na.omit %>%
  filter(species != "unidentified") %>%
  filter(cover != 0) %>%
  rename(taxon = species) %>%
  write.csv("data/bacteria-species.csv", row.names = FALSE, fileEncoding = "latin1")
