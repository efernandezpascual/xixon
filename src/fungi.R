library(tidyverse)

read.csv("data/fungi/Table3/table_after_filtering.csv", sep = "\t", 
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

fungi %>% filter(is.na(kingdom))
fungi %>% filter(is.na(phylum))
fungi %>% filter(is.na(class))
fungi %>% filter(is.na(order))
fungi %>% filter(is.na(family))
fungi %>% filter(is.na(genus))

fungi %>% pull(kingdom) %>% na.omit %>% unique -> kingdoms 
fungi %>% pull(phylum) %>% na.omit %>% unique -> phyla 
fungi %>% pull(class) %>% na.omit %>% unique -> classes 
fungi %>% pull(order) %>% na.omit %>% unique -> orders 
fungi %>% pull(family) %>% na.omit %>% unique -> families 
fungi %>% pull(genus) %>% na.omit %>% unique -> genera  
fungi %>% select(species) %>% filter(species != "unidentified") %>% arrange(species) %>% na.omit %>% unique -> spp  

fungi %>%
  select(kingdom:species) %>%
  unique %>%
  arrange(kingdom, phylum, class, order, family, genus, species) %>%
  write.csv("results/fungi-taxa.csv", row.names = FALSE, fileEncoding = "latin1")

fungi %>% pull(ASV) %>% unique %>% length

fungi %>%
  filter(! is.na(species)) %>%
  filter(species != "unidentified") %>%
  pull(ASV) %>% unique %>% length

fungi %>%
  filter(! is.na(genus)) %>%
  filter(genus != "unidentified") %>%
  pull(ASV) %>% unique %>% length

fungi %>%
  select(ASV, species, Xixon01:Xixon09) %>%
  gather(id, cover, Xixon01:Xixon09) %>%
  group_by(species, id) %>%
  summarise(cover = sum(cover)) %>%
  na.omit %>%
  filter(species != "unidentified") %>%
  filter(cover != 0) %>%
  rename(taxon = species) %>%
  write.csv("data/fungi-species.csv", row.names = FALSE, fileEncoding = "latin1")
