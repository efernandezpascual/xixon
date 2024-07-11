library(tidyverse)

c("#FFA500",  
  "yellow", 
  "#B3EE3A", 
  "#40E0D0",
  "#5CACEE", 
  "#A020F0",
  "#551A8B") -> colores

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/species.csv", fileEncoding = "latin1") %>% group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # merge layers
  species
read.csv("data/fungi-species.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria-species.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/traits.csv", fileEncoding = "latin1") -> traits
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

merge(soils, species) %>%
  filter(Pb > 150) %>%
  merge(traits) %>%
  filter(origin == "Native") %>%
  group_by(taxon) %>%
  summarise(tolerance = weighted.mean(Pb, cover)) %>%
  arrange(-tolerance) %>%
  data.frame %>%
  pull(taxon) -> tolerantspp

species %>%
  filter(taxon %in% tolerantspp) %>%
  group_by(taxon) %>%
  summarise(n = length(taxon), cover = mean(cover)) %>%
  filter(n > 5) %>%
  arrange(-cover) %>%
  data.frame
