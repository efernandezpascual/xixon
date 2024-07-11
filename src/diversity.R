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

# Aliens

merge(species, traits) %>%
  group_by(id, origin) %>%
  summarise(c = sum(cover)) %>%
  spread(origin, c, fill = 0) %>%
  mutate(aliencover = 100 * (Alien / (Alien + Native))) %>%
  merge(header) %>%
  group_by(habitat) %>%
  summarise(m = mean(aliencover))

merge(species, traits) %>%
  group_by(id, origin) %>%
  summarise(n = length(taxon)) %>%
  spread(origin, n, fill = 0) %>%
  mutate(alienspp = 100 * (Alien / (Alien + Native))) %>%
  merge(header) %>%
  group_by(habitat) %>%
  summarise(nalien = mean(alienspp))

merge(species, traits) %>%
  merge(header) %>%
  select(habitat, taxon, origin) %>%
  unique %>%
  group_by(habitat, origin) %>%
  tally %>%
  spread(origin, n, fill = 0) %>%
  mutate(aliens = 100 * (Alien / (Alien + Native))) -> alienspp

# Alpha

species %>% group_by(id) %>% summarise(plantR = length(taxon)) %>% data.frame -> r1
fungi %>% group_by(id) %>% summarise(fungiR = length(taxon)) %>% data.frame-> r2
bacteria %>% group_by(id) %>% summarise(bacteriaR = length(taxon)) %>% data.frame -> r3

# Gamma

species %>% merge(header) %>% select(habitat, taxon) %>% unique %>% group_by(habitat) %>% summarise(plantG = length(taxon)) %>% data.frame -> g1
fungi %>% merge(header) %>% select(habitat, taxon) %>% unique %>% group_by(habitat) %>% summarise(fungiG = length(taxon)) %>% data.frame -> g2
bacteria %>% merge(header) %>% select(habitat, taxon) %>% unique %>% group_by(habitat) %>% summarise(bacteriaG = length(taxon)) %>% data.frame -> g3

header %>%
  merge(r1) %>%
  merge(r2) %>%
  merge(r3) %>%
  merge(soils)  %>%
  select(habitat,
         Pb,
         OM,
         Mg,
         plantR,
         fungiR,
         bacteriaR) %>%
  gather(Trait, Value, Pb:bacteriaR) %>%
  na.omit %>%
  group_by(habitat, Trait) %>%
  summarise(m = mean(Value)) %>%
  spread(Trait, m) %>%
  merge(g1) %>%
  merge(g2) %>%
  merge(g3) %>%
  merge(alienspp) %>% 
  select(-c(plantR, bacteriaR, fungiR, plantG, aliens, Mg)) %>%
  gather(trait, value, OM:Native) %>%
  mutate(trait = fct_relevel(trait, "bacteriaG", "fungiG", "Native", 
                             "Alien", "Pb", "OM")) %>%
  mutate(trait = fct_recode(trait,
                            "Plomo en el suelo" = "Pb",
                            "Magnesio" = "Mg",
                            "Materia orgánica en el suelo" = "OM",
                            "% de plantas invasoras" = "aliens",
                            "Plantas alóctonas" = "Alien",
                            "Plantas nativas" = "Native",
                            "Hongos" = "fungiG",
                            "Bacterias" = "bacteriaG")) %>%
  mutate(habitat = fct_recode(habitat,
                              "N1 - Bosques urbanos" = "N1",
                              "N2 - Prados de siega" = "N2",
                              "N3 - Parques y jardines" = "N3",
                              "N4 - Solares residenciales" = "N4A",
                              "N4 - Cunetas" = "N4B",
                              "N4 - Solares industriales" = "N4C")) -> resumen

resumen  %>%
  filter(!trait %in% c("Plomo en el suelo", "Materia orgánica en el suelo")) %>%
  ggplot(aes(habitat, value, fill = habitat)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ trait, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = colores) + 
  ggthemes::theme_tufte() +
  ggtitle(lab = "Número de especies por hábitat") +
  theme(text = element_text(family = "sans"),
        legend.position = "top", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_blank(),
        strip.text = element_text(size = 18, color = "black"),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_blank()) -> F5; F5

ggsave(F5, file = "results/Barplots.png", bg = "white", 
       path = NULL, scale = 1, width = 300, height = 150, units = "mm", dpi = 600)

resumen  %>%
  filter(trait %in% c("Plomo en el suelo")) %>%
  ggplot(aes(habitat, value, fill = habitat)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  facet_wrap(~ trait, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = colores) + 
  ggthemes::theme_tufte() +
  ggtitle(lab = "Plomo en el suelo por hábitat") +
  theme(text = element_text(family = "sans"),
        legend.position = "right", legend.box = "vertical", legend.margin = margin(),
        legend.title = element_blank(),
        title = element_text(size = 18, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        panel.background = element_rect(color = "black", fill = NULL),
        axis.title = element_blank(),
        strip.text = element_blank(),
        axis.text = element_text(size = 18, color = "black"),
        axis.text.x = element_blank()) -> F6; F6

ggsave(F6, file = "results/Barplots2.png", bg = "white", 
       path = NULL, scale = 1, width = 200, height = 100, units = "mm", dpi = 600)
