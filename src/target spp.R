library(tidyverse)

read.csv("data/species.csv") %>%
  group_by(id, taxon) %>%
  summarise(cover = sum(cover_percent)) -> # add header data
  releves

releves %>%
  group_by(id) %>%
  arrange(-cover, taxon, .by_group = TRUE) %>%
  mutate(totalcover = sum(cover)) %>%
  mutate(relcover = cover/totalcover) %>%
  mutate(cumcover = cumsum(relcover)) -> relcovers

read.csv("../#data/tpl/results/TPLNames.csv", fileEncoding = "latin1") %>%
  select(Taxon, Family) %>%
  rename(taxon = Taxon) -> families

read.csv("../#data/baskin/results/dormancy.csv", fileEncoding = "latin1") %>%
  select(TPLName, Dormancy) %>%
  rename(taxon = TPLName) -> dormancy


relcovers %>% filter(cumcover > 0.80) %>%
  filter(cumcover == min(cumcover)) %>% select(id, cumcover) %>%
  rename(threshold = cumcover) %>%
  merge(relcovers) %>%
  filter(cumcover <= threshold) %>%
  select(id, taxon, cover) %>%
  group_by(taxon) %>%
  summarise(F = length(taxon), A = mean(cover)) %>%
  merge(families,all.x = TRUE) %>% 
  merge(dormancy, all.x = TRUE) %>% 
  write.csv("results/targetspp.csv", row.names = F)
