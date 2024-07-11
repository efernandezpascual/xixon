read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

header %>%
  select(id, habitat) %>%
  merge(tpn) %>%
  merge(tfn) %>%
  merge(tbn)  %>%
  merge(cpn1) %>%
  merge(cpn2) %>%
  merge(cfn1) %>%
  merge(cfn2) %>%
  merge(cbn1) %>%
  merge(cbn2) %>%
  merge(soils) %>%
  select(-c(habitat, id)) %>%
  FactoMineR::PCA()
  

header %>%
  select(id, habitat) %>%
  merge(tpn) %>%
  merge(tfn) %>%
  merge(tbn)  %>%
  merge(cpn1) %>%
  merge(cfn1) %>%
  merge(cbn1) %>%
  merge(soils) %>%
  select(-c(habitat, id)) %>%
  FactoMineR::PCA()
