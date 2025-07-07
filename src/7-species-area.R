library(tidyverse); library(ggvenn); library(vegan)

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/plants.csv", fileEncoding = "latin1") -> plants
read.csv("data/fungi.csv", fileEncoding = "latin1") -> fungi
read.csv("data/bacteria.csv", fileEncoding = "latin1") -> bacteria
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils
read.csv("data/soil-pca.csv", fileEncoding = "latin1") -> pca


header %>%
  select(habitat, id) %>%
  merge(plants) %>%
  select(id, taxon, cover) %>%
  mutate(cover = 1) %>% # Transform cover to presence
  spread(taxon, cover, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

library(BiodiversityR)

Condit.spec <- matrix

Condit.2 <- header
site.rows <- as.character(Condit.2[, 1])
Condit.env <- Condit.2[, 2:7]
rownames(Condit.env) <- site.rows
Condit.env$Age.cat <- factor(Condit.env$habitat)
Condit.env$Area <- rep(16, nrow(Condit.env))

check.datasets(Condit.spec, Condit.env)

Condit.env <- Condit.env[complete.cases(Condit.env), ]
Condit.spec <- same.sites(Condit.spec, Condit.env)
check.datasets(Condit.spec, Condit.env)


Accum.2 <- accumcomp(Condit.spec, y=Condit.env, factor='Age.cat', scale='Area',
                     method='exact', conditioned=FALSE, plotit=FALSE)
Accum.2

accum.long2 <- accumcomp.long(Accum.2, ci=NA, label.freq=1)
head(accum.long2)

plotgg2 <- ggplot(data=accum.long2, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point(data=subset(accum.long2, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=5) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, alpha=0.3))), 
              show.legend=FALSE) + 
  labs(x = expression("m"^2), y = "Species", colour = "Age", shape = "Age") +
  ggtitle(lab = "plants")


plotgg2


ggsave(plotgg2, file = "results/figures/plotgg2.png", bg = "white", 
       path = NULL, scale = 1, units = "mm", dpi = 600)





fungi %>%
  select(id, ASV, presence) %>%
  spread(ASV, presence, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

library(BiodiversityR)

Condit.spec <- matrix

Condit.2 <- header
site.rows <- as.character(Condit.2[, 1])
Condit.env <- Condit.2[, 2:7]
rownames(Condit.env) <- site.rows
Condit.env$Age.cat <- factor(Condit.env$habitat)
Condit.env$Area <- rep(16, nrow(Condit.env))

check.datasets(Condit.spec, Condit.env)

Condit.env <- Condit.env[complete.cases(Condit.env), ]
Condit.spec <- same.sites(Condit.spec, Condit.env)
check.datasets(Condit.spec, Condit.env)


Accum.2 <- accumcomp(Condit.spec, y=Condit.env, factor='Age.cat', scale='Area',
                     method='exact', conditioned=FALSE, plotit=FALSE)
Accum.2

accum.long2 <- accumcomp.long(Accum.2, ci=NA, label.freq=1)
head(accum.long2)

plotgg2 <- ggplot(data=accum.long2, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point(data=subset(accum.long2, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=5) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, alpha=0.3))), 
              show.legend=FALSE) + 
  labs(x = expression("m"^2), y = "Species", colour = "Age", shape = "Age") +
  ggtitle(lab = "fungi")


plotgg2


ggsave(plotgg2, file = "results/figures/plotgg2-fungi.png", bg = "white", 
       path = NULL, scale = 1, units = "mm", dpi = 600)





bacteria %>%
  select(id, ASV, presence) %>%
  spread(ASV, presence, fill = 0) %>%
  column_to_rownames(var = "id") -> matrix 

library(BiodiversityR)

Condit.spec <- matrix

Condit.2 <- header
site.rows <- as.character(Condit.2[, 1])
Condit.env <- Condit.2[, 2:7]
rownames(Condit.env) <- site.rows
Condit.env$Age.cat <- factor(Condit.env$habitat)
Condit.env$Area <- rep(16, nrow(Condit.env))

check.datasets(Condit.spec, Condit.env)

Condit.env <- Condit.env[complete.cases(Condit.env), ]
Condit.spec <- same.sites(Condit.spec, Condit.env)
check.datasets(Condit.spec, Condit.env)


Accum.2 <- accumcomp(Condit.spec, y=Condit.env, factor='Age.cat', scale='Area',
                     method='exact', conditioned=FALSE, plotit=FALSE)
Accum.2

accum.long2 <- accumcomp.long(Accum.2, ci=NA, label.freq=1)
head(accum.long2)

plotgg2 <- ggplot(data=accum.long2, aes(x = Sites, y = Richness, ymax = UPR, ymin = LWR)) + 
  scale_x_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  scale_y_continuous(sec.axis = dup_axis(labels=NULL, name=NULL)) +
  geom_line(aes(colour=Grouping), size=2) +
  geom_point(data=subset(accum.long2, labelit==TRUE), 
             aes(colour=Grouping, shape=Grouping), size=5) +
  geom_ribbon(aes(colour=Grouping, fill=after_scale(alpha(colour, alpha=0.3))), 
              show.legend=FALSE) + 
  labs(x = expression("m"^2), y = "Species", colour = "Age", shape = "Age") +
  ggtitle(lab = "bacteria")


plotgg2


ggsave(plotgg2, file = "results/figures/plotgg2-bacteria.png", bg = "white", 
       path = NULL, scale = 1, units = "mm", dpi = 600)
