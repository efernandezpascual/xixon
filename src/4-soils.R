library(tidyverse); library(lubridate)
Sys.setlocale("LC_TIME", "English")

c("forestgreen",  
  "gold", 
  "#40E0D0",
  "#A020F0",
  "#5CACEE",
  "#551A8B") -> colores

### Reads and format GP5WSHELL csv export files

read_gp5 <- function(x)
{
  require(tidyverse)
  read.csv(x, skip = 2, header = TRUE) %>% # Rows to skip depend on GP5WShell version!!
    dplyr::rename(temperature = X.1.oC) %>%
    mutate(time = as.POSIXct(Time, 
                             format =" %d.%m.%Y %H:%M:%S", 
                             tz = "UTC"),
           logger = gsub("\\_.*", "" , x)) %>%
    dplyr::select(logger, time, temperature)
}

read.table("data/temperature/Loggers.txt", sep = "\t", header = TRUE, fileEncoding = "latin1") %>%
  mutate(Installed = as.POSIXct(Installed, 
                                format ="%d/%m/%Y", 
                                tz = "UTC")) %>%
  rename(habitat = N.type) %>%
  rename(logger = Logger) %>%
  filter(plot %in% c("Xixon_24", # Bosque mixto en Porceyu con sotobosque de Laurus
                     "Xixon_07", # Detrás del cementeriu de Xove
                     "Xixon_25", # Parque del Lauredal
                     "Xixon_08", # Parcela en La Calzada cerca de los feriantes
                     "Xixon_15", # Vías del tren en La Calzada
                     "Xixon_21")) -> # Estrella de Qatar
  loggers

## Read and bind all loggers, merge with logger header data

wd <- getwd()
setwd(paste(wd, "/data/temperature", sep = ""))
do.call(rbind, lapply(list.files(pattern = "*.csv"), read_gp5)) %>% 
  merge(loggers, by = "logger") %>%
  na.omit %>% 
  filter(time > (Installed + 60*60*24*7)) %>% # Remove records up to one week after installation
  filter(time >= "2024-01-01 01:00:00 UTC") %>%
  filter(time < "2025-01-01 01:00:00 UTC") %>%
  select(ID, plot, habitat, time, temperature) %>%
  arrange(habitat, time) ->
  logs
setwd(wd)
head(logs, n = 25)
tail(logs, n = 25)

logs %>% 
  group_by(time = lubridate::floor_date(time, "day"), habitat) %>%
  summarise(temperature = mean(temperature)) %>%
  mutate(habitat = fct_relevel(habitat, "N1", "N2", "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "N1", "Meadows" = "N2",
                              "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) -> daily

logs %>% pull(time) %>% min
logs %>% pull(time) %>% max

daily %>% pull(time) %>% min
daily %>% pull(time) %>% max

logs %>%
  mutate(habitat = fct_relevel(habitat, "N1", "N2", "N3", "N4B", "N4A", "N4C")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "N1", "Meadows" = "N2",
                              "Parks" = "N3", "Roadsides" = "N4B",
                              "Residential" = "N4A", "Industrial" = "N4C")) %>%
  ggplot(aes(time, temperature, color = habitat)) + 
  facet_wrap( ~ habitat, nrow = 1) +
  geom_line(linewidth = .25, alpha = 0.4) +
  geom_line(data = daily, linewidth = .25) +
  scale_color_manual(values = colores) + 
  ggthemes::theme_tufte() +
  xlab("Urban habitats") + ylab("Soil temperature (ºC)") +
  ggtitle(label = "(A) Soil temperatures") +
  scale_fill_manual(values = c( 
    "forestgreen",  
    "gold", 
    "#40E0D0",
    "#A020F0",
    "#5CACEE",
    "#551A8B")) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 10, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1,0.1,0.1,0.5), "cm")) +
  geom_hline(yintercept = 0, linetype = "dashed") -> F2A; F2A

### Soil bioclimatic indices

logs %>%
  mutate(time = as.POSIXct(time, tz = "UTC")) %>% 
  filter(time >= "2024-01-01 01:00:00") %>%
  filter(time <= "2024-12-31 23:00:00") %>%
  group_by(plot, habitat, Day = lubridate::floor_date(time, "day")) %>%
  summarise(T = mean(temperature), X = max(temperature), N = min(temperature), n = length(time)) %>% # Daily mean, max, min
  group_by(plot, habitat, Month = lubridate::floor_date(Day, "month")) %>%
  summarise(T = mean(T), X = mean(X), N = mean(N)) %>% 
  group_by(plot, habitat, Year = lubridate::floor_date(Month, "year")) %>%
  summarise(bio1 = mean(T), # Annual Mean Temperature
            bio2 = mean(X - N), # Mean Diurnal Range (Mean of monthly (max temp - min temp))
            bio7 = max(X) - min(N)) # Temperature Annual Range (BIO5-BIO6)) 

### Soil physicochemical sampling

read.csv("data/header.csv", fileEncoding = "latin1") -> header
read.csv("data/soils.csv", fileEncoding = "latin1") -> soils

### PCA soils

header %>% select(id, habitat) %>% merge(soils) -> datos

datos %>% select(-c(id, habitat)) %>% FactoMineR::PCA(graph = FALSE) -> pca1

pca1$eig
pca1$var$contrib %>% data.frame %>% select(Dim.1) %>% arrange(-Dim.1) %>% head(6)
pca1$var$contrib %>% data.frame %>% select(Dim.2) %>% arrange(-Dim.2) %>% head(5)
pca1$var$contrib %>% data.frame %>% select(Dim.3) %>% arrange(-Dim.3) %>% head(5)
pca1$var$cor %>% data.frame %>% select(Dim.3) %>% arrange(-Dim.3) %>% head(5)

### PC1 vs. PC2

cbind((datos %>%  dplyr::select(id, habitat)), data.frame(pca1$ind$coord[, 1:2])) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial")) -> pcaInds

pca1$var$coord[, 1:2] %>% data.frame %>% rownames_to_column(var = "Variable") %>%
  mutate(Dim.1 = ifelse(Variable == "Pb", Dim.1 + .05, Dim.1)) -> pcaVars

ggplot(pcaInds, aes(x = Dim.1, y = Dim.2)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 6*Dim.1, yend = 6*Dim.2)) +
  geom_point(aes(fill = habitat), color = "black", size = 5, shape = 21) +
  geom_label(data = pcaVars, aes(x = 6*Dim.1, y = 6*Dim.2, label = Variable),  show.legend = FALSE, size = 3) +
  ggthemes::theme_tufte() + 
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "bottom", 
        #legend.direction = "vertical",
        legend.title = element_blank(),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 2 (", round(pca1$eig[2, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  guides(fill = guide_legend(nrow = 1)) -> FS1a; FS1a

ggsave(FS1a, file = "results/supplementary/FigS2a.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 180, units = "mm", dpi = 600)

### PC1 vs. PC3

cbind((datos %>%  dplyr::select(id, habitat)), data.frame(pca1$ind$coord[, 1:3])) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial")) -> pcaInds

pca1$var$coord[, 1:3] %>% data.frame %>% rownames_to_column(var = "Variable") %>%
  mutate(Dim.1 = ifelse(Variable == "Pb", Dim.1 + .05, Dim.1)) -> pcaVars

ggplot(pcaInds, aes(x = Dim.1, y = Dim.3)) +
  coord_fixed() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_segment(data = pcaVars, aes(x = 0, y = 0, xend = 6*Dim.1, yend = 6*Dim.3)) +
  geom_point(aes(fill = habitat), color = "black", size = 5, shape = 21) +
  geom_label(data = pcaVars, aes(x = 6*Dim.1, y = 6*Dim.3, label = Variable),  show.legend = FALSE, size = 3) +
  ggthemes::theme_tufte() + 
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "bottom", 
        #legend.direction = "vertical",
        legend.title = element_blank(),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_text(size = 10),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1, 0.1, 0.1, 0.1), "cm")) +
  scale_x_continuous(name = paste("Axis 1 (", round(pca1$eig[1, 2], 0),
                                  "% variance explained)", sep = "")) + 
  scale_y_continuous(name = paste("Axis 3 (", round(pca1$eig[3, 2], 0), 
                                  "% variance explained)", sep = "")) +
  scale_shape_manual(values = c(21, 24)) +
  scale_color_manual(values = colores) +
  scale_fill_manual(values = colores) +
  guides(fill = guide_legend(nrow = 1)) -> FS1b; FS1b

ggsave(FS1b, file = "results/supplementary/FigS2b.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 180, units = "mm", dpi = 600)

### Geographic patterns

header %>% 
  select(id, longitude, latitude) %>% 
  merge(soils) %>%
  select(longitude, latitude, Pb, OM, Mg) %>%
  cor

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  merge(soils) %>%
  select(longitude, habitat, latitude, Pb, OM, Mg) %>%
  ggplot(aes(longitude, Pb, color = habitat)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~habitat, scales = "free")

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  filter(habitat == "forest") %>%
  merge(soils) %>%
  select(longitude, Pb) %>%
  cor

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  filter(habitat == "industrial") %>%
  merge(soils) %>%
  select(longitude, Pb) %>%
  cor

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  filter(habitat == "meadow") %>%
  merge(soils) %>%
  select(longitude, Pb) %>%
  cor

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  filter(habitat == "park") %>%
  merge(soils) %>%
  select(longitude, Pb) %>%
  cor

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  filter(habitat == "residential") %>%
  merge(soils) %>%
  select(longitude, Pb) %>%
  cor

header %>% 
  select(id, habitat, longitude, latitude) %>% 
  filter(habitat == "roadside") %>%
  merge(soils) %>%
  select(longitude, Pb) %>%
  cor


### Correlation

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

datos %>%
  select(-c(id, habitat)) %>%
  cor() %>% 
  round(1) %>%
  get_lower_tri() %>%
  reshape2::melt(na.rm = TRUE) %>%
  # data.frame %>%
  # rownames_to_column(var = "var1") %>%
  # gather(var2, value, -var1) %>%
  # na.omit %>%
  # mutate(value = round(value, 2)) %>%
  ggplot(aes(Var2, Var1, fill = value)) +
  geom_tile() +
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "darkblue", high = "indianred3", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  ggthemes::theme_tufte() +
  geom_text(aes(label = value), size = 3) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "right", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 9.5, hjust = 0, margin = margin(l = 0, b = 4)),
        #strip.text = element_blank(),
        plot.title = element_text(size = 10),
        axis.title = element_blank(),
        #axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm")) +
  coord_fixed() -> FS1c; FS1c

ggsave(FS1c, file = "results/supplementary/FigS2c.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 180, units = "mm", dpi = 600)

### Soil boxplots

datos %>%
  select(id, habitat, Pb, Mg, OM) %>%
  gather(Trait, Value, -c(id, habitat)) %>%
  mutate(Trait = fct_relevel(Trait, "Pb", "OM", "Mg")) %>%
  mutate(Trait = fct_recode(Trait, "Soil Pb (mg / kg of soil)" = "Pb",
                            "Soil organic matter (% P/P)" = "OM",
                            "Soil Mg (mg / kg of soil)" = "Mg")) %>%
  mutate(habitat = fct_relevel(habitat, "forest", "meadow", "park", "roadside", "residential", "industrial")) %>%
  mutate(habitat = fct_recode(habitat, "Forests" = "forest", "Meadows" = "meadow",
                              "Parks" = "park", "Roadsides" = "roadside",
                              "Residential" = "residential", "Industrial" = "industrial")) %>%
  ggplot(aes(habitat, Value, fill = habitat)) +
  geom_boxplot() +
  facet_wrap( ~ Trait, nrow = 1, scales = "free_y", strip.position = "left") +
  ggthemes::theme_tufte() +
  xlab("Urban habitats") + ylab("Value") +
  ggtitle(label = "(B) Soil physicochemical properties") + 
  scale_fill_manual(values = c( 
    "forestgreen",  
    "gold", 
    "#40E0D0",
    "#A020F0",
    "#5CACEE",
    "#551A8B")) +
  theme(text = element_text(family = "sans"),
        strip.background = element_blank(),
        legend.position = "none", 
        #legend.direction = "vertical",
        legend.title = element_text(size = 10),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"),
        legend.text = element_text(size = 10), 
        panel.background = element_rect(color = "black", fill = NULL),
        strip.text = element_text(size = 10, margin = margin(r = 1, l = 5, b = 0)),
        strip.placement = "outside",
        #strip.text = element_blank(),
        plot.title = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 1, hjust=1),
        axis.text.y = element_text(size = 10, color = "black"),
        plot.margin = unit(c(.5,0.1,0.1,0.1), "cm")) +
  geom_hline(yintercept = 0, linetype = "dashed") -> F2B; F2B

cowplot::plot_grid(F2A, F2B, nrow = 2) -> F2

ggsave(F2, file = "results/figures/Fig2.png", bg = "white", 
       path = NULL, scale = 1, width = 180, height = 2*75, units = "mm", dpi = 600)

### Permanova

datos %>% select(-c(id, habitat)) -> df1
RVAideMemoire::pairwise.perm.manova(dist(df1, "euclidian"), datos$habitat, nperm = 100000, p.method = "holm") -> pairperma
vegan::adonis2(df1 ~ habitat, data = datos, permutations = 100000) -> perma
anova(betadisper(vegdist(df1, method = "euclidean"), df1b$Alliance)) -> betadis