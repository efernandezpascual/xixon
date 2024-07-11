library(tidyverse); library(lubridate)

c("#FFA500",  
  "yellow", 
  "#B3EE3A", 
  "#40E0D0",
  "#5CACEE", 
  "#A020F0",
  "#551A8B") -> colores

## Function reads and formats GP5WSHELL csv export files

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

## Read logger header data

read.table("data/temperature/Loggers.txt", sep = "\t", header = TRUE, fileEncoding = "latin1") %>%
  mutate(Installed = as.POSIXct(Installed, 
                                format ="%d/%m/%Y", 
                                tz = "UTC")) %>%
  rename(habitat = N.type) %>%
  rename(logger = Logger) -> header

## Read and bind all loggers, merge with logger header data

wd <- getwd()
setwd(paste(wd, "/data/temperature", sep = ""))
do.call(rbind, lapply(list.files(pattern = "*.csv"), read_gp5)) %>% 
  merge(header, by = "logger") %>%
  na.omit %>% 
  filter(time > (Installed + 60*60*24*7)) %>% # Remove records up to one week after installation
  select(ID, plot, habitat, time, temperature) ->
  logs
setwd(wd)

logs %>%
  ggplot(aes(time, temperature, color = habitat)) + 
  facet_wrap(habitat ~ plot) +
  geom_line() +
  scale_color_manual(values = colores) + 
  coord_cartesian(ylim = c(0, 30))
