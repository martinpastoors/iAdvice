# --------------------------------------------------------------------------------------------
# combine FAO and IHO areas.r
#
# 19/12/2018 based on SFrame code
# --------------------------------------------------------------------------------------------

# devtools::install_github("fishvice/tidyices", dependencies = FALSE)

# Note: geom_sf is only on the development version of ggplot2. Make sure to start rstudio in administrator mode before starting the command. 
# library(devtools)
# devtools::install_github("tidyverse/ggplot2")

# devtools::install_github("einarhjorleifsson/iceshape", dependencies = FALSE)
library(iceshape)   # ls("package:iceshape")
library(sf)
library(tidyverse)
library(readxl)
library(maps)
library(ggplot2)
library(viridis)

source("../mptools/r/my_utils.r")
# load("../prf/rdata/world.df.RData")

load(file.path(onedrive,"rdata/iho_eez.RData"))

# get dropbox directory
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# source("https://raw.githubusercontent.com/einarhjorleifsson/gisland/master/R/read_sf_ftp.R")
# faolink <- read_csv("/net/www/export/home/ftp/pub/reiknid/einar/shapes/fao-areas_nocoastline.csv")
# fao <- read_sf_ftp("fao-areas_nocoastline")
# eez <- read_sf_ftp("eez")



# ----------------------------------------------------------------------------------------------------------
# set world in sf format
# ----------------------------------------------------------------------------------------------------------

world1 <- sf::st_as_sf(
  map('world', plot = FALSE, fill = TRUE, 
               regions = c("netherlands","belgium","France(?!:Corsica)",
                           "ireland","united kingdom", "UK", "Denmark","Germany",
                           "norway","iceland", "greenland", "faroe islands", "spain","portugal",
                           "sweden", "finland","poland", "luxembourg"))
)


# ----------------------------------------------------------------------------------------------------------
# generate FAO areas as SF object
# ----------------------------------------------------------------------------------------------------------

fao <-
  
  iceshape::faolink %>% 
  
  # link to geometry
  # left_join(fao) %>%
  left_join(iceshape::fao) %>%
  
  # remove missing geometries
  filter(!is.na(geometry)) %>% 
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() 

# ----------------------------------------------------------------------------------------------------------
# generate IHO & EEZ areas as SF object
# ----------------------------------------------------------------------------------------------------------

iho <- 
  sf::st_as_sf(iho_eez) %>% 
  mutate(
    coastalstate = ifelse(tolower(Country) %in% c("united kingdom","ireland", "france", "spain","belgium",
                                                  "denmark","germany","netherlands","portugal", 
                                                  "sweden", "finland","italy","greece"), 
                          "EU", NA),
    coastalstate = ifelse(grepl("Canary|Azores|Madeira", EEZ), 
                          "EU", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("norway"), 
                          "NOR", coastalstate),
    coastalstate = ifelse(tolower(Sovereign) %in% c("norway") & grepl("Jan Mayen", MarRegion), 
                          "NOR", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("faeroe islands"), 
                          "FAR", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("russia"), 
                          "RUS", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("iceland"), 
                          "ICE", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("greenland"), 
                          "GRL", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("morocco","western sahara"), 
                          "MAR", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("mauritania"), 
                          "MAU", coastalstate),
    coastalstate = ifelse(tolower(Country) %in% c("canada"), 
                          "CAN", coastalstate),
    coastalstate = ifelse(grepl("High seas of the North Atlantic Ocean", MarRegion), 
                          "IHO", coastalstate),
    coastalstate = ifelse(grepl("High seas of the Norwegian Sea", MarRegion), 
                          "IHO", coastalstate),
    coastalstate = ifelse(grepl("High seas of the Greenland Sea", MarRegion), 
                          "IHO", coastalstate)
       
  ) %>% 
  filter(!is.na(coastalstate) )  %>% 
  st_sf() %>% 
  
  # join areas together by coastal state (or IHO)
  group_by(coastalstate) %>%
  summarise() 

# Join the stuff and first only keep the ISO_geometry stuff; then add the FAO areas again
fao_iho <-
  st_intersection(fao, iho) %>%
  
  # generate a new variable, the ices name and then country iso
  mutate(variable = paste(name, coastalstate, sep="_")) %>% 
  
  distinct(unit, variable, geometry, .keep_all = FALSE) %>% 
  rename(name = variable) %>% 
  
  rbind(., fao) 

# save file on dropbox
save(fao_iho, file=file.path(dropboxdir,"rdata/fao_iho.RData"))

# testing with plaice TAC areas
t <- 
  data.frame(
    tac  = c("PLE/2A3AX4", "PLE/03AN."),
    area = c("27.2.a.2_EU;27.4.a;27.4.b;27.4.c", "27.3.a.20"),
    stringsAsFactors = FALSE
  ) %>% 
  
  # make into long list
  separate(area, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=name, x1:x50) %>%
  drop_na() %>%
  select(-dummy) %>%
  
  # left_join the area geometries
  left_join(fao_iho, by="name") %>% 
  st_sf() %>% 
  
  # summarize by tac 
  group_by(tac) %>% 
  summarise() 

w <-
  st_intersection(world1, st_as_sfc(st_bbox(t)))

t %>% 
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank()) +
  
  theme(legend.title=element_blank()) +
  geom_sf(data=w, inherit.aes = FALSE, fill="gray90") +
  geom_sf(aes(fill = factor(tac)), alpha=0.4) +
  ggmisc::scale_fill_crayola() 

