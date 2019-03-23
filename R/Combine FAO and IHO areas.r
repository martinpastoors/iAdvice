# --------------------------------------------------------------------------------------------
# combine FAO and IHO areas.r
#
# 19/12/2018 based on SFrame code
# 21/01/2019 now run the code with the original FAO files (includes more levels)
# 22/01/2019 contains both the EEZ_FAO combinations, and the FAO codes as stand alone. 
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

# get dropbox directory
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# ----------------------------------------------------------------------------------------------------------
# set world in sf format
# ----------------------------------------------------------------------------------------------------------

world_sf <- 
  sf::st_as_sf(map('world', 
                   plot    = FALSE, 
                   fill    = TRUE, 
                   regions = c("netherlands","belgium","France(?!:Corsica)",
                               "ireland","united kingdom", "UK", "Denmark","Germany",
                               "norway","iceland", "greenland", "faroe islands", "spain","portugal",
                               "sweden", "finland","poland", "luxembourg")) )

save(world_sf, file=file.path(dropboxdir,"rdata/world_sf.RData"))

# ----------------------------------------------------------------------------------------------------------
# check ICES areas in sf format
# ----------------------------------------------------------------------------------------------------------

ices_sf <-
  get(load(file.path(onedrive,"rdata/ices.RData"))) %>% 
  subset(F_AREA == "27") %>% 
  sf::st_as_sf() %>% 
  select(FID, F_LEVEL, variable=F_CODE, F_STATUS, id, geometry) %>%
  mutate(coastalstate = as.character(NA)) %>% 
  arrange(variable, F_LEVEL)


# ----------------------------------------------------------------------------------------------------------
# set fao in sf format
# ----------------------------------------------------------------------------------------------------------
# load fao (and limit to area 27 for now) and convert to sf
fao_sf <-
  get(load(file.path(onedrive,"rdata/fao.RData"))) %>% 
  subset(F_AREA == "27") %>% 
  sf::st_as_sf() %>% 
  select(FID, F_LEVEL, variable=F_CODE, F_STATUS, id, geometry) %>%
  mutate(coastalstate = as.character(NA)) %>% 
  arrange(variable, F_LEVEL)

save(fao_sf, file=file.path(dropboxdir,"rdata/fao_sf.RData"))

# ----------------------------------------------------------------------------------------------------------
# set iho and eez in sf format
# ----------------------------------------------------------------------------------------------------------

iho_eez_sf <-
  get(load(file.path(onedrive,"rdata/iho_eez.RData"))) %>% 
  sf::st_as_sf() %>% 
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
  sf::st_sf() %>% 
  
  # join areas together by coastal state (or IHO)
  group_by(coastalstate) %>%
  summarise() 
  
save(iho_eez_sf, file=file.path(dropboxdir,"rdata/iho_eez_sf.RData"))

# ----------------------------------------------------------------------------------------------------------
# Intersect the FAO and IHO/EEZ areas. 
# ----------------------------------------------------------------------------------------------------------

intersect_sf <-
  
  st_intersection(fao_sf, iho_eez_sf) %>%
  
  # generate a new variable, the fao code and then country iso
  mutate(variable = paste(F_CODE, coastalstate, sep="_"),
         F_LEVEL  = paste(F_LEVEL, "EEZ", sep="_")) %>% 
  
  dplyr::select(-F_CODE)

# ----------------------------------------------------------------------------------------------------------
# Merge the FAO and IHO/EEZ data frames and then add the geometries. 
# ----------------------------------------------------------------------------------------------------------

fao_eez_sf <-
  
  # this is the way to bind rows for SF objects
  do.call(rbind, list(intersect_sf, fao_sf)) %>% 
  
  arrange(variable)


# ----------------------------------------------------------------------------------------------------------
# save file on dropbox
# ----------------------------------------------------------------------------------------------------------

save(fao_eez_sf, file=file.path(dropboxdir,"rdata/fao_eez_sf.RData"))


# ----------------------------------------------------------------------------------------------------------
# testing with plaice TAC areas
# ----------------------------------------------------------------------------------------------------------

fao_eez_sf %>% filter(grepl("27.3.a", variable)) %>% View()
fao_sf %>% filter(grepl("27.3", variable)) %>% View()

t <- 
  data.frame(
    tac  = c("PLE/2A3AX4", "PLE/03AN."),
    area = c("27.2.a.2_EU;27.4.a;27.4.b;27.4.c", "27.3.a.20"),
    stringsAsFactors = FALSE
  ) %>% 
  
  # make into long list
  separate(area, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=variable, x1:x50) %>%
  drop_na() %>%
  select(-dummy) %>%
  
  # left_join the area geometries
  left_join(fao_eez_sf, by="variable") %>% 
  st_sf() %>% 
  
  # summarize by tac 
  group_by(tac) %>% 
  summarise() 

w <-
  st_intersection(world_sf, st_as_sfc(st_bbox(t)))

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


# -----------------------------------------------------------------------------------
# Testing the FAO area definitions in the Baltic Sea
# -----------------------------------------------------------------------------------

t <-
  fao_sf %>% 
  filter(grepl("27.3", variable)) %>% 
  mutate(F_LEVEL = factor(F_LEVEL, levels=c("SUBAREA","DIVISION","SUBDIVISION","SUBUNIT")))

w <-
  st_intersection(world_sf, st_as_sfc(st_bbox(t)))

t %>% 
  ggplot() +
  theme_publication() +
  theme(legend.position="none") +
  geom_sf(data=w, inherit.aes = FALSE, fill="cornsilk") +
  geom_sf(aes(fill=variable)) +
  geom_sf_text(aes(label = variable)) +
  labs(x="", y="") +
  facet_wrap(~F_LEVEL)

