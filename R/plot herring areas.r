# --------------------------------------------------------------------------------------------
# plot herring areas.r
#
# plotting stock areas (over time)
# 
# 27/04/2018 cleaned up the code
# 28/11/2018 updated the code to work with new iAdvice database
# 19/12/2018 using new FAO_IHO dataset for international and EEZ waters
# --------------------------------------------------------------------------------------------

# devtools::install_github("fishvice/tidyices", dependencies = FALSE)

# devtools::install_github("einarhjorleifsson/iceshape", dependencies = FALSE)
# devtools::install_github("einarhjorleifsson/ggmisc", dependencies = FALSE)
library(iceshape)   # ls("package:iceshape")

library(sf)
library(tidyverse)
library(readxl)
library(maps)
library(ggplot2)
library(viridis)

source("../mptools/r/my_utils.r")
# load("../prf/rdata/world.df.RData")

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# ----------------------------------------------------------------------------------------------------------
# load spatial objects in sf format
# ----------------------------------------------------------------------------------------------------------

world.sf <- get(load(file.path(onedrive,"rdata/world.sf.RData"))) %>% 
  rownames_to_column()

world2.sf <- get(load(file.path(onedrive,"rdata/world2.sf.RData"))) %>% 
  rownames_to_column()

fao.eez.sf <-
  get(load(file.path(onedrive,"rdata/fao.eez.sf.RData"))) 

fao.sf <-
  get(load(file.path(onedrive,"rdata/fao.sf.RData"))) 

load(file.path(onedrive,"rdata/iho.eez.sf.RData"))

# ----------------------------------------------------------------------------------------------------------
# load herring data
# ----------------------------------------------------------------------------------------------------------

herring_areas <- 
  readxl::read_excel(path="D:/HAWG/Herring areas.xlsx", col_names=TRUE, col_types="text") %>% 
  # make "area" into a long table
  separate(Areas, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, x1:x50) %>%
  drop_na() %>%
  dplyr::select(-dummy) %>% 

  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(Fleet, Type) %>%
  summarise()

# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()



# ----------------------------------------------------------------------------------------------------------
# generate plot
# ----------------------------------------------------------------------------------------------------------

w <- st_intersection(world.sf, st_as_sfc(st_bbox(herring_areas)))

herring_areas %>% 
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="bold", hjust=0.5, margin = margin(0.1,0,0,0, "mm")),
        legend.title     = element_blank()) +
  
  theme(legend.title=element_blank()) +
  
  geom_sf(data=w, inherit.aes = FALSE, fill="gray90") +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  geom_sf(aes(fill = factor(Fleet)), alpha=0.6, colour="black") +
  # geom_sf(aes(fill = factor(fishstock)), alpha=0.6, colour="black") +
  
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  # geom_sf_text(aes(label = fishstock)) +
  ggmisc::scale_fill_crayola() +
  facet_grid(Type~Fleet)
  # facet_grid(variable~assessmentyear)


# ----------------------------------------------------------------------------------------------------------
# generate FAO overview
# ----------------------------------------------------------------------------------------------------------

fao.sf %>%
  filter(grepl("27.3", F_CODE)) %>%
  mutate(F_CODE = str_sub(F_CODE, start=6)) %>% 
  
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="bold", hjust=0.5, margin = margin(0.1,0,0,0, "mm")),
        legend.title     = element_blank()) +
  theme(legend.position = "none") +
  geom_sf(aes(fill=F_CODE)) +
  geom_sf_text(aes(label = F_CODE)) +
  facet_wrap(~F_LEVEL)


# world.sf %>%
#   filter(rowname %in% c("Russia", "Lithuania", "Poland")) %>% 
#   ggplot() +
#   theme(legend.position = "none") +
#   geom_sf(aes(fill=rowname), alpha=0.2) +
#   geom_sf_text(aes(label = rowname, colour=rowname)) +
#   coord_sf(xlim=c(10,40), ylim=c(50, 65)) 

# ----------------------------------------------------------------------------------------------------------
# generate herring tac overview
# ----------------------------------------------------------------------------------------------------------

herring_tacs <- 
  readxl::read_excel(path="D:/Dropbox/iAdvice/Excel/ICES Scientific Advice database.xlsm", 
                     col_names=TRUE, col_types="text", sheet = "iTAC") %>% 
  lowcase() %>% 
  filter(tacid %in% c("HER/3BC+24", "HER/03A.", "HER/03A-BC", "HER/4AB.", "HER/4CXB7D", "HER/2A47DX")) %>% 
  
  # make "area" into a long table
  separate(area, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, x1:x50) %>%
  drop_na() %>%
  dplyr::select(-dummy) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(tacid) %>%
  summarise()

# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()

w <- st_intersection(world.sf, st_as_sfc(st_bbox(herring_tacs)))
f <- st_intersection(fao.sf, st_as_sfc(st_bbox(herring_tacs)))

herring_tacs %>% 
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="bold", hjust=0.5, margin = margin(0.1,0,0,0, "mm")),
        legend.title     = element_blank()) +
  
  theme(legend.title=element_blank()) +
  
  geom_sf(data=w, inherit.aes = FALSE, fill="gray90") +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  
  geom_sf(aes(fill = factor(tacid)), alpha=0.6, colour="black") +
  # geom_sf(aes(fill = factor(fishstock)), alpha=0.6, colour="black") +

  geom_sf(data=f, inherit.aes = FALSE, fill=NA, colour="black", size=0.5) +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  # geom_sf_text(aes(label = fishstock)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~tacid)
# facet_grid(variable~assessmentyear)


# ----------------------------------------------------------------------------------------------------------
# generate herring stock overview
# ----------------------------------------------------------------------------------------------------------

herring_stocks <- 
  iAdvice %>% 
  
  filter(stockkeylabelold %in% c("her-47d3", "her-3a22"),
         assessmentyear == 2018) %>%
  distinct(stockkeylabelold, stockkeylabel, stockarea) %>% 
  
  # make "area" into a long table
  separate(stockarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, x1:x50) %>%
  drop_na() %>%
  dplyr::select(-dummy) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(stockkeylabelold, stockkeylabel) %>%
  summarise()

# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()

w <- st_intersection(world.sf, st_as_sfc(st_bbox(herring_stocks)))
f <- st_intersection(fao.sf, st_as_sfc(st_bbox(herring_stocks)))

herring_stocks %>% 
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="bold", hjust=0.5, margin = margin(0.1,0,0,0, "mm")),
        legend.title     = element_blank()) +
  
  theme(legend.title=element_blank()) +
  
  geom_sf(data=w, inherit.aes = FALSE, fill="gray90") +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  
  geom_sf(aes(fill = factor(stockkeylabel)), alpha=0.6, colour="black") +
  # geom_sf(aes(fill = factor(fishstock)), alpha=0.6, colour="black") +
  
  geom_sf(data=f, inherit.aes = FALSE, fill=NA, colour="black", size=0.5) +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  # geom_sf_text(aes(label = fishstock)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~stockkeylabel)
# facet_grid(variable~assessmentyear)