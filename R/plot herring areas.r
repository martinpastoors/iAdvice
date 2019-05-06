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

# source("r/iDatabases generator.r")

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))

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

iho.eez.sf <-
  get(load(file.path(onedrive,"rdata/iho.eez.sf.RData")))

# filter(fao.sf, grepl("27.6", F_CODE)) %>% View()
# filter(fao.eez.sf, grepl("27.6", F_CODE)) %>% View()

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


