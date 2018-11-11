# --------------------------------------------------------------------------------------------
# convert STECF Sframe to management areas.r
#
# 28/04/2018 cleaned up the code
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
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

# open Sframe (STECF) data
load(file = file.path(dropboxdir,"rdata", "STECF sframe.RData"))

head(stockdef)
head(fmz)


# ----------------------------------------------------------------------------------------------------------
# set world in sf format
# ----------------------------------------------------------------------------------------------------------

world1 <- sf::st_as_sf(
  map('world', plot = FALSE, fill = TRUE, 
               regions = c("netherlands","belgium","France(?!:Corsica)",
                           "ireland","united kingdom", "UK", "Denmark","Germany",
                           "norway","iceland", "greenland", "faroe islands", "spain","portugal",
                           "sweden", "finland","poland"))
)

# ggplot(data=world1) + geom_sf()


# ----------------------------------------------------------------------------------------------------------
# plot the required stocks, combine with fao areas (in package iceshape) and generate plot
# ----------------------------------------------------------------------------------------------------------

fmz %>% 
  
  filter(grepl("COD/1N2AB.", TAC_id)) %>% 
  # filter(grepl("COD/[A-Z0-9]", TAC_id)) %>% 
  mutate(unit = tolower(area)) %>% 
  arrange(TAC_id) %>% 

  # link to the link table to get name of geometry
  left_join(iceshape::faolink) %>%
  
  # link to geometry
  left_join(iceshape::fao) %>%
  
  # remove missing geometries
  filter(!is.na(geometry)) %>% 
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(TAC_id) %>%
  summarise() %>%

  # generate the plot
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        legend.title     = element_blank()) +
  
  theme(legend.title=element_blank()) +
  geom_sf(data=world1, inherit.aes = FALSE, fill="gray90") +
  geom_sf(aes(fill = factor(TAC_id)), alpha=0.4) +
  # coord_sf(xlim = c(-50,40), ylim = c(32,78)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~TAC_id)


# ----------------------------------------------------------------------------------------------------------
# Create TAC areas by TAC_id
# ----------------------------------------------------------------------------------------------------------

fmz %>% 
  group_by(TAC_id) %>% 
  filter(grepl("[A-Z]{3}/[A-Z0-9]", TAC_id)) %>% 
  mutate(area  = tolower(area)) %>% 
  arrange(TAC_id, area) %>% 
  summarize(area  = paste0(area, collapse ="-")) %>% 
  write.csv(., file=file.path(dropboxdir, "fmz.csv"), row.names=FALSE)


