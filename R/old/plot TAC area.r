# --------------------------------------------------------------------------------------------
# plot TAC area.r
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

# get dropbox directory
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# ----------------------------------------------------------------------------------------------------------
# set world in sf format
# ----------------------------------------------------------------------------------------------------------

world_sf <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE, 
                           regions = c("netherlands","belgium","France(?!:Corsica)",
                                       "ireland","united kingdom", "UK", "Denmark","Germany",
                                       "norway","iceland", "greenland", "faroe islands", "spain","portugal",
                                       "sweden", "finland","poland")))
# ggplot(data=world_sf) + geom_sf()


# ----------------------------------------------------------------------------------------------------------
# load the iAdvice data
# ----------------------------------------------------------------------------------------------------------

load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))
load(file.path(dropboxdir,"rdata/fao_iho.RData"))

# ----------------------------------------------------------------------------------------------------------
# generate the tac object: stock, assessmentyear and area(s) 
# ----------------------------------------------------------------------------------------------------------

tac <-
  iAdvice %>% 
  
  # filter(speciesfaocode == "ple") %>%
  # filter(grepl("27.2|27.3|27.4", tacarea)) %>% 
  
  filter(speciesfaocode == "her") %>%
  filter(grepl("27.1", tacarea)) %>% 
  
  filter(assessmentyear %in% 2010:2018) %>%
  distinct(fishstock = stockkeylabelold, stockkey, assessmentyear, tacarea) %>%
  mutate(tacarea = toupper(tacarea)) %>% 
  
  # make "area" into a long table
  separate(tacarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=variable, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3)) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao_eez_sf, by="variable") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(fishstock, variable, assessmentyear) %>%
  summarise()


# filter(stk, fishstock == "cod-347d", assessmentyear==2015) %>% View()
# filter(istock, stockkeylabelold == "cod-347d", assessmentyear==2015) %>% View()
# filter(stk, fishstock == "hom-nsea", assessmentyear==1987) %>% View()

# ----------------------------------------------------------------------------------------------------------
# generate plot
# ----------------------------------------------------------------------------------------------------------

w <-
  st_intersection(world_sf, st_as_sfc(st_bbox(tac)))

tac %>%
  
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
  geom_sf(aes(fill = factor(variable)), alpha=0.6, colour=NA) +
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~assessmentyear)
# facet_grid(fishstock~assessmentyear)


