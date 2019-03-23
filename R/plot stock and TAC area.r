# --------------------------------------------------------------------------------------------
# plot Stock and TAC area.r
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

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 


# ----------------------------------------------------------------------------------------------------------
# load the iAdvice data
# ----------------------------------------------------------------------------------------------------------

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

load(file.path(onedrive,"rdata/iho.eez.sf.RData"))

# ----------------------------------------------------------------------------------------------------------
# generate the stock object: stock, assessmentyear and area(s) 
# ----------------------------------------------------------------------------------------------------------

stock <-
  iAdvice %>% 
  
  # filter(speciesfaocode == "ple") %>%
  # filter(grepl("27.2|27.3|27.4", tacarea)) %>% 
  
  filter(speciesfaocode == "her") %>%
  filter(grepl("27.3", stockarea) & !grepl("27.4", stockarea)) %>% 
  filter(assessmentyear %in% 1985:1993) %>%
  # filter(assessmentyear == 1989) %>% 
  
  distinct(fishstock = stockkeylabelold, stockkey, assessmentyear, stockarea) %>%
  
  # make "area" into a long table
  separate(stockarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3)) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  arrange(fishstock, F_CODE) %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(fishstock, assessmentyear) %>%
  summarise()

# fao.eez.sf %>% filter(grepl("27.3.d.29", F_CODE)) %>% View()
# fao.eez.sf %>% filter(grepl("27.3.D.29", F_CODE)) %>% View()
# ices.sf %>% filter(grepl("27.3.D.29", F_CODE)) %>% View()

# ----------------------------------------------------------------------------------------------------------
# generate the tac object: stock, assessmentyear and area(s) 
# ----------------------------------------------------------------------------------------------------------

source("D:/GIT/iAdvice/R/iDatabases generator.r")

tac <-
  iAdvice %>% 
  
  # filter(speciesfaocode == "ple") %>%
  # filter(grepl("27.2|27.3|27.4", tacarea)) %>% 
  
  filter(speciesfaocode == "her") %>%
  filter(grepl("27.3", tacarea)) %>% 
  filter(assessmentyear %in% 1986:1993) %>%
  # filter(assessmentyear == 1989) %>% 
  
  distinct(taccode, assessmentyear, tacarea) %>%
  # mutate(tacarea = toupper(tacarea)) %>% 
  
  # make "area" into a long table
  separate(tacarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, -assessmentyear, -taccode) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = tolower(stringr::str_sub(taccode, 1, 3))) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(taccode, assessmentyear) %>%
  summarise()

# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()

# ----------------------------------------------------------------------------------------------------------
# generate plot
# ----------------------------------------------------------------------------------------------------------

w <- st_intersection(world.sf, st_as_sfc(st_bbox(stock)))
stock %>% rename(variable=fishstock) %>%

# w <- st_intersection(world.sf, st_as_sfc(st_bbox(tac)))
# tac %>% rename(variable=taccode) %>% 
  
  filter(assessmentyear == 1988) %>%
  
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
  
  geom_sf(aes(fill = factor(variable)), alpha=0.6, colour="black") +
  # geom_sf(aes(fill = factor(fishstock)), alpha=0.6, colour="black") +
  
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  # geom_sf_text(aes(label = fishstock)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~assessmentyear)
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


world.sf %>%
  filter(rowname %in% c("Russia", "Lithuania", "Poland")) %>% 
  ggplot() +
  theme(legend.position = "none") +
  geom_sf(aes(fill=rowname), alpha=0.2) +
  geom_sf_text(aes(label = rowname, colour=rowname)) +
  coord_sf(xlim=c(10,40), ylim=c(50, 65)) 

