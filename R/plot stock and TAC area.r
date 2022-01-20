# --------------------------------------------------------------------------------------------
# plot stock and tac areas.r
#
# plotting stock and tac areas (over time); also overview of FAO and EEZ areas
# 
# 27/04/2018 cleaned up the code
# 28/11/2018 updated the code to work with new iAdvice database
# 19/12/2018 using new FAO_IHO dataset for international and EEZ waters
# 28/03/2019 updated after HAWG
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

source("../mptools/r/my utils.r")
# load("../prf/rdata/world.df.RData")

# source("r/iDatabases generator.r")

# set onedrive directory
onedrive <- file.path(Sys.getenv('USERPROFILE'), 'PFA/PFA team site - PRF') 

# Set dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))

# ----------------------------------------------------------------------------------------------------------
# count changes in areas
# ----------------------------------------------------------------------------------------------------------

iAdvice %>% 
  filter(assessmentyear >= 1980) %>% 
  filter(speciesfaocode=="cod") %>% 
  ungroup() %>% 
  distinct(stockkey, stockkeylabelold, assessmentyear, speciesfaocode, speciescommonname, stockarea) %>% 
  # group_by(assessmentyear, speciesfaocode, speciescommonname) %>% 
  # summarize(n=n()) %>% 
  View()



# ----------------------------------------------------------------------------------------------------------
# load spatial objects in sf format
# ----------------------------------------------------------------------------------------------------------

world.sf <- get(load(file.path(onedrive,"rdata/world.sf.RData"))) %>% 
  rownames_to_column()

world2.sf <- get(load(file.path(onedrive,"rdata/world2.sf.RData"))) %>% 
  rownames_to_column()

fao.sf <-
  get(load(file.path(onedrive,"rdata/fao.sf.RData"))) 

fao.eez.sf <-
  get(load(file.path(onedrive,"rdata/fao.eez.sf.RData"))) 


# ----------------------------------------------------------------------------------------------------------
# generate FAO overview
# ----------------------------------------------------------------------------------------------------------

fao.sf %>%
  filter(grepl("27.7.a", F_CODE)) %>%

  mutate(F_CODE = str_sub(F_CODE, start=4)) %>% 
  
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


# ----------------------------------------------------------------------------------------------------------
# generate FAO_EEZ overview
# ----------------------------------------------------------------------------------------------------------

fao.eez.sf %>%
  filter(grepl("27.7.a", F_CODE)) %>%
  data.frame() %>% 
  mutate(F_CODE = str_sub(F_CODE, start=4)) %>% 

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
# generate stock overviews
# ----------------------------------------------------------------------------------------------------------
# fao.eez.sf %>% filter(grepl("27.7.a", F_CODE)) %>% View()


stocks_toplot <- 
  iAdvice %>% 
  
  filter(grepl("her", stockkeylabel)) %>% 
  filter(assessmentyear >= 1980) %>%
  filter(stockkeylabelold %in% c("her-vian","her-irlw", "her-irls", "her-nirs","her-67bc", 
                                 "her-47d3","her-47d", "her-4a", "her-4b","her-4c7d","her-4ab")) %>% 
  # filter(stockkeylabelold %in% c("her-vian","her-irlw", "her-irls", "her-nirs")) %>% 
  mutate(stockkeylabelnew = ifelse(stockkeylabelold == "her-vian","her.27.6an", stockkeylabelnew),
         stockkeylabelnew = ifelse(stockkeylabelold == "her-irlw","her.27.irlw", stockkeylabelnew)) %>% 
  distinct(stockkeylabelold, stockkeylabel, stockkeylabelnew, stockarea, assessmentyear) %>% 
  
  # make "area" into a long table
  separate(stockarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, x1:x50) %>%
  filter(!is.na(F_CODE)) %>%
  dplyr::select(-dummy) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(assessmentyear, stockkeylabelold, stockkeylabel) %>%
  summarise()

# sort(unique(stocks_toplot$stockkeylabelold))

# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()

w <- st_intersection(world.sf, st_as_sfc(st_bbox(stocks_toplot)))
f <- st_intersection(fao.sf, st_as_sfc(st_bbox(stocks_toplot)))

stocks_toplot %>% 
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="bold", hjust=0.5, margin = margin(0.1,0,0,0, "mm"))) +
  
  # theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  # theme(axis.text        = element_blank(),
  #       axis.ticks       = element_blank()) +

  geom_sf(data=w, inherit.aes = FALSE, fill="gray90") +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  geom_sf(data=f, inherit.aes = FALSE, fill=NA, colour="gray", size=0.5) +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  geom_sf(aes(fill = factor(stockkeylabelold)), alpha=0.6, colour="black", size=1) +
  # geom_sf(aes(fill = factor(fishstock)), alpha=0.6, colour="black") +
  
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  # geom_sf_label(aes(label = stockkeylabel), alpha=0.9) +
  ggmisc::scale_fill_crayola() +
  labs(x="",y="") +
  facet_wrap(~assessmentyear)
# facet_grid(variable~assessmentyear)

# save to shape file
st_write(stocks_toplot, dsn = "herring_stocks.shp", layer = "herring.shp", driver = "ESRI Shapefile")


# ----------------------------------------------------------------------------------------------------------
# generate tac overviews
# ----------------------------------------------------------------------------------------------------------

# filter(fao.eez.sf, grepl("27.3", F_CODE)) %>% View()

  
# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()
tacs_all <- 
  readxl::read_excel(path="D:/Dropbox/iAdvice/Excel/ICES Scientific Advice database.xlsm", 
                     col_names=TRUE, col_types="text", sheet = "iTAC") %>% 
  lowcase()

tacs_toplot <- 
  iAdvice %>% 
  mutate(taccode = toupper(taccode)) %>% 
  dplyr::select(stockkeylabel, assessmentyear, tacyear, taccode) %>% 
  
  # readxl::read_excel(path="D:/Dropbox/iAdvice/Excel/ICES Scientific Advice database.xlsm", 
  #                    col_names=TRUE, col_types="text", sheet = "iTAC") %>% 
  # lowcase() %>% 
  # dplyr::select(-area, -x1) %>% 
  
  filter(grepl("HER/", taccode) ) %>% 
  # filter(grepl("HER/", taccode) & taccode != "HER/1/2-") %>% 
  # filter(grepl("HER/", taccode) & taccode != "HER/1/2-") %>% 
  # filter(taccode %in% c("HER/3BC+24", "HER/03A.", "HER/03A-BC", "HER/4AB.", "HER/4CXB7D", "HER/2A47DX")) %>% 
  # filter(taccode %in% c("HER/5B6ANB;HER/6AS7BC")) %>% 
  filter(tacyear == 2018) %>% 
  

  # make "tac" into a long table first
  separate(taccode, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=taccode, x1:x50) %>%
  drop_na() %>%
  dplyr::select(-dummy) %>% 
  
  # merge tacs_all again
  left_join(tacs_all, by="taccode") %>% 

  # now make "area" into a long table first
  separate(area, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=F_CODE, x1:x50) %>%
  drop_na() %>%
  dplyr::select(-dummy) %>% 
  arrange(taccode) %>% 
  
  # link to the link table to get name of geometry
  left_join(fao.eez.sf, by="F_CODE") %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(taccode) %>%
  summarise()

# fao.eez.sf %>% filter(grepl("27.3", F_CODE)) %>% View()


w <- st_intersection(world.sf, st_as_sfc(st_bbox(tacs_toplot)))
f <- st_intersection(fao.sf, st_as_sfc(st_bbox(tacs_toplot)))

tacs_toplot %>% 
  ggplot() +
  theme_publication() +
  theme(panel.spacing    = unit(0.1, "lines"),
        text             = element_text(size=8),
        axis.text        = element_blank(),
        axis.ticks       = element_blank(),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="bold", hjust=0.5, margin = margin(0.1,0,0,0, "mm"))) +
  
  theme(legend.title=element_blank()) +
  theme(legend.position="none") +
  
  # coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  # coord_sf(datum=st_crs(3035)) +
  
  geom_sf(data=w, inherit.aes = FALSE, fill="gray90") +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  
  geom_sf(data=f, inherit.aes = FALSE, fill=NA, colour="gray", size=0.5) +
  # geom_sf_text(data=w, aes(label=rowname), inherit.aes = FALSE, fill="gray90") +
  
  geom_sf(aes(fill = factor(taccode)), alpha=0.6, colour="black") +
  # geom_sf(aes(fill = factor(fishstock)), alpha=0.6, colour="black") +
  
           
  geom_sf_label(aes(label = taccode), alpha=0.9) +
  
  ggmisc::scale_fill_crayola() +
  facet_wrap(~taccode, ncol=8)
# facet_grid(variable~assessmentyear)


