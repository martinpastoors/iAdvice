library(tidyverse)
library(sf)
library(maps)

source("https://raw.githubusercontent.com/einarhjorleifsson/gisland/master/R/read_sf_ftp.R")


# read ICES/FAO areas
ices <- 
  read_sf_ftp("fao-areas_nocoastline") %>% 
  mutate(x = str_sub(name, 1, 2)) %>%
  filter(x == "27") %>%
  select(-x)
# plot(ices)

# clip map to include only ICES bound
bb = st_bbox(ices)

# actually, lets just focus on the central area, for now
bb[[1]] <- -30
bb[[2]] <- 45
bb[[3]] <- 20
bb[[4]] <- 70
box = st_as_sfc(bb)

# update ICES areas to box
ices <-
  st_intersection(ices, box) %>% 
  left_join(iceshape::faolink)

# generate country map
world1 <- 
  sf::st_as_sf(map('world', plot = FALSE, fill = TRUE, 
                   regions = c("netherlands","belgium","France(?!:Corsica)",
                               "ireland","united kingdom", "UK", "Denmark","Germany",
                               "norway","iceland", "greenland", "faroe islands", "spain","portugal",
                               "sweden", "finland","poland"))) %>% 
  st_intersection(., box)

# read the eez_iho interaction datasets
eez_iho <-
  read_sf("D:/Dropbox/ICES advice database/GIS/Intersect_EEZ_IHO_v3_2018/EEZ_IHO_v3.shp",
          stringsAsFactors = FALSE) %>%
  rename_all(., tolower) %>% 
  st_intersection(., box) %>% 
  
  # generate INT code for international waters
  mutate(iso_ter1 = ifelse(is.na(iso_ter1), "INT", iso_ter1)) %>% 
  
  # Convert EU countries to EU and make one area out of it
  mutate(iso_ter1 = ifelse(iso_ter1 %in% c("BEL", "DEU","DNK", "ESP", "FIN","FRA", "GGY", "GBR", "HRV", "IRL",
                                           "ITA", "JEY", "LTU","LVA", "NLD","POL","SVN", "SWE"), "EU", iso_ter1)) %>% 

  group_by(iso_ter1) %>%
  summarise()   

# plot eez_iho
eez_iho %>% ggplot() + geom_sf(aes(fill=iso_ter1), alpha=0.5, lwd=0.5) 
  
# Join the stuff
xy <-
  st_intersection(ices, eez_iho) %>%
  
  # generate a new variable, the ices name and then country iso
  mutate(variable = paste(unit, iso_ter1))

plot(xy["variable"])

sort(unique(xy$variable))

tacs <-
  data.frame(tac   = c("ANF/2AC4-C","ANF/56-14"), 
             areas = c("27.2.a EU~27.4.a EU", "27.6 EU~27.6 INT~27.5.b INT~27.12 INT~27.14 INT"), 
             stringsAsFactors = FALSE)

t <-
  tacs %>% 
  separate(areas, into=c(paste0("x",1:50)), sep = "~") %>% 
  gather(key=dummy, value=variable, -tac) %>%
  drop_na() %>%
  select(-dummy) %>%
  
  left_join(xy, by=c("variable")) %>% 
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  group_by(tac) %>% 
  summarise()

# plot tac
t %>% ggplot() + 
  geom_sf(data=ices, lwd=0.8, colour="blue") + 
  geom_sf(data=world1, fill="lightgray") +
  geom_sf(aes(fill=tac), alpha=0.5, lwd=0.0) 



# To get into the details (zoom and then hover over an area to see the variable name):
library(leaflet)
leaflet(xy) %>%
  addTiles() %>%
  addPolygons(label = ~variable)


ggplot() +
  geom_sf(data = ices, alpha=0.5, lwd=0.5) +
  geom_sf(data = eez_iho, aes(fill=eez), alpha = 0.5, lwd = 1) 

eez <-
  read_sf_ftp("eez") %>%
  # filter(pol_type == "200NM") %>% 
  select(territory1, iso_ter1, pol_type)

filter(eez, name == "Iceland") %>% ggplot() + geom_sf(aes(fill=name))



eez_iceland <-
  read_sf_ftp("eez_iceland2") %>%
  select(name)

  
iho <-
  read_sf("D:/Dropbox/ICES advice database/GIS/World_Seas_IHO_v3/World_Seas_IHO_v3.shp",
          stringsAsFactors = FALSE) %>%
  rename_all(., tolower)

plot(eez[,"territory1"])
st_write(eez, "/net/www/export/home/ftp/pub/reiknid/einar/shapes/eez.shp")
head(iho) %>% View()


ggplot() +
  geom_sf(data = ices, aes(fill = name), alpha=0.5) +
  geom_sf(data = eez, alpha = 0, lwd = 1) +
  theme(legend.position = "none")

xy <-
  st_intersection(ices, eez)

# plot all interactions
xy %>%
  ggplot() +
  geom_sf(data = xy, aes(fill = name))

# plot 5b only
xy %>%
  filter(grepl("27.5.b", name)) %>% 
  ggplot() +
  geom_sf(aes(fill = pol_type))

eez <-
  st_intersection(eez, box) %>%
  filter(pol_type == "200NM")

eez_iho <-
  st_intersection(eez_iho, box) 


ggplot() +
  geom_sf(data = ices, colour = "yellow", lwd = 2) +
  geom_sf(data = eez, colour = "black", alpha = 0.5)



