# --------------------------------------------------------------------------------------------
# plot iStock area.r
#
# plotting stock areas (over time)
# 
# 27/04/2018 cleaned up the code
# 28/11/2018 updated the code to work with new iAdvice database
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

world1 <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE, 
                           regions = c("netherlands","belgium","France(?!:Corsica)",
                                       "ireland","united kingdom", "UK", "Denmark","Germany",
                                       "norway","iceland", "greenland", "faroe islands", "spain","portugal",
                                       "sweden", "finland","poland")))
# ggplot(data=world1) + geom_sf()


# ----------------------------------------------------------------------------------------------------------
# load the iAdvice data
# ----------------------------------------------------------------------------------------------------------

load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))


# ----------------------------------------------------------------------------------------------------------
# generate the stk object: stock, assessmentyear and area(s) 
# ----------------------------------------------------------------------------------------------------------

stk <-
  iAdvice %>% 
  
  filter(speciesfaocode == "ple") %>%
  filter(grepl("27.3|27.4", stockarea)) %>% 
  filter(assessmentyear %in% 2010:2018) %>%
  
  # filter(adviceonstock == TRUE) %>% 
  
  # filter(fishstock %in% c("cod-iris","cod-7e-k", "cod-7f-g","cod-7f-h","cod-7e-h",
  #                         "cod-ech","cod-echw","cod-eche",
  #                         "cod-nsea","cod-skag","cod-kat","cod-347d", "cod-scow",
  #                         "cod.27.47d20","cod.27.7e-k", "cod.27.7a","cod.27.21","cod.27.6a")) %>%
  
  
  # filter(!unit %in% c("27.1","27.2","27.14", "27.14.b","27.14.a")) %>%
  
  distinct(fishstock = stockkeylabelold, stockkey, assessmentyear, stockarea) %>%

  # make "area" into a long table
  separate(stockarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=unit, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3)) %>% 
  
  # link to the link table to get name of geometry
  left_join(iceshape::faolink) %>%
  
  # link to geometry
  left_join(iceshape::fao) %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(fishstock, assessmentyear) %>%
  summarise()
  

# filter(stk, fishstock == "cod-347d", assessmentyear==2015) %>% View()
# filter(istock, stockkeylabelold == "cod-347d", assessmentyear==2015) %>% View()
# filter(stk, fishstock == "hom-nsea", assessmentyear==1987) %>% View()

# ----------------------------------------------------------------------------------------------------------
# generate plot
# ----------------------------------------------------------------------------------------------------------

stk %>%

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
  geom_sf(data=world1, inherit.aes = FALSE, fill="gray90") +
  geom_sf(aes(fill = factor(fishstock)), alpha=0.6) +
  coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~assessmentyear)
  # facet_grid(fishstock~assessmentyear)


# ----------------------------------------------------------------------------------------------------------
# generate the tab object: species, assessmentyear and area(s) 
# ----------------------------------------------------------------------------------------------------------

tac <-
  iAdvice %>% 
  
  filter(speciesfaocode == "ple") %>%
  filter(grepl("27.3|27.4", tacarea)) %>% 
  filter(assessmentyear %in% 2010:2018) %>%
  
  # filter(adviceonstock == TRUE) %>% 
  
  # filter(fishstock %in% c("cod-iris","cod-7e-k", "cod-7f-g","cod-7f-h","cod-7e-h",
  #                         "cod-ech","cod-echw","cod-eche",
  #                         "cod-nsea","cod-skag","cod-kat","cod-347d", "cod-scow",
  #                         "cod.27.47d20","cod.27.7e-k", "cod.27.7a","cod.27.21","cod.27.6a")) %>%
  
  
  # filter(!unit %in% c("27.1","27.2","27.14", "27.14.b","27.14.a")) %>%
  
  distinct(fishstock = stockkeylabelold, stockkey, assessmentyear, tacarea) %>%
  
  # make "area" into a long table
  separate(tacarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=unit, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3)) %>% 
  
  # link to the link table to get name of geometry
  left_join(iceshape::faolink) %>%
  
  # link to geometry
  left_join(iceshape::fao) %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(fishstock, assessmentyear) %>%
  summarise()


# filter(stk, fishstock == "cod-347d", assessmentyear==2015) %>% View()
# filter(istock, stockkeylabelold == "cod-347d", assessmentyear==2015) %>% View()
# filter(stk, fishstock == "hom-nsea", assessmentyear==1987) %>% View()

# ----------------------------------------------------------------------------------------------------------
# generate plot
# ----------------------------------------------------------------------------------------------------------

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
  geom_sf(data=world1, inherit.aes = FALSE, fill="gray90") +
  geom_sf(aes(fill = factor(fishstock)), alpha=0.6) +
  coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~assessmentyear)
# facet_grid(fishstock~assessmentyear)


tac_test <-
  iAdvice %>% 
  
  filter(speciesfaocode == "ple") %>%
  filter(grepl("27.3", tacarea)) %>% 
  filter(assessmentyear %in% 2010:2018) %>%
  
  mutate(tacarea = gsub("27\\.2\\.a","27.2.a_EU", tacarea)) %>% 

  distinct(fishstock = stockkeylabelold, stockkey, assessmentyear, tacarea) %>%
  
  # make "area" into a long table
  separate(tacarea, c(paste0("x",1:50)), sep = ";") %>%
  gather(key=dummy, value=unit, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3)) %>% 

  # separate into flag areas
  separate(unit, c("unit", "area"), sep = "_") %>%
  gather(key=dummy, value=unit, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3)) %>% 
  
  # link to the link table to get name of geometry
  left_join(iceshape::faolink) %>%
  
  # link to geometry
  left_join(iceshape::fao) %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(fishstock, assessmentyear) %>%
  summarise()


# filter(stk, fishstock == "cod-347d", assessmentyear==2015) %>% View()
# filter(istock, stockkeylabelold == "cod-347d", assessmentyear==2015) %>% View()
# filter(stk, fishstock == "hom-nsea", assessmentyear==1987) %>% View()

# ----------------------------------------------------------------------------------------------------------
# generate plot
# ----------------------------------------------------------------------------------------------------------

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
  geom_sf(data=world1, inherit.aes = FALSE, fill="gray90") +
  geom_sf(aes(fill = factor(fishstock)), alpha=0.6) +
  coord_sf(xlim = c(-20,25), ylim = c(38,70)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~assessmentyear)
# facet_grid(fishstock~assessmentyear)
