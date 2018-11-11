# --------------------------------------------------------------------------------------------
# plot iStock area.r
#
# plotting stock areas (over time)
# 
# 27/04/2018 cleaned up the code
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
search()

source("../mptools/r/my_utils.r")
# load("../prf/rdata/world.df.RData")

# get dropbox directory
dropboxdir <- paste(get_dropbox(), "/ICES Assessment database", sep="")

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
# read the iStock data
# ----------------------------------------------------------------------------------------------------------
istock <-
  read_excel(path=paste(dropboxdir, "/data/istock.xlsx",sep=""),
             sheet="istock", col_names=TRUE, col_types="text") %>%
  lowcase() %>%
  mutate(stockarea = ifelse(substr(stockarea,1,2)=="21",toupper(stockarea), stockarea))


# ----------------------------------------------------------------------------------------------------------
# generate the stk object: stock, assessmentyear and area(s) 
# ----------------------------------------------------------------------------------------------------------
stk <-
  istock %>% 
  select(fishstock = stockkeylabelold, stockkey, assessmentyear, stockarea) %>%

  # make "area" into a long table
  separate(stockarea, c(paste0("x",1:50)), sep = "-") %>%
  gather(key=dummy, value=unit, -fishstock, -assessmentyear, -stockkey) %>%
  drop_na() %>%
  select(-dummy) %>%
  mutate(species = stringr::str_sub(fishstock, 1, 3))

# filter(stk, fishstock == "cod-347d", assessmentyear==2015) %>% View()
# filter(istock, stockkeylabelold == "cod-347d", assessmentyear==2015) %>% View()

# ----------------------------------------------------------------------------------------------------------
# filter the required stocks, combine with fao areas (in package iceshape) and generate plot
# ----------------------------------------------------------------------------------------------------------
stk %>%
  
  filter(species == "arg") %>%
  # filter(fishstock %in% c("cod-iris","cod-7e-k", "cod-7f-g","cod-7f-h","cod-7e-h",
  #                         "cod-ech","cod-echw","cod-eche",
  #                         "cod-nsea","cod-skag","cod-kat","cod-347d", "cod-scow",
  #                         "cod.27.47d20","cod.27.7e-k", "cod.27.7a","cod.27.21","cod.27.6a")) %>%
  filter(assessmentyear %in% 1986:2017) %>%
  # filter(!unit %in% c("27.1","27.2","27.14", "27.14.b","27.14.a")) %>%
  
  # link to the link table to get name of geometry
  left_join(iceshape::faolink) %>%
  
  # link to geometry
  left_join(iceshape::fao) %>%
  
  # because left_join above, we just have a data.frame. We need to specify that this is a data.frame with sf features
  st_sf() %>%
  
  # join areas together by stock and assessmentyear
  group_by(fishstock, assessmentyear) %>%
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
  geom_sf(aes(fill = factor(fishstock)), alpha=0.9) +
  coord_sf(xlim = c(-20,15), ylim = c(48,64)) +
  ggmisc::scale_fill_crayola() +
  facet_wrap(~assessmentyear, ncol=8)

