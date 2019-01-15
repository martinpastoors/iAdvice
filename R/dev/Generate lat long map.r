# --------------------------------------------------------------------------------------------
# generate lat long map.r
#
# 27/12/2018 first coding
# --------------------------------------------------------------------------------------------

library(sf)
library(tidyverse)
library(readxl)
library(maps)
library(ggplot2)
library(rgeos)

source("../mptools/r/my_utils.r")

# get dropbox directory
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# ----------------------------------------------------------------------------------------------------------
# set world in sf format
# ----------------------------------------------------------------------------------------------------------
world1 <- 
  map('world', plot=FALSE) %>% 
  summary()
filter(grepl("spain",tolower(names)))
View(world1)

nams <- 
  map("world", namesonly=TRUE, plot=FALSE) 
nams[grep("island", tolower(nams))]

world1 <- 
  map('world',
      plot = FALSE, 
      fill = TRUE, 
      regions = c("Netherlands","belgium","France(?!:Corsica)","ireland","united kingdom", "UK", 
                  "Denmark","Germany","norway","iceland", "greenland", "faroe islands", "spain",
                  "portugal","sweden", "finland","poland", 
                  "canary islands:gran canaria", "canary islands:tenerife", "canary islands:la palma",
                  "morocco", "mauritania", "western sahara", "chile", "peru", "chile:easter island") ) %>% 
  map_data()  
  # sample_n(5000, replace=FALSE) %>% 
  # arrange(group, order)


ggplot(data=world1, aes(x=long, y=lat, group=group)) +
  theme_publication() +
  geom_path()

w1 <-
  world1 %>% 
  group_by(group, region, subregion) %>%
  filter(n() > 10) %>% 
  distinct(group, region, subregion)

w1a <-
  w1 %>% 
  left_join(world1, by=c("group","region","subregion")) %>% 
  ungroup() %>% 
  sample_n(7000, replace=FALSE) 

w1b <-
  world1 %>% 
  filter(!(group %in% w1$group & region %in% w1$region & subregion %in% w1$subregion))

w2 <-
  bind_rows(w1a, w1b) %>% 
  arrange(group, order) %>% 
  group_by(group, region, subregion) %>% 
  do(add_row(., .before=0))

write.csv(w2, file="w2.csv", row.names=FALSE)

w2 %>% 
  ggplot(aes(x=long, y=lat, group=group)) +
  theme_publication() +
  geom_path()

iris %>% group_by(Species) %>% do(add_row(., .before=0))

