# -----------------------------------------------------------------------------------------------
# Plot of changes in stock definitions.r
#
# 14/08/2017 added plot for assessment methods
# 29/11/2018 adapted from Mohn's code
# 03/12/2018 redone the logic of the selections; now with left_joins instead of lags; variables: recruitment, stocksize and fishing pressure
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours
library(lubridate)
library(scales)
library(directlabels)  # for printing labels at end of geom lines

library(devtools)
devtools::install_github("hrbrmstr/ggalt")
library(ggalt)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))


# ---------------------------------------------------------------------------------------------
# Scale differences: calculate average value for all the years in common. Calculate the number of jumps, 
# defined as a relative difference of more than xx%
# ---------------------------------------------------------------------------------------------

stockdef <-
  iAssess %>%
  
  # filter(grepl("had-34|had-n", stockkeylabelold)) %>%
  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns|hom-c|hom-w|had-34|had-n|hke-n", 
                stockkeylabelold)) %>%
  # filter(adviceonstock) %>% 
  
  mutate(
    stock = NA,
    stock = ifelse(grepl("ple-n",stockkeylabelold)                  , "ple (north sea)",stock),
    stock = ifelse(grepl("her-4",stockkeylabelold)                  , "her (north sea)",stock),
    stock = ifelse(grepl("mac-67|mac-nea|mac-west",stockkeylabelold), "mac (west)",stock),
    stock = ifelse(grepl("cod-34|cod-nsea",stockkeylabelold)        , "cod (north sea)",stock),
    stock = ifelse(grepl("whb-c|whb-n",stockkeylabelold)            , "whb (comb)",stock),
    stock = ifelse(grepl("sol-ns",stockkeylabelold)                 , "sol (north sea)",stock),
    stock = ifelse(grepl("hom-c|hom-w",stockkeylabelold)            , "hom (west)",stock),
    stock = ifelse(grepl("had-34|had-n",stockkeylabelold)            , "had (north sea)",stock),
    stock = ifelse(grepl("hke-n",stockkeylabelold)                  , "hke (north)",stock)
  ) %>% 
  
  filter(grepl("advice", purpose )) %>%
  # filter(grepl("hke", stock), assessmentyear == 2018) %>% View()

  filter(stocksizeunits == "tonnes", unitofrecruitment == "thousands", fishingpressureunits %in% c("per year", "year-1")) %>% 
  
  select(assessmentyear, stock, stockkeylabelold) %>% 
  distinct() %>% 
  mutate(stock = factor(stock),
         stock = factor(stock, levels=rev(levels(stock)))) %>% 
  group_by(stock, stockkeylabelold) %>% 
  summarize(miny = min(assessmentyear),
            maxy = max(assessmentyear))




stockdef %>% 
  ggplot(aes(x=assessmentyear, y=stock)) +
  theme_publication() +
  geom_dumbbell(aes(xend = maxy, x=miny, group=stock)) +
  geom_text(aes(x=miny, label=stockkeylabelold), hjust=0, nudge_y=0.15, size=3)


