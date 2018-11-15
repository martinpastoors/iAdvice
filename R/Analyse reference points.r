# -----------------------------------------------------------------------------------------------
# analyse benchmarks
#
# 15/11/2018 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)      # reshaping data; e.g. dcast
library(pander)        # for print tables
library(readxl)        # read excel files
library(lubridate)     # dates
library(cowplot)       # multiplots
library(directlabels)  # for printing labels at end of geom lines

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
# load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# -----------------------------------------------------------------------------------------
# Plot of number of reference points
# -----------------------------------------------------------------------------------------

iAdvice %>% 
  filter(assessmentyear <= 2000) %>%
  filter(!grepl("bench", purpose)) %>% 
  
  # add the reference points from sagrefpoints
  bind_rows(sagrefpoints) %>% 
  
  group_by(assessmentyear) %>% 
  summarize(flim = sum(!is.na(flim)),
            fpa  = sum(!is.na(fpa)),
            fmsy = sum(!is.na(fmsy)),
            blim = sum(!is.na(blim)),
            bpa  = sum(!is.na(bpa)),
            msybtrigger = sum(!is.na(msybtrigger)) ) %>% 
  gather(key=variable, value=value, flim:msybtrigger) %>% 
  mutate(type = ifelse(substr(variable,1,1) == "f", "fishing mortality", "biomass")) %>% 
  mutate(type2= ifelse(grepl("lim", variable), "lim", NA),
         type2= ifelse(grepl("pa" , variable), "pa", type2),
         type2= ifelse(grepl("msy", variable), "msy",type2)) %>% 
  
  ggplot(aes(x=assessmentyear, y=value)) +
  theme_publication() +
  geom_line(aes(colour=factor(type2))) +
  facet_wrap(~type)

# Calculate number of changes in reference points
iAdvice %>% 
  filter(assessmentyear <= 2000) %>%
  filter(!grepl("bench", purpose)) %>% 
  
  # add the reference points from sagrefpoints
  bind_rows(sagrefpoints) %>% 
  
  dplyr::select(stockkey, stockkeylabel, assessmentyear, flim, fpa, fmsy, blim, bpa, msybtrigger) %>% 
  gather(key=variable, value=value, flim:msybtrigger) %>% 
  filter(!is.na(value)) %>% 
  group_by(stockkey, stockkeylabel, variable) %>% 
  arrange(stockkey, stockkeylabel, variable, assessmentyear) %>% 
  mutate(valuebefore = lag(value)) %>% 
  
  # only when valuebefore is not.na
  filter(!is.na(valuebefore)) %>% 
  mutate(change = ifelse(value != valuebefore, 1, 0)) %>% 
  
  # sum changes by year
  group_by(variable, assessmentyear) %>% 
  summarize(change = sum(change, na.rm=TRUE)) %>% 
  
  # add classifiers
  mutate(type = ifelse(substr(variable,1,1) == "f", "fishing mortality", "biomass")) %>% 
  mutate(type2= ifelse(grepl("lim", variable), "lim", NA),
         type2= ifelse(grepl("pa" , variable), "pa", type2),
         type2= ifelse(grepl("msy", variable), "msy",type2)) %>% 
  
  ggplot(aes(x=assessmentyear, y=change)) +
  theme_publication() +
  geom_line(aes(colour=factor(type2))) +
  facet_wrap(~type)


