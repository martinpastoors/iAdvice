# -----------------------------------------------------------------------------------------------
# Analyse changes in pelagic stocks
#
# 07/01/2018 first coding
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

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(dropboxdir, "/rdata/iAssess.RData",sep=""))
load(file=paste(dropboxdir, "/rdata/iAdvice.RData",sep=""))

# create subset
d <-
  iAdvice %>% 
  
  filter(grepl("her-47|mac-67|mac-nea|whb-c|whb-n|hom-c|hom-w|her-3a22|her-noss", 
               stockkeylabelold)) %>%
  filter(!grepl("benchmark", purpose)) %>% 
  filter(assessmentyear >= 2008) %>% 
  ungroup()

# plot F reference points
d %>% 
  dplyr::select(stockkeylabelold, assessmentyear, fpa, fmsy) %>% 
  gather(key=var, value=value, fpa:fmsy) %>% 
  
  ggplot(aes(x=assessmentyear)) +
  theme_publication() +
  theme(legend.title = element_blank()) +
  
  geom_line(aes(y=value, group=var, colour=factor(var))) +
  geom_point(aes(y=value, group=var, colour=factor(var))) +
  
  scale_x_continuous(breaks=pretty_breaks()) +
  expand_limits(y=0) + 
  labs(y="F") +
  scale_color_manual(values = c("blue", "green")) +
  facet_wrap(~stockkeylabelold, scales="free_y")

# plot B reference points
d %>% 
  dplyr::select(stockkeylabelold, assessmentyear, blim, msybtrigger) %>% 
  gather(key=var, value=value, blim:msybtrigger) %>% 
  
  ggplot(aes(x=assessmentyear)) +
  theme_publication() +
  theme(legend.title = element_blank()) +
  
  geom_line(aes(y=value, group=var, colour=factor(var))) +
  geom_point(aes(y=value, group=var, colour=factor(var))) +
  
  scale_x_continuous(breaks=pretty_breaks()) +
  expand_limits(y=0) + 
  labs(y="SSB") +
  scale_color_manual(values = c("red", "green")) +
  facet_wrap(~stockkeylabelold, scales="free_y")

# plot of advice
d %>% 
  ungroup() %>% 
  dplyr::select(stockkeylabelold, tacyear, adviceonstock, advisedlandingsmax) %>% 
  ggplot(aes(x=tacyear)) +
  theme_publication() +
  theme(legend.title = element_blank()) +
  
  geom_point(aes(y       = advisedlandingsmax, 
                 colour  = ifelse(adviceonstock==TRUE,'advice','replaced'))) +
  geom_line(aes(y        = advisedlandingsmax, 
                colour   = ifelse(adviceonstock==TRUE,'advice','replaced'),
                linetype = ifelse(adviceonstock==TRUE,'advice',"replaced") )) +
  
  scale_x_continuous(breaks=pretty_breaks()) +
  expand_limits(y=0) + 
  labs(y="Advice Landings Max (tonnes)") +
  scale_color_manual(values = c("red", "blue")) +
  scale_linetype_manual(values = c("solid", "blank")) +
  facet_wrap(~stockkeylabelold, scales="free_y")

# plot of adviceyear stock size relative to 2018 assessment
e <-
  iAssess %>% 
  filter(stockkeylabelold %in% unique(d$stockkeylabelold)) %>% 
  filter(assessmentyear == 2018) %>% 
  filter(year >= 2008) %>% 
  filter(year <= assessmentyear) %>% 
  filter(adviceonstock == TRUE)

d %>% 
  ungroup() %>% 
  dplyr::select(stockkeylabelold, assessmentyear, adviceonstock, ssbay) %>% 
  rename(year = assessmentyear) %>% 
  
  ggplot(aes(x=year)) +
  theme_publication() +
  theme(legend.title = element_blank()) +
  
  geom_point(aes(y=ssbay,
                 colour   = ifelse(adviceonstock==TRUE,'advice','replaced'))) +
  geom_line(data=e,
            aes(y = stocksize )) +
  
  scale_x_continuous(breaks=pretty_breaks()) +
  expand_limits(y=0) + 
  labs(y="SSB") +
  scale_color_manual(values = c("red", "blue")) +
  facet_wrap(~stockkeylabelold, scales="free_y")
