# ===========================================================================
# iAdvice refpoints.r
#
# 10/10/2018 initial coding
# ===========================================================================

# rm(list=ls())

library(readxl)
library(tidyverse)
library(stringr)
library(pander)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Source the utils file
source("../prf/R/my utils.r")

# set dropboxdirs
dropboxdir <- paste(get_dropbox(), "/iAdvice", sep="")


load(file=paste(dropboxdir, "/rdata/iStockkey.RData", sep=""))
load(file=paste(dropboxdir, "/rdata/iRename.RData", sep=""))

load(file=paste(dropboxdir, "/rdata/iAdvice.RData", sep=""))

compare <-
  iAdvice %>% 
  filter(adviceonstock == TRUE) %>% 
  dplyr::select(year=tacyear, stockkeylabelold, stockarea, advisedlandingsmax, tal, tac, landings, catches) %>% 
  rename(advice=advisedlandingsmax) %>% 
  group_by(year, stockkeylabelold, stockarea) %>% 
  filter(n() == 1) %>% 
  # filter(stockkeylabelold %in% c("her-47d3")) %>% 
  # filter(stockkeylabelold %in% c("her-47d3", "mac-nea","whb-comb", "her-noss")) %>% 
  # filter(stockkeylabelold %in% c("her-47d3", "mac-nea","whb-comb", "her-noss")) %>% 
  filter(year >= 2000) %>% 
  gather(key=variable, value=value, advice:catches) 

# compare %>% filter(stockkeylabelold=="ane-bisc", year==2014) %>% View()
# compare %>% filter(stockkeylabelold=="ane-bisc") %>% group_by(year, stockkeylabelold, stockarea, variable) %>% summarise(n=n()) %>% filter(n>1)



# ---------------------------------------------------------------------------
# overview of %change in reference points
# ---------------------------------------------------------------------------


# plot of comparison of advice, tac, landings and catch
compare %>% 
  ungroup() %>% 
  
  filter(variable %in% c("advice", "catches")) %>% 
  filter(!is.na(value)) %>% 
  mutate(variable = factor(variable, levels=c("advice","catches"))) %>% 
  # mutate(variable = factor(variable, levels=c("advice", "tac", "landings", "catches"))) %>% 
  
  ggplot(aes(x=year, y=value, group=variable)) +
  theme_publication() +
  geom_path(aes(colour=factor(variable)), size=1) +
  geom_point(aes(colour=factor(variable)), size=2) +
  # scale_size_manual    (values=c(advice=1.5 ,tac = 1.5, landings = 1.0, catches = 1.0)) +
  expand_limits(y = 0) +
  facet_wrap(~stockkeylabelold, scales="free_y") 

# plot of difference between tac and advice
t <-
  compare %>% 
  filter(variable %in% c("advice","tac")) %>% 
  pivot_wider(names_from=variable, values_from=value) %>% 
  mutate(deviance = as.integer(100*(tac/advice-1)) ) %>% 
  filter(!is.na(deviance)) %>% 

  ggplot(aes(x=year, y=deviance)) +
  theme_publication() +
  geom_hline(aes(yintercept=0), colour="black") +
  geom_line(colour="blue", size=1.5) +
  facet_wrap(~stockkeylabelold) 


glimpse(t)

# plot of difference between catch and advice
compare %>% 
  filter(variable %in% c("advice","catches")) %>% 
  filter(!is.na(value)) %>% 
  
  spread(key=variable, value=value) %>% 
  mutate(deviance = as.integer(100*(catches/advice-1)) ) %>% 
  filter(!is.na(deviance)) %>%
  
  ungroup() %>% 
  mutate(meandev = mean(deviance)) %>% 

  ggplot(aes(x=year, y=deviance)) +
  theme_publication() +
  geom_hline(aes(yintercept=0), colour="black") +
  geom_line(colour="blue", size=1.5) +
  facet_wrap(~stockkeylabelold) 
