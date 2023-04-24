# -----------------------------------------------------------------------------------------------
# analyse reference points
#
# 15/11/2018 first coding
# 03/04/2019 added reference points for specific stocks
# -----------------------------------------------------------------------------------------------

library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)      # reshaping data; e.g. dcast
library(pander)        # for print tables
library(readxl)        # read excel files
library(lubridate)     # dates
library(cowplot)       # multiplots
library(directlabels)  # for printing labels at end of geom lines

# Load utils code
source("../prf/r/my utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# -----------------------------------------------------------------------------------------
# Plot of number of reference points
# -----------------------------------------------------------------------------------------

iAdvice %>% 
  filter(assessmentyear >= 2000) %>%
  filter(!grepl("bench", purpose)) %>% 
  
  # add the reference points from sagrefpoints
  # bind_rows(sagrefpoints) %>% 
  # filter(assessmentyear >= 1997) %>% 
  
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


# -----------------------------------------------------------------------------------------
# Plot reference points by stock
# -----------------------------------------------------------------------------------------

mystock <- "hom-west"
mystock <- "her-47d3"
mystock <- "her-3a22"
mystock <- "ple-nsea"

iAdvice %>% 
  filter(stockkeylabelold == mystock) %>% 
  filter(assessmentyear >= 2000) %>%
  filter(!grepl("bench", purpose)) %>% 
  
  # add the reference points from sagrefpoints
  # bind_rows(sagrefpoints) %>% 
  # filter(assessmentyear >= 1997) %>% 
  
  select(stockkey, stockkeylabel, stockkeylabelold, stockkeylabelnew, assessmentyear, 
         flim, fpa, fmsy, blim, bpa, msybtrigger) %>% 
  gather(key=variable, value=value, flim:msybtrigger) %>% 
  mutate(type=case_when(
           variable %in% c("blim","bpa","msybtrigger") ~ "biomass", 
           variable %in% c("flim","fpa","fmsy")        ~ "f"),
         type2 = case_when(
           grepl("lim", variable)  ~ "lim",
           grepl("pa", variable)   ~ "pa",
           grepl("msy", variable)  ~ "msy"),
         type2 = factor(type2, levels=c("lim","pa","msy"))) %>%
  
  ggplot(aes(x=assessmentyear, y=value)) +
  theme_publication() +
  theme(legend.position = "none") +
  expand_limits(y=0) +
  geom_line(aes(colour=factor(variable))) +
  geom_point(aes(colour=factor(variable))) +
  labs(title = paste0("Reference points: ", mystock)) +
  facet_grid(type~type2, scales="free_y")

# -----------------------------------------------------------------------------------------
# Plot distance between Blim and Bpa
# -----------------------------------------------------------------------------------------
iAdvice %>% 
  filter(assessmentyear >= 2000) %>%
  filter(!grepl("bench", purpose)) %>% 
  # filter(speciesfaocode %in% c("her","mac","whb","hom","hke","cod","ple")) %>%   
  filter(speciesfaocode %in% c("her","mac","whb","hom")) %>%   
  filter(blim > 0) %>% 
  mutate(
    btrig_blim = msybtrigger/blim,
    bpa_blim   = bpa/blim,
    btrig_bpa  = msybtrigger/bpa
  ) %>%
  mutate(
    decade     = 10*floor(assessmentyear/10)
  ) %>% 
  dplyr::select(stockkeylabelold, species=speciesfaocode, assessmentyear, decade,
                blim, bpa, msybtrigger, btrig_blim, bpa_blim, btrig_bpa) %>% 
  
  ggplot(aes(x=bpa, y=btrig_bpa)) +
  # ggplot(aes(x=blim, y=btrig_blim)) +
  # ggplot(aes(x=blim, y=bpa_blim)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_point(aes(colour=species)) +
  expand_limits(y=0) +
  facet_wrap(~decade)


