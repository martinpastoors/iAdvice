# -----------------------------------------------------------------------------------------------
# Analyse all assessments for all stocks
#
# 20/11/2018 first coding
# -----------------------------------------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)

# Load utils code
source("../mptools/r/my_utils.r")

# Set dropbox folder
advicedir <- paste(get_dropbox(), "/iAdvice", sep="")

# load the data
load(file=paste(advicedir,"/rdata/iAssess.RData", sep=""))

# ---------------------------------------------------------------------------------------------
# Historic retros: plot stock data over different assessment years 
# ---------------------------------------------------------------------------------------------

iAssess %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  mutate(decade = as.character(10*floor(assessmentyear/10))) %>% 
  
  # filter(speciesfaocode == "ang") %>% 
  # filter(substr(speciesfaocode,1,1) == "a") %>% 
  # filter(stockkeylabelold == "ang-ivvi") %>% 
  
  filter(purpose == "advice") %>% 
  filter(!is.na(stockkeylabelold)) %>%
  filter(stocksizeunits == "tonnes") %>% 
  filter(stocksizedescription == "ssb") %>% 
  filter(!is.na(stocksize)) %>% 
  filter(year > 1980, year <= assessmentyear) %>% 
  
  ungroup() %>% 
  data.frame() %>% 

  ggplot(aes(year,stocksize, group=assessmentyear)) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(0.1, "mm"),
        panel.spacing.y = unit(0.5, "mm"),
        strip.background = element_blank(),
        strip.text       = element_text(size=8, face="plain", hjust=0, margin = margin(0,0,0,0, "mm"))
        # legend.position = "null"
        ) +
  geom_line(aes(colour = decade) ) +
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "SSB")  +
  # facet_wrap(~ assessmentyear, scales="free_y")
  facet_wrap(~ stockkeylabelold, scales="free_y")



# Now for fishing pressure
iAssess %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  mutate(decade = as.character(10*floor(assessmentyear/10))) %>% 
  
  filter(purpose == "advice") %>% 
  filter(!is.na(stockkeylabelold)) %>%
  filter(fishingpressureunits %in% c("year-1","per year")) %>% 
  # filter(speciesfaocode %in% c("cod","her","ple","sol","whb","hom","had","whg", "sai", "san") ) %>% 
  filter(!is.na(fishingpressure)) %>% 
  # filter(assessmentscale != "relative") %>% 
  
  filter(year > 1980, year <= assessmentyear) %>% 
  ungroup() %>% 
  data.frame() %>% 
  
  ggplot(aes(year,fishingpressure, group=assessmentyear)) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(0.1, "mm"),
        panel.spacing.y = unit(1, "mm")
        # strip.background = element_blank(),
        # legend.position = "null"
  ) +
  geom_line(aes(colour = decade) ) +
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "Fishing mortality")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")


# And now for recruitment
iAssess %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  mutate(decade = as.character(10*floor(assessmentyear/10))) %>% 
  
  filter(purpose == "advice") %>% 
  filter(!is.na(stockkeylabelold)) %>%
  filter(unitofrecruitment %in% c("thousands")) %>% 
  filter(!is.na(recruitment)) %>% 

  filter(year > 1980, year <= assessmentyear) %>% 
  ungroup() %>% 
  data.frame() %>% 
  
  ggplot(aes(year,recruitment, group=assessmentyear)) +
  theme_publication() +
  theme(legend.title=element_blank(),
        axis.text.x  = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.spacing.x = unit(0.1, "mm"),
        panel.spacing.y = unit(1, "mm")
        # strip.background = element_blank(),
        # legend.position = "null"
  ) +
  geom_line(aes(colour = decade) ) +
  expand_limits(y = 0) +
  # xlim(2005,2020) +
  labs(x = NULL, y = NULL , title = "Recruitment")  +
  facet_wrap(~ stockkeylabelold, scales="free_y")
