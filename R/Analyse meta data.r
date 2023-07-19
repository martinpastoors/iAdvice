# -----------------------------------------------------------------------------------------------
# analyse meta data
#
# 15/11/2018 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse)     # combined package of dplyr, tidyr, ggplot, readr, purrr and tibble
library(reshape2)      # reshaping data; e.g. dcast
library(pander)        # for print tables
library(readxl)        # read excel files
library(lubridate)     # dates
library(cowplot)       # multiplots

# Load utils code
source("../prf/r/my utils.r")

# Set working directory to dropbox folder
advicedir  <- paste(get_dropbox(), "/iAdvice", sep="")

# load(file=paste(advicedir, "/rdata/iStock.RData",sep=""))
# load(file=paste(advicedir, "/rdata/qcsexcel.RData",sep=""))
load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# -----------------------------------------------------------------------------------------
# number of stockassessments per stock
# -----------------------------------------------------------------------------------------

p <-
  iAdvice %>% 
  filter(!grepl("benchmark|unofficial", purpose) ) %>% 
  filter(!grepl("elasmobranch", speciestype) ) %>% 
  filter(!grepl("grenadier|forkbeard|blue ling|roughy|blackspot|ling|skates", speciescommonname) ) %>% 
  mutate(speciescommonname = ifelse(grepl("megrim", speciescommonname), "megrims",speciescommonname)) %>% 
  mutate(speciescommonname = ifelse(grepl("anglerfish", speciescommonname), "anglerfishes",speciescommonname)) %>% 
  mutate(speciescommonname = ifelse(grepl("redfish", speciescommonname), "redfishes",speciescommonname)) %>% 
  mutate(assessmentmodel = substr(assessmentmodel2, 1, 40)) %>% 
  group_by(speciescommonname, assessmentmodel) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(y=assessmentmodel, x=speciescommonname)) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_point(aes(size=n))

png(filename=file.path("methods_species.png"),
    width=9, height=14, units="in", res=300, bg="transparent")
plot(p, fit = "fixed", just = "center")
dev.off()

p <-
  iAdvice %>% 
  filter(!grepl("benchmark|unofficial", purpose) ) %>% 
  filter(!grepl("elasmobranch", speciestype) ) %>% 
  filter(!grepl("grenadier|forkbeard|blue ling|roughy|blackspot|ling", speciescommonname) ) %>% 
  mutate(speciescommonname = ifelse(grepl("megrim", speciescommonname), "megrims",speciescommonname)) %>% 
  mutate(speciescommonname = ifelse(grepl("anglerfish", speciescommonname), "anglerfishes",speciescommonname)) %>% 
  mutate(speciescommonname = ifelse(grepl("redfish", speciescommonname), "redfishes",speciescommonname)) %>% 
  mutate(assessmentmodel = substr(assessmentmodel2, 1, 40)) %>% 
  mutate(assessmentmodel = ifelse(!(is.na(assessmentmodel) | assessmentmodel == "no assessment"), "assessment", assessmentmodel)) %>% 
  group_by(assessmentmodel, assessmentyear) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=assessmentyear, y=assessmentmodel)) +
  theme_publication() +
  # theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  geom_point(aes(size=n), colour="lightgray") +
  geom_text(aes(label=n)) +
  scale_size(range = c(0, 10))

png(filename=file.path("assessment_year.png"),
    width=18, height=5, units="in", res=300, bg="transparent")
plot(p, fit = "fixed", just = "center")
dev.off()

# -----------------------------------------------------------------------------------------
# number of benchmarks
# -----------------------------------------------------------------------------------------

iAdvice %>% 
  filter(grepl("benchmark", purpose) ) %>% 
  group_by(assessmentyear, purpose) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=assessmentyear, y=n, group=purpose)) +
  theme_publication() +
  geom_bar(aes(fill=factor(purpose)), stat="identity", position="stack")

# -----------------------------------------------------------------------------------------
# number of benchmarks per stock
# -----------------------------------------------------------------------------------------
iAdvice %>% filter(grepl("mac", stockkeylabelnew), grepl("bench", purpose)) %>% View()

iAdvice %>% 
  filter(grepl("benchmark", purpose) ) %>% 
  group_by(stockkeylabelnew) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n)) %>% 
  
  ggplot(aes(x=assessmentyear, y=n, group=purpose)) +
  theme_publication() +
  geom_bar(aes(fill=factor(purpose)), stat="identity", position="stack")
