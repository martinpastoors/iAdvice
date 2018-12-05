# -----------------------------------------------------------------------------------------------
# Table of assessments per species
#
# 19/11/2018 first coding
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(cowplot)   # multiplots
library(RColorBrewer) # colours
library(lubridate)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(advicedir, "/rdata/iAssess.RData",sep=""))

# ---------------------------------------------------------------------------------------------
# table of number of assessments by year and species
# ---------------------------------------------------------------------------------------------

iAssess %>% 
  ungroup() %>% 
  mutate_at(c("stockkeylabel","stockkeylabelold"), funs(tolower)) %>% 
  mutate(speciesfaocode   = substr(stockkeylabel, 1, 3) ) %>% 
  filter(!grepl("nep", stockkeylabel)) %>% 
  filter(!grepl("^rj", stockkeylabel)) %>% 
  filter(assessmentyear >= 1986) %>% 
  
  distinct(speciesfaocode, stockkey, stockkeylabel, stockkeylabelold, 
           assessmentyear, purpose, published, source) %>% 
  group_by(speciesfaocode, assessmentyear) %>% 
  summarize(n = n()) %>% 
  left_join(iSpecies, by="speciesfaocode") %>% 
  mutate(species = paste(toupper(speciesfaocode), speciescommonname, sep="-")) %>% 
  
  dcast(species ~ assessmentyear, sum, value.var="n", margins = TRUE) %>% 
  # no.emphasis.table() %>% 
  # select(-1) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "left",
               missing=".",
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) 


# ---------------------------------------------------------------------------------------------
# table of number of assessments by year and species
# ---------------------------------------------------------------------------------------------

qcsexcel %>%
  
  ungroup() %>% 
  mutate_at(c("stockkeylabel","stockkeylabelold"), funs(tolower)) %>% 
  filter(!grepl("nep", stockkeylabel)) %>% 
  filter(!grepl("^rj", stockkeylabel)) %>% 
  
  filter(grepl("ple-n|her-47|mac-67|mac-nea|mac-west|cod-34|cod-nsea|whb-c|whb-n|sol-ns|hom-c|hom-w", 
               stockkeylabelold)) %>%
  
  distinct(speciesfaocode, stockkey, stockkeylabel, stockkeylabelold, 
           assessmentyear, assessmentdate, purpose, published, source) %>% 
  
  group_by(speciesfaocode, assessmentyear) %>% 
  summarize(n = n()) %>% 
  left_join(iSpecies, by="speciesfaocode") %>% 
  mutate(species = paste(toupper(speciesfaocode), speciescommonname, sep="-")) %>% 
  
  dcast(species ~ assessmentyear, sum, value.var="n", margins = TRUE) %>% 
  # no.emphasis.table() %>% 
  # select(-1) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "left",
               missing=".",
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) 


