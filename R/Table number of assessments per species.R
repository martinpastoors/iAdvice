# -----------------------------------------------------------------------------------------------
# Analyse assessments per species
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
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  filter(assessmentyear >= 1980) %>% 
  
  distinct(speciesfaocode, stockkey, stockkeylabel, stockkeylabelold, assessmentyear, purpose, published, source) %>% 
  group_by(speciesfaocode, assessmentyear) %>% 
  summarize(n = n()) %>% 
  left_join(iSpecies, by="speciesfaocode") %>% 
  mutate(species = paste(toupper(speciesfaocode), speciescommonname, sep="-")) %>% 
  
  dcast(species ~ assessmentyear, sum, value.var="n", margins = TRUE) %>% 
  mutate_all(funs(na_if(., 0))) %>% 
  # no.emphasis.table() %>% 
  # select(-1) %>% 
  pandoc.table(.,
               style = "simple",
               split.tables=400, 
               justify = "left",
               missing=".",
               round=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)) 


iAssess %>% distinct(assessmentyear) %>% min()
