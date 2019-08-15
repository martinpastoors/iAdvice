# -----------------------------------------------------------------------------------------------
# Compare iAdvice with ICES checks.r
#
# 15/08/2019 first coding
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

# Load iAdvice
iAssess <-
  get(load(file=paste(advicedir, "/rdata/iAssess.RData",sep=""))) %>% 
  ungroup() %>% 
  filter(!grepl("bench|withdrawn|initial|replaced", purpose)) %>% 
  filter(assessmentyear >= 2001) %>% 
  mutate(stockkeylabelold = ifelse(is.na(stockkeylabelold), stockkeylabel, stockkeylabelold)) %>% 
  # mutate(source = "iAdvice") %>% 
  distinct(stockkeylabel, assessmentyear, purpose, published, source) 

iAssess %>% filter(stockkeylabel == "ple-nsea", assessmentyear == 2003) %>% View()
qcsexcel %>% filter(stockkeylabel == "ple-nsea", assessmentyear == 2003) %>% View()


# Load ICES checks
ICESchecks <-
  read_excel(path=paste(advicedir, "/excel/Publish_round1.xlsx",sep="")) %>% 
  lowcase() %>% 
  rename(stockkeylabel = fishstock) %>% 
  dplyr::select(-year) %>% 
  mutate(source="ICEScheck") 

# range(ICESchecks$assessmentyear)

# Combine
comb <-
  bind_rows(ICESchecks, iAssess) %>% 
  group_by(stockkeylabel, assessmentyear) %>% 
  mutate(nobs = n()) %>%
  filter(!(nobs==1 & source %in% c("sag", "excel", "iadvice"))) %>% 
  arrange(desc(nobs), stockkeylabel, assessmentyear)






