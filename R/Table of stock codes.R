# -----------------------------------------------------------------------------------------------
# Table of stock codes
#
# 19/11/2018 first coding
# 22/03/2019 adapted for assessment models
# 27/03/2019 adapted for stock codes
# -----------------------------------------------------------------------------------------------

library(tidyverse) # for piping and easy coding
library(reshape2)  # reshaping data; e.g. dcast
library(ggthemes)  # for themes
library(pander)    # for print tables
library(readxl)    # read excel files
library(lubridate)

# Load utils code
source("../mptools/r/my_utils.r")

# Set working directory to dropbox folder
advicedir <- paste(get_dropbox(), "/iAdvice", sep="")

# Load dataset
load(file=paste(advicedir, "/rdata/iAdvice.RData",sep=""))

# ---------------------------------------------------------------------------------------------
# table of number of assessments by year and species
# ---------------------------------------------------------------------------------------------

iAdvice %>% 
  ungroup() %>% 
  mutate_at(c("stockkeylabel","stockkeylabelold"), funs(tolower)) %>% 
  mutate(speciesfaocode   = substr(stockkeylabel, 1, 3) ) %>%
  
  filter(speciesfaocode == "her") %>% 
  filter(assessmentyear <= 1996) %>% 
  filter(stockkey < 120000) %>% 
  
  distinct(stockkey, stockkeylabel, stockkeylabelold, assessmentyear, purpose, published, .keep_all = FALSE) %>% 
  write.csv(., file="her stockcodes.csv", row.names = FALSE)

